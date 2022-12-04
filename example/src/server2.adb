
-- Besides this is a multitask and reasonable complete example, from here,
-- you can do more, as: (1) more que one listen socket and polls, (2) more
-- length polls, (3) simultaneous listen event_types,
-- (4) the use of others types beyond String:
-- (4.1) from built-in types and records to
-- (4.2) wide class and tagged types
-- (4.3) and with a more fine treatment, all records, tagged types included,
-- can be endian proof.
-- (5) etc. ^^
-- It is yet up to you to create a yet better real world champion software
-- with Adare_net and we believe you can do it!! ^^

-- Info about this software:
-- Server udp example. work in pair with client2
-- It choose the first working host address.
-- The working address can be ipv6 or ipv4 automatically. The first working one
-- will be picked.

with Ada.Text_IO;
use Ada;

with Ada.Task_Identification;
with Ada.Streams;

with Ada.Command_Line;
with Ada.Strings.Unbounded;

with adare_net.sockets.utils;
with adare_net.sockets.polls;
use adare_net.sockets;

with adare_net_exceptions;
use adare_net_exceptions;

with socket_types;
use  socket_types;

with adare_net_init;
use  adare_net_init;

with Interfaces.C;
use Interfaces, Interfaces.C;

procedure server2
is
begin
  start_adare_net;

  b0 :
  declare
    host_address  : aliased addresses_list := init_addresses
      (ip_or_host =>  "", -- host addresses
      port        =>  "25000",
      ai_socktype =>  udp,
      ai_family   =>  any -- choose ipv4 and ipv6
      );

    ok            : Boolean :=  False;
    host_socket   : aliased socket;
  begin
    if host_address'Length < 1 then
      Text_IO.Put_Line (" Failed to discover addresses in this host");
      Text_IO.Put_Line (" Quitting.");
      goto end_app_label1;
    end if;

    Text_IO.Put_Line (" Addresses Discovered in this host:");

    utils.show_address_and_port (host_address'Access);

    if not init_socket (host_socket, host_address'Access) then
      Text_IO.New_Line;
      Text_IO.Put_Line (" Failed to initialize socket: ");
      Text_IO.Put_Line (" " & string_error);

      goto end_app_label1;
    end if;

    reuse_address (host_socket);

    if not bind (host_socket) then
      Text_IO.New_Line;
      Text_IO.Put_Line (" Bind error: " & string_error);

      goto end_app_label1;
    end if;

    Text_IO.New_Line;
    Text_IO.Put_Line (" Binded  at address " &
      get_addresses (get_addresses (host_socket)) & " and at port " &
      get_port (get_addresses (host_socket)));

    b2 :
    declare

      task type recv_send_task (
        host_address_family : Address_family;
        remote_addr : not null access addresses;
        host_buff   : not null access socket_buffer
      );

      task body recv_send_task -- See ARM-2012 7.6 (9.2/2)
      is
        use Task_Identification;

        this_task_id_str    : constant String   := Image (Current_Task);
        remote_actual_addr  : aliased addresses := remote_addr.all;
        host_old_buff : aliased socket_buffer := get_buffer_init (host_buff);

        use  Ada.Strings.Unbounded;

        message : Unbounded_String := To_Unbounded_String ("");

      begin

        if is_null (remote_actual_addr) then
          Text_IO.Put_Line (this_task_id_str &
            " remote address not configured.");
          Text_IO.Put_Line (this_task_id_str & " quitting.");

          goto finish2_task_label;
        end if;

        if is_empty (host_old_buff) or else
            actual_data_size (host_old_buff) < 1
        then
          Text_IO.Put_Line (this_task_id_str & " buffer is empty.");
          Text_IO.Put_Line (this_task_id_str & " quitting.");

          goto finish2_task_label;
        end if;

        Text_IO.Put_Line (" " & this_task_id_str & " remote host " &
          get_address_and_port (remote_actual_addr'Access));

        Text_IO.Put_Line (" " & this_task_id_str & " received messages:");

        bt0 :
        declare
          use Streams;

          remote_buff : aliased socket_buffer (
            Stream_Element_Offset (actual_data_size (host_old_buff)) +
            Stream_Element_Offset'(45));

        begin

          bt1 :
          begin
            String'Output (remote_buff'Access, "Thank you for send ");

            loop1 :
            loop
              message := To_Unbounded_String (
                String'Input (host_old_buff'Access));

              String'Output (remote_buff'Access, To_String (message));

              Text_IO.Put_Line (" " & this_task_id_str & " message |" &
                To_String (message) & "|");
            end loop loop1;

          exception
            when buffer_insufficient_space_error =>
              Text_IO.Put_Line (" " & this_task_id_str &
                " all messages showed.");
          end bt1;

          bt2 :
          declare
            host_local_address  : aliased addresses_list := init_addresses
              (ip_or_host => "", -- host addresses.
              port        =>  "0", -- ignored without 'bind'
              ai_socktype =>  udp,
              ai_family   =>  host_address_family
              );

            socket_send   : aliased socket;
            poll_send     : aliased polls.poll_type (1);
            poll_result   : int := 0;
            bytes_send    : ssize_t := 0;
          begin
            if host_local_address'Length < 1 then
              Text_IO.Put_Line (" " & this_task_id_str &
                " Failed to get server host address.");

              Text_IO.Put_Line (" " & this_task_id_str &
                " Quitting this working task.");

              goto finish2_task_label;
            end if;

            if not init_socket (socket_send, host_local_address'Access) then
              Text_IO.Put_Line (" " & this_task_id_str &
                " Failed to create temporary socket.");

              Text_IO.Put_Line (" " & this_task_id_str &
                " Quitting this working task.");

              goto finish2_task_label;
            end if;

            polls.add_events (poll_send'Access, socket_send'Access,
              polls.send_ev);

            Text_IO.Put_Line (" " & this_task_id_str &
              " waiting to send data to remote host");

            poll_result  :=
              polls.start_events_listen (poll_send'Access, 2500); -- 2.5 seconds

            if poll_result > 0 then
              bytes_send := sendto (socket_send, remote_actual_addr, remote_buff); -- block
              Text_IO.Put_Line (" " & this_task_id_str & " sended messages !");
            else
              Text_IO.Put_Line (" " & this_task_id_str &
              " failed in send data");
              Text_IO.Put_Line (" " & this_task_id_str & " to " &
                get_address_and_port (remote_actual_addr) & " host.");

              if poll_result = 0 then
                Text_IO.Put_Line (" " & this_task_id_str &
                  " but it is a normal time_out");
              end if;
            end if;
            if initialized (socket_send) then
              close (socket_send);
            end if;
          end bt2;
        end bt0;

        <<finish2_task_label>>
      end recv_send_task;

      working_task  : access recv_send_task;

      remote_address  : aliased addresses;
      host_buffer     : aliased socket_buffer (400);
      host_poll       : aliased polls.poll_type (1);
      host_socket_family  : constant Address_family  :=
        get_address_family (get_addresses (host_socket));
    begin
      Text_IO.New_Line;

      polls.add_events (host_poll'Access, host_socket'Access, polls.receive_ev);

      ok := True;

      loop2 :
      loop

        if polls.start_events_listen (host_poll'Access, 15000) < 1 then
        -- 15 seconds timeout

          if initialized (host_socket) then
            close (host_socket);
          end if;

          Text_IO.Put_Line (" Main event 15 seconds Time_out.");
          Text_IO.Put_Line (" Waiting 5 seconds to allow enough time " &
          "for working tasks finish.");
          Text_IO.New_Line;

          delay 5.0;

          Text_IO.Put_Line (" Have a nice day and night. Bye!");
          Text_IO.New_Line;

          exit loop2;
        end if;


        if receive_from (host_socket, host_buffer, remote_address) > 0 then -- block
          -- For the curious: We believe the task(s) will not leak.
          -- Reason: ARM-2012 7.6 (9.2/2) :-)
          working_task  :=
            new recv_send_task (host_socket_family,
              remote_address'Access, host_buffer'Access);

        end if;

        polls.clear_all_event_responses (host_poll'Access);

        clean (host_buffer);
      end loop loop2;
    end b2;

    <<end_app_label1>>

    if initialized (host_socket) then
      close (host_socket);
    end if;

    Text_IO.Put (" " & Command_Line.Command_Name);

    if ok then
      Text_IO.Put_Line (" successfully finalized.");
    else
      Text_IO.Put_Line (" failed.");
    end if;

    Text_IO.New_Line;

  end b0;

  stop_adare_net;

end server2;
