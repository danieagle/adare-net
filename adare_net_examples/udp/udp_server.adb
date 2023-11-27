
-- Besides this is a multitask and reasonable complete example with Adare_net, you can do more, as:
--
-- (1) More que one listen socket and polls,
-- (2) More length polls,
-- (3) Simultaneous listen event_types,
-- (4) Use of others types beyond String:
-- (4.1) From built-in types and records to
-- (4.2) Wide class and tagged types
-- (4.3) And with a more fine treatment, all records, tagged types included, can be endian proof.
-- (5) Etc. ^^
-- But is yet up to you create a yet better real world champion software with Adare_net and you can do it!! ^^

-- Info about this software:
-- Udp server with Adare_net example. It work in pair with udp client
-- The working address can be ipv6 or ipv4, automatically. The first working one will be picked.

with Ada.Text_IO;
use Ada;

with Ada.Task_Identification;

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

procedure udp_server
is
begin
  start_adare_net;

  b0 :
  declare
    host_address  : addresses_list_access := null;
    host_socket   : socket_access := null;
    ok            : Boolean :=  False;

  begin

    init_addresses
      (ip_or_host =>  "", -- host addresses
      port        =>  "25000",
      ai_socktype =>  udp,
      ai_family   =>  any, -- choose ipv4 and ipv6
      addr        => host_address);

    if host_address.all'Length < 1 then

      Text_IO.Put_Line (" Failed to discover addresses in this host");
      Text_IO.Put_Line (" Quitting.");

      goto end_app_label1;
    end if;

    Text_IO.Put_Line (" Addresses Discovered in this host:");

    utils.show_address_and_port (host_address);

    if not init_socket (host_socket, host_address) then

      Text_IO.New_Line;

      Text_IO.Put_Line (" Failed to initialize socket: " & string_error);

      goto end_app_label1;
    end if;

    reuse_address (host_socket);

    if not bind (host_socket) then

      Text_IO.New_Line;

      Text_IO.Put_Line (" Bind error: " & string_error);

      goto end_app_label1;
    end if;

    Text_IO.New_Line;

    Text_IO.Put_Line (" Binded  at address " & get_addresses (get_addresses (host_socket)) & " and at port " &
      get_port (get_addresses (host_socket)));

    b2 :
    declare

      task type recv_send_task (
        host_address_family : Address_family;
        remote_actual_addr  : not null addresses_access;
        host_old_buff       : not null socket_buffer_access
      );

      task body recv_send_task -- See ARM-2012 7.6 (9.2/2)
      is
        use Task_Identification;

        this_task_id_str    : constant String   := Image (Current_Task);

        use  Ada.Strings.Unbounded;

        message : Unbounded_String := To_Unbounded_String ("");

      begin

        if is_null (remote_actual_addr) then

          Text_IO.Put_Line (this_task_id_str & " remote address not configured. quitting.");

          goto finish2_task_label;
        end if;

        if is_empty (host_old_buff) or else actual_data_size (host_old_buff) < 1 then

          Text_IO.Put_Line (this_task_id_str & " buffer is empty. quitting.");

          goto finish2_task_label;
        end if;

        Text_IO.Put_Line (" " & this_task_id_str & " remote host " & get_address_and_port (remote_actual_addr));

        Text_IO.Put_Line (" " & this_task_id_str & " received messages:");

        bt0 :
        declare
          remote_buff : socket_buffer_access := new socket_buffer;

        begin

          clean (remote_buff);

          bt1 :
          begin

            String'Output (remote_buff, "Thank you for send ");

            loop1 :
            loop

              message := To_Unbounded_String (String'Input (host_old_buff));

              String'Output (remote_buff, To_String (message));

              Text_IO.Put_Line (" " & this_task_id_str & " message |" & To_String (message) & "|");

            end loop loop1;

          exception

            when buffer_insufficient_space_error =>
              Text_IO.Put_Line (" " & this_task_id_str & " all messages showed.");

          end bt1;

          bt2 :
          declare

            host_local_address  : addresses_list_access;

            socket_send   : socket_access;
            poll_send     : aliased polls.poll_type (1);

            poll_result   : int := 0;
            bytes_send    : ssize_t := 0;

          begin
            init_addresses
              (ip_or_host   => "", -- host addresses.
               port         =>  "0", -- ignored without 'bind'
               ai_socktype  =>  udp,
               ai_family    =>  host_address_family,
               addr         =>  host_local_address);

            if host_local_address.all'Length < 1 then

              Text_IO.Put_Line (" " & this_task_id_str & " Failed to get server host address.");

              Text_IO.Put_Line (" " & this_task_id_str & " Quitting this working task.");

              goto finish2_task_label;
            end if;

            if not init_socket (socket_send, host_local_address) then

              Text_IO.Put_Line (" " & this_task_id_str & " Failed to create temporary socket.");

              Text_IO.Put_Line (" " & this_task_id_str & " Quitting this working task.");

              goto finish2_task_label;
            end if;

            polls.add_events (poll_send'Access, socket_send, polls.send_ev);

            Text_IO.Put_Line (" " & this_task_id_str & " waiting to send data to remote host");

            poll_result  := polls.start_events_listen (poll_send'Access, 2500); -- block, 2.5 seconds timeout.

            if poll_result > 0 then

              bytes_send := sendto (socket_send, remote_actual_addr, remote_buff); -- block

              Text_IO.Put_Line (" " & this_task_id_str & " sended messages !");

            else

              Text_IO.Put_Line (" " & this_task_id_str & " failed in send data");

              Text_IO.Put_Line (" " & this_task_id_str & " to " & get_address_and_port (remote_actual_addr) & " host.");

              if poll_result = 0 then

                Text_IO.Put_Line (" " & this_task_id_str & " but it is a normal time_out");

              end if;

            end if;

            if initialized (socket_send) then

              close (socket_send);

            end if;
          end bt2;
        end bt0;

        <<finish2_task_label>>
      end recv_send_task;

      working_task  : access recv_send_task
        with Unreferenced;

      host_socket_family  : constant Address_family := get_address_family (get_addresses (host_socket));
      remote_address      : addresses_access        := null;
      host_buffer         : socket_buffer_access    :=  new socket_buffer;
      host_poll           : aliased polls.poll_type (1);

    begin

      clean (host_buffer);

      Text_IO.New_Line;

      polls.add_events (host_poll'Access, host_socket, polls.receive_ev);

      ok := True;

      loop2 :
      loop

        if polls.start_events_listen (host_poll'Access, 15000) < 1 then -- block, 15 seconds timeout.

          if initialized (host_socket) then

            close (host_socket);

          end if;

          Text_IO.Put_Line (" Main event 15 seconds Time_out.");
          Text_IO.Put_Line (" Waiting 5 seconds to allow enough time for working tasks finish.");

          Text_IO.New_Line;

          delay 5.0;

          Text_IO.Put_Line (" Have a nice day and night. Bye!");
          Text_IO.New_Line;

          exit loop2;
        end if;


        if receive_from (host_socket, host_buffer, remote_address) > 0 then -- block

          -- For the curious: We believe the task(s) will not leak.
          -- Reason: ARM-2012 7.6 (9.2/2) :-)

          working_task  := new recv_send_task (host_socket_family, remote_address, get_buffer_init (host_buffer));

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

end udp_server;
