
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
--
-- udp_server_new is an Adare_net example and work in pair with one or more udp_client_new clients.
-- the working address can be ipv6 or ipv4. Automatically the first working address will be picked.
-- mostly common automatic choosen address in server part is "0.0.0.0" or "::" and then use localhost or
-- other configured ip address in udp_client_new. e.g.: 127.0.0.1 or ::1  or ? :-)

with Ada.Text_IO;
use Ada;

with Ada.Task_Identification;

with Ada.Command_Line;
with Ada.Strings.Unbounded;

with adare_net.sockets.utils;
with adare_net.sockets.epolls;
use adare_net.sockets;
use adare_net.sockets.epolls;

with adare_net_exceptions;
use adare_net_exceptions;

with socket_types;
use  socket_types;

with adare_net_init;
use  adare_net_init;

with Interfaces.C;
use Interfaces, Interfaces.C;

procedure udp_server_new
is
begin
  start_adare_net;

  b0 :
  declare
    host_address  : addresses_list_access := null;
    host_socket   : socket_access := null;
    ok            : Boolean :=  False;
    poll_ok       : Boolean :=  False with Unreferenced;
    host_poll     : epoll_access  :=  null;

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
        remote_actual_addr_i  : not null addresses_access;
        host_old_buff       : not null socket_buffer_access
      );

      task body recv_send_task -- See ARM-2012 7.6 (9.2/2)
      is
        remote_actual_addr : constant addresses_access := new addresses'(remote_actual_addr_i.all);

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
          remote_buff : constant socket_buffer_access := new socket_buffer;

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

            host_local_address  : addresses_list_access := null;

            socket_send   : socket_access := null;
            poll_send     : epoll_access  := null;

            poll_result   : int := 0;
            bytes_send    : ssize_t := 0 with Unreferenced;

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

            if not init (poll_send) then
              Text_IO.Put_Line (" " & this_task_id_str & " Failed to init poll event.");

              Text_IO.Put_Line (" " & this_task_id_str & " Quitting this working task.");

              goto finish2_task_label;
            end if;

            if not add (poll_send, socket_send, send_event) then
              Text_IO.Put_Line (" " & this_task_id_str & " Failed to add send event in poll event.");

              Text_IO.Put_Line (" " & this_task_id_str & " Quitting this working task.");

              goto finish2_task_label;
            end if;

            Text_IO.Put_Line (" " & this_task_id_str & " waiting max 2.5 second timeout to send data to remote host");

            poll_result  := poll_wait (poll_send, 2500); -- block, 2.5 seconds timeout.

            if poll_result > 0 and then confirm_send_event (poll_send, socket_send) then

              bytes_send := sendto (socket_send, remote_actual_addr, remote_buff); -- block

              Text_IO.Put_Line (" " & this_task_id_str & " sended messages !");

            else

              Text_IO.Put_Line (" " & this_task_id_str & " failed in send data");

              Text_IO.Put_Line (" " & this_task_id_str & " to " & get_address_and_port (remote_actual_addr) & " host.");

              if poll_result = 0 then

                Text_IO.Put_Line (" " & this_task_id_str & " but it is a normal time_out");

              end if;

            end if;

            if is_initialized (poll_send) then

              poll_ok := close (poll_send);
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
      host_buffer         : constant socket_buffer_access    :=  new socket_buffer;

    begin

      if not init (host_poll) then
        Text_IO.Put_Line ("failed to initialize main server event poll.");
        Text_IO.Put_Line ("finishing.");
        goto end_app_label1;
      end if;

      if not add (host_poll, host_socket, receive_event) then
        Text_IO.Put_Line ("failed to add receive event in event poll.");
        Text_IO.Put_Line ("finishing.");
        goto end_app_label1;
      end if;

      clean (host_buffer);

      Text_IO.New_Line;

      ok := True;

      Text_IO.Put_Line (" Start allowing connect in Main Server.");
      Text_IO.Put_Line (" 15 seconds max timeout between clients.");
      Text_IO.New_Line (2);

      loop2 :
      loop

        if  poll_wait (host_poll, 15000) < 1  or else
            not confirm_receive_event (host_poll, host_socket)
        then -- block, 15 seconds timeout.

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

        reset_poll_result (host_poll);

        clean (host_buffer);

        Text_IO.New_Line (2);

        Text_IO.Put_Line (" restarting 15 seconds timeout.");

        Text_IO.New_Line;

      end loop loop2;
    end b2;

    ok  :=  True;

    <<end_app_label1>>

    if is_initialized (host_poll) then

      poll_ok := close (host_poll);
    end if;

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

end udp_server_new;
