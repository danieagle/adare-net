
-- Besides this is a multitask and reasonable complete example with Adare_net, you can do more, as:
--
-- (1) More que one listen socket and polls,
-- (2) More length polls,
-- (3) Simultaneous listen event_types,
-- (4) Use of others types beyond String:
-- (4.1) From built-in types and records to
-- (4.2) Wide class(es) and tagged types
-- (4.3) And with a more fine treatment, all records, tagged types included, can be endian proof.
-- (5) Etc. ^^
-- But is yet up to you create a yet better real world champion software with Adare_net and you can do it!! ^^

-- Info about this software:
--
-- tcp_server_new is an Adare_net example and work in pair with one or more tcp_client_new clients.
-- the working address can be ipv6 or ipv4. Automatically the first working address will be picked.
-- mostly common choosen address in server part is "0.0.0.0" or "::" then use localhost or
-- other configured ip address. eg:
-- 127.0.0.1 or ::1  or ? :-)


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

procedure tcp_server_new
is
begin

  start_adare_net;

  b0 :
  declare
    host_addr           : addresses_list_access := null;
    choosed_remote_addr : addresses_access := null; -- ToDo : rename to choosed_server_addr :-)
    host_sock           : socket_access := null;
    mi_poll             : epoll_access  := null;
    mi_poll_ok          : Boolean :=  False with Unreferenced;
  begin
    init_addresses (ip_or_host =>  "", -- host addresses
                      port        =>  "25000",
                      ai_socktype =>  tcp,
                      ai_family   =>  any, -- choose ipv4 and ipv6
                      addr        =>  host_addr);

    if host_addr.all'Length < 1 then

      Text_IO.Put_Line (" Failed to discover addresses in this host. Quitting.");

      goto end_app_label1;
    end if;

    Text_IO.Put_Line (" Addresses Discovered in this host:");

    utils.show_address_and_port (host_addr);

    if not init_socket (host_sock, host_addr) then

      Text_IO.New_Line;
      Text_IO.Put_Line (" Failed to initialize socket: " & string_error);

      goto end_app_label1;
    end if;

    reuse_address (host_sock);

    if not bind (host_sock) then

      Text_IO.New_Line;
      Text_IO.Put_Line (" Bind error: " & string_error);

      goto end_app_label1;
    end if;

    if not listen (host_sock, 9) then

      Text_IO.New_Line;
      Text_IO.Put_Line (" Listen error: " & string_error);

      goto end_app_label1;
    end if;

    choosed_remote_addr  :=  get_addresses (host_sock);

    Text_IO.New_Line;

    Text_IO.Put_Line (" Binded and Listening at address "  & get_addresses (choosed_remote_addr) &
      " and at port " & get_port (choosed_remote_addr));

    b1 :
    declare

      incomming_socket  : socket_access;

      task type recv_send_task (connected_sock  : socket_access);

      task body recv_send_task
      is
        task_socket : constant socket_access := new socket'(connected_sock.all);

        task_poll   : epoll_access  := null;
        pol_ok      : Boolean       := False with Unreferenced;

        result_from_poll  : int := 0;

        remote_address    :  addresses_access;

        use Task_Identification;

        this_task_id_str  : constant String := Image (Current_Task);

      begin

        if not (initialized (task_socket) or else connected (task_socket)) then
          Text_IO.Put_Line (this_task_id_str & " Incomming socket not initialized or connected.");
          Text_IO.Put_Line (this_task_id_str & " Quitting this working task.");

          goto finish1_task_label;
        end if;

        if not init (task_poll) then
          Text_IO.Put_Line (this_task_id_str & " failed to init poll event.");
          Text_IO.Put_Line (this_task_id_str & " quiting.");
          goto finish1_task_label;
        end if;

        remote_address  := get_addresses (task_socket);

        reset_poll_result (task_poll);

        bt0 :
        declare

          recv_send_buffer  : constant socket_buffer_access := new socket_buffer;
          recv_send_buffer2 : constant socket_buffer_access := new socket_buffer;

          size_tmp          : ssize_t  := 1;

          use  Ada.Strings.Unbounded;

          message : Unbounded_String := To_Unbounded_String ("");
        begin
          clean (recv_send_buffer);
          clean (recv_send_buffer2);

          Text_IO.Put_Line (" " & this_task_id_str & " remote host connected from " &
            get_address_and_port (remote_address));

          if not add (task_poll, task_socket, receive_event) then
            Text_IO.Put_Line (" " & this_task_id_str & " Failed to setup receive_event.");
            Text_IO.Put_Line (" Aborting " & this_task_id_str & ".");

            goto finish1_task_label;
          end if;

          Text_IO.Put_Line (" " & this_task_id_str & " will wait 2 seconds to receive data.");

          result_from_poll  := poll_wait (task_poll, 2000); -- block, 2 seconds time_out

          if result_from_poll = 0 then
            Text_IO.Put_Line (" " & this_task_id_str & " 2 seconds Timeout.");
            Text_IO.Put_Line (" " & this_task_id_str & " Aborting.");

            goto finish1_task_label;
          end if;

          if result_from_poll < 0 then
            Text_IO.Put_Line (" " & this_task_id_str & " errors while waiting an event.");

            Text_IO.Put_Line (" " & this_task_id_str & " remote GraceFull Shutdown ? " &
              Boolean'(is_gracefull_shutdown_error (task_poll, task_socket))'Image);

            Text_IO.Put_Line (" " & this_task_id_str & " remote disconneted ? "
              & Boolean'(is_hangup_error (task_poll, task_socket))'Image);

            Text_IO.Put_Line (" " & this_task_id_str & " other socket errors ? "
              & Boolean'(is_other_error (task_poll, task_socket))'Image);

            Text_IO.Put_Line (" " & this_task_id_str & " Aborting.");

            goto finish1_task_label;
          end if;

          if not confirm_receive_event (task_poll, task_socket) then
            Text_IO.Put_Line (" " & this_task_id_str & " Timeout without receive event.");
            Text_IO.Put_Line (" " & this_task_id_str & " Aborting.");

            goto finish1_task_label;
          end if;

          size_tmp  := receive (task_socket, recv_send_buffer); -- block and initialize recv_send_buffer

          if size_tmp = socket_error then

            Text_IO.Put_Line (" " & this_task_id_str & " An error occurred during data reception.");
            Text_IO.Put_Line (" " & this_task_id_str & " finishing task.");

            goto finish1_task_label;
          end if;

          if size_tmp <= 0 then
            Text_IO.Put_Line (" " & this_task_id_str & " received zero length message.");
            Text_IO.Put_Line (" " & this_task_id_str & " nothing to do.");
            Text_IO.Put_Line (" " & this_task_id_str & " finishing.");

            goto finish1_task_label;
          end if;

          Text_IO.Put_Line (" " & this_task_id_str & " received messages!");

          Text_IO.Put_Line (" " & this_task_id_str & " message length " & size_tmp'Image & " bytes.");

          bt1 :
          begin
            String'Output (recv_send_buffer2, "Thank you for send ");

            loop1 :
            loop
              message := To_Unbounded_String (String'Input (recv_send_buffer));

              String'Output (recv_send_buffer2, To_String (message));

              Text_IO.Put_Line (" " & this_task_id_str & " message |" & To_String (message) & "|");
            end loop loop1;

          exception

            when buffer_insufficient_space_error =>

              Text_IO.Put_Line (" " & this_task_id_str & " all messages showed.");

          end bt1;

          reset_poll_result (task_poll);

          if not update (task_poll, task_socket, send_event) then
            Text_IO.Put_Line (" " & this_task_id_str & " failed to update poll to send data");

            goto finish1_task_label;
          end if;

          Text_IO.Put_Line (" " & this_task_id_str & " waiting 2 seconds to send data to remote host");

          result_from_poll  := poll_wait (task_poll, 2000); -- block, 2 seconds timeout.

          if result_from_poll = 0 then
            Text_IO.Put_Line (" " & this_task_id_str & " Timeout before can send message.");
            Text_IO.Put_Line (" " & this_task_id_str & " Aborting.");

            goto finish1_task_label;
          end if;

          if result_from_poll < 0 then
            Text_IO.Put_Line (" " & this_task_id_str & " errors while waiting an event.");

            Text_IO.Put_Line (" " & this_task_id_str & " remote GraceFull Shutdown ? " &
              Boolean'(is_gracefull_shutdown_error (task_poll, task_socket))'Image);

            Text_IO.Put_Line (" " & this_task_id_str & " remote disconneted ? "
              & Boolean'(is_hangup_error (task_poll, task_socket))'Image);

            Text_IO.Put_Line (" " & this_task_id_str & " other socket errors ? "
              & Boolean'(is_other_error (task_poll, task_socket))'Image);

            Text_IO.Put_Line (" " & this_task_id_str & " Aborting.");

            goto finish1_task_label;
          end if;

          if confirm_send_event (task_poll, task_socket) then

            size_tmp  := send (task_socket, recv_send_buffer2); -- block

            Text_IO.Put_Line (" " & this_task_id_str & " sended messages !");

            goto finish1_task_label;
          end if;

          Text_IO.Put_Line (" " & this_task_id_str & " failed in send data.");

        end bt0;

        <<finish1_task_label>>

        if is_initialized (task_poll) then

          pol_ok  := close (task_poll);
        end if;

        if initialized (task_socket) then

          close (task_socket);
        end if;

      end recv_send_task;

      type recv_send_access is access all recv_send_task;

      working_task  : recv_send_access
        with Unreferenced;

    begin

      Text_IO.New_Line;

      if not init (mi_poll) then

        Text_IO.Put_Line (" Failed to init main socket poll.");
        Text_IO.Put_Line (" Aborting Server.");
        goto end_app_label1;
      end if;

      if not add (mi_poll, host_sock, accept_socket_event) then

        Text_IO.Put_Line (" Failed to add accept event in main socket poll.");
        Text_IO.Put_Line (" Aborting Server.");
        goto end_app_label1;
      end if;

      Text_IO.Put_Line (" Start Accepting connect in Main Server.");
      Text_IO.Put_Line (" 15 seconds max timeout between clients.");
      Text_IO.New_Line (2);

      loop2 :
      loop

        if poll_wait (mi_poll, 15000) < 1 or else not confirm_accept_event (mi_poll, host_sock) then

          close (host_sock); -- to disable 'listen' too.

          Text_IO.New_Line (2);

          Text_IO.Put_Line (" Main event 15 seconds Time_out.");
          Text_IO.Put_Line (" Waiting 5 seconds to allow enough time for working tasks finish.");

          Text_IO.New_Line (2);

          delay 5.0;

          Text_IO.Put_Line (" Have a nice day and night. Bye!");
          Text_IO.New_Line (2);

          exit loop2;
        end if;

        if accept_socket (host_sock, incomming_socket) then -- block. accepted socket incomming_socket is allways a new one.
          -- For the curious: We believe the task(s) will not leak.
          -- Reason: ARM-2012 7.6 (9.2/2) :-)
          working_task  :=  new recv_send_task (incomming_socket);
        end if;

        reset_poll_result (mi_poll);

        Text_IO.New_Line (2);

        Text_IO.Put_Line ("restarting 15 seconds timeout.");

      end loop loop2;
    end b1;

    <<end_app_label1>>

    if is_initialized (mi_poll) then

      mi_poll_ok  :=  close (mi_poll);
    end if;

    if initialized (host_sock) then

      close (host_sock);
    end if;

    Text_IO.Put (" " & Command_Line.Command_Name & " finished. ");

    Text_IO.New_Line;

  end b0;

  stop_adare_net;

end tcp_server_new;
