
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
-- Tcp1 server with Adare_net example. It work in pair with tcp1 client
-- Automatically, the working address can be ipv6 or ipv4. The first working one will be picked.

with Ada.Text_IO;
use Ada;

with Ada.Task_Identification;

with Ada.Command_Line;
with Ada.Strings.Unbounded;

with adare_net.sockets.utils;
with adare_net.sockets.polls;
use adare_net.sockets;
use adare_net.sockets.polls;

with adare_net_exceptions;
use adare_net_exceptions;

with socket_types;
use  socket_types;

with adare_net_init;
use  adare_net_init;

with Interfaces.C;
use Interfaces, Interfaces.C;

procedure tcp_server
is
begin

  start_adare_net;

  b0 :
  declare
    host_addr           : addresses_list_access := null;
    choosed_remote_addr : addresses_access := null;
    host_sock           : socket_access := null;
    ok                  : Boolean := False;
  begin
    init_addresses (ip_or_host =>  "::1", -- host addresses
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
      mi_poll           : aliased poll_type (1);

      task type recv_send_task (connected_sock  : socket_access);

      task body recv_send_task
      is
        remote_address  : addresses_access := get_addresses (connected_sock);
      begin

        if not (initialized (connected_sock) or else connected (connected_sock)) then
          Text_IO.Put_Line (" Incomming socket not initialized or connected.");
          Text_IO.Put_Line (" Quitting this working task.");

          goto finish2_task_label;
        end if;

        bt0 :
        declare

          use Task_Identification;

          this_task_id_str  : constant String := Image (Current_Task);

          task_poll         : aliased poll_type (1);

          recv_send_buffer  : socket_buffer_access := new socket_buffer;
          recv_send_buffer2 : socket_buffer_access := new socket_buffer;

          size_tmp          : ssize_t  := 1;
          result_from_poll  : int := 0;

          use  Ada.Strings.Unbounded;

          message : Unbounded_String := To_Unbounded_String ("");
        begin
          clean (recv_send_buffer);
          clean (recv_send_buffer2);
          clear_all_event_responses(task_poll'Access);

          Text_IO.Put_Line (" " & this_task_id_str & " remote host " &
            get_address_and_port (remote_address));

          add_events (task_poll'Access, connected_sock, receive_ev); -- all *_ev events can be or'ed.

          Text_IO.Put_Line (" " & this_task_id_str & " waiting to receive data.");

          result_from_poll  := start_events_listen (task_poll'Access, 3000); -- block, 3 seconds time_out

          if result_from_poll > 0 then

            size_tmp  := receive (connected_sock, recv_send_buffer); -- block and initialize recv_send_buffer

            if size_tmp = socket_error then

              Text_IO.Put_Line (" " & this_task_id_str & " An error occurred during reception.");
              Text_IO.Put_Line (" " & this_task_id_str & " finishing task.");

              goto finish1_task_label;
            end if;

            Text_IO.Put_Line (" " & this_task_id_str & " received message:");
            Text_IO.Put_Line (" " & this_task_id_str & " message len " & size_tmp'Image & " bytes.");

            if size_tmp > 0 then

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

            end if;
          else

            Text_IO.Put_Line (" " & this_task_id_str & " failed in receive data.");

            if result_from_poll = 0 then

              Text_IO.Put_Line (" " & this_task_id_str & " but it is a normal time_out.");

            else
              if hang_up_error (task_poll'Access, connected_sock) then

                Text_IO.Put_Line (" " & this_task_id_str & " remote host closed the connection. Quitting.");

                goto finish1_task_label;
              end if;
            end if;
          end if;

          clear_all_event_responses (task_poll'Access);

          update (task_poll'Access, connected_sock, send_ev);

          Text_IO.Put_Line (" " & this_task_id_str & " waiting to send data to remote host");

          result_from_poll  := start_events_listen (task_poll'Access, 3000); -- block, 3 seconds timeout.

          if result_from_poll > 0 then

            size_tmp  := send (connected_sock, recv_send_buffer2); -- block

            Text_IO.Put_Line (" " & this_task_id_str & " sended messages !");

          else

            Text_IO.Put_Line (" " & this_task_id_str & " failed in send data.");

            if result_from_poll = 0 then

              Text_IO.Put_Line (" " & this_task_id_str & " but it is a normal time_out");

            else

              if hang_up_error (task_poll'Access, connected_sock) then

                Text_IO.Put_Line (" " & this_task_id_str & " remote host closed the connection. Quitting");

              end if;
            end if;
          end if;

          <<finish1_task_label>>

          if initialized (connected_sock) then

            close (connected_sock);

          end if;
        end bt0;

        <<finish2_task_label>>
      end recv_send_task;

      type recv_send_access is access all recv_send_task;

      working_task  : recv_send_access
        with Unreferenced;

    begin

      Text_IO.New_Line;

      reset_all (mi_poll'Access); -- DNM

      add_events (mi_poll'Access, host_sock, accept_ev);

      ok := True;

      loop2 :
      loop

        if start_events_listen (mi_poll'Access, 15000) < 1 then -- block, 15 seconds timeout

          Text_IO.Put_Line (string_error);

          close (host_sock); -- to disable 'listen' too.

          Text_IO.Put_Line (" Main event 15 seconds Time_out.");
          Text_IO.Put_Line (" Waiting 5 seconds to allow enough time for working tasks finish.");

          Text_IO.New_Line;

          delay 5.0;

          Text_IO.Put_Line (" Have a nice day and night. Bye!");
          Text_IO.New_Line;

          exit loop2;
        end if;

        if accept_socket (host_sock, incomming_socket) then -- block. accepted socket incomming_socket is allways a new one.
          -- For the curious: We believe the task(s) will not leak.
          -- Reason: ARM-2012 7.6 (9.2/2) :-)
          working_task  :=  new recv_send_task (incomming_socket);
        end if;

        clear_all_event_responses (mi_poll'Access);

      end loop loop2;
    end b1;

    <<end_app_label1>>

    if initialized (host_sock) then

      close (host_sock);

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

end tcp_server;
