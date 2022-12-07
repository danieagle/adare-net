
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

with adare_net_exceptions;
use adare_net_exceptions;

with socket_types;
use  socket_types;

with adare_net_init;
use  adare_net_init;

with Interfaces.C;
use Interfaces, Interfaces.C;

procedure server
is
begin

  start_adare_net;

  b0 :
  declare
    host_addr : constant addresses_list_access := new addresses_list'(
      init_addresses (ip_or_host =>  "", -- host addresses
                      port        =>  "25000",
                      ai_socktype =>  tcp,
                      ai_family   =>  any -- choose ipv4 and ipv6
                      )
    );


    host_sock : socket_access := null;

    ok        : Boolean := False;

    choosed_remote_addr :  addresses_access := null;
  begin
    if host_addr'Length < 1 then

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

    choosed_remote_addr  :=  new addresses'(get_addresses (host_sock));

    Text_IO.New_Line;

    Text_IO.Put_Line (" Binded and Listening at address "  & get_addresses (choosed_remote_addr) &
      " and at port " & get_port (choosed_remote_addr));

    b1 :
    declare

      incomming_socket  : socket_access;
      mi_poll           : aliased polls.poll_type (1); -- each poll_type have a independent range of 1 .. 255


      task type recv_send_task (sock  : socket_access);


      task body recv_send_task -- See ARM-2012 7.6 (9.2/2)
      is
        incomming_sock : constant socket_access :=  new socket'(sock.all);
      begin

        if not (initialized (incomming_sock) or else connected (incomming_sock)) then
          Text_IO.Put_Line (" Incomming socket not initialized or connected.");
          Text_IO.Put_Line (" Quitting this working task.");

          goto finish2_task_label;
        end if;

        bt0 :
        declare

          use Task_Identification;

          this_task_id_str  : constant String := Image (Current_Task);

          task_poll         : aliased polls.poll_type (1);

          recv_send_buffer  : socket_buffer_access := null;
          recv_send_buffer2 : socket_buffer_access := null;

          size_tmp          : ssize_t  := 1;
          result_from_poll  : int := 0;

          use  Ada.Strings.Unbounded;

          message : Unbounded_String := To_Unbounded_String ("");
        begin

          Text_IO.Put_Line (" " & this_task_id_str & " remote host " &
            get_address_and_port (new addresses'(get_addresses (incomming_sock))));

          polls.add_events (task_poll'Access, incomming_sock, polls.receive_ev); -- all *_ev events can be or'ed.

          Text_IO.Put_Line (" " & this_task_id_str & " waiting to receive data.");

          result_from_poll  := polls.start_events_listen (task_poll'Access, 2000); -- block, 2 seconds time_out

          if result_from_poll > 0 then

            size_tmp  := receive (incomming_sock, recv_send_buffer); -- block and initialize recv_send_buffer

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

                --  the following is necessary

                --  if recv_send_buffer = null then
                --    recv_send_buffer := new socket_buffer;
                --  end if;

                --  But 'receive' already made It in 'recv_send_buffer' for us.

                String'Output (recv_send_buffer, "Thank you for send ");

                if recv_send_buffer2 = null then
                  recv_send_buffer2 := new socket_buffer;
                end if;

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
              if polls.hang_up_error (task_poll'Access, incomming_sock) then

                Text_IO.Put_Line (" " & this_task_id_str & " remote host closed the connection. Quitting.");

                goto finish1_task_label;
              end if;
            end if;
          end if;

          polls.clear_all_event_responses (task_poll'Access);

          polls.update (task_poll'Access, incomming_sock, polls.send_ev);

          Text_IO.Put_Line (" " & this_task_id_str & " waiting to send data to remote host");

          result_from_poll  := polls.start_events_listen (task_poll'Access, 2000); -- block, 2 seconds timeout.

          if result_from_poll > 0 then

            size_tmp  := send (incomming_sock, recv_send_buffer2); -- block

            Text_IO.Put_Line (" " & this_task_id_str & " sended messages !");

          else

            Text_IO.Put_Line (" " & this_task_id_str & " failed in send data.");

            if result_from_poll = 0 then

              Text_IO.Put_Line (" " & this_task_id_str & " but it is a normal time_out");

            else

              if polls.hang_up_error (task_poll'Access, incomming_sock) then

                Text_IO.Put_Line (" " & this_task_id_str & " remote host closed the connection. Quitting");

              end if;
            end if;
          end if;

          <<finish1_task_label>>

          if initialized (incomming_sock) then

            close (incomming_sock);

          end if;
        end bt0;

        <<finish2_task_label>>
      end recv_send_task;

      type recv_send_access is access all recv_send_task;

      working_task  : recv_send_access
        with Unreferenced;

    begin

      Text_IO.New_Line;

      polls.add_events (mi_poll'Access, host_sock, polls.accept_ev);

      ok := True;

      loop2 :
      loop

        if polls.start_events_listen (mi_poll'Access, 15000) < 1 then -- block, 15 seconds timeout

          close (host_sock); -- to disable 'listen' too.

          Text_IO.Put_Line (" Main event 15 seconds Time_out.");
          Text_IO.Put_Line (" Waiting 5 seconds to allow enough time for working tasks finish.");

          Text_IO.New_Line;

          delay 5.0;

          Text_IO.Put_Line (" Have a nice day and night. Bye!");
          Text_IO.New_Line;

          exit loop2;
        end if;

        if accept_socket (host_sock, incomming_socket) then -- block
          -- For the curious: We believe the task(s) will not leak.
          -- Reason: ARM-2012 7.6 (9.2/2) :-)
          working_task  :=  new recv_send_task (incomming_socket);
        end if;

        polls.clear_all_event_responses (mi_poll'Access);

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

end server;
