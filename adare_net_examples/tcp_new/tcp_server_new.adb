
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


with adare_net.base;  use adare_net.base;
with adare_net_init;  use adare_net_init;
with adare_net_exceptions;  use adare_net_exceptions;

with Interfaces.C;  use Interfaces, Interfaces.C;
with Ada.Text_IO; use Ada;
with Ada.Command_Line;
with Ada.Task_Identification;
with Ada.Strings.Unbounded;

procedure tcp_server_new
is
begin

  start_adare_net;

  b0 :
  declare
    host_socket_addresses : aliased socket_addresses;
    tmp_socket_address    : aliased socket_address := null_socket_address;
    host_socket           : aliased socket := null_socket;

  begin

    host_socket_addresses := create_address
      (host_or_ip => "",
      network_port_or_service => "25000",
      Addr_family => any,
      Addr_type => tcp);

    Text_IO.New_Line;

    Text_IO.Put_Line (" Addresses Discovered in this host:");

    while get_address (host_socket_addresses, tmp_socket_address) loop
      Text_IO.Put_Line (" address => " & get_address (tmp_socket_address) & " and port => " & get_address_port (tmp_socket_address));
      Text_IO.New_Line;
    end loop;

    --  rewind (host_socket_addresses);

    host_socket := create_socket (
      sock_address  => host_socket_addresses,
      bind_socket   => True);

    if host_socket = null_socket then
      Text_IO.Put_Line (" Failed to initialize socket: " & string_error);

      goto end_app_label1;
    end if;

    tmp_socket_address  :=  get_address (host_socket);

    Text_IO.New_Line;

    Text_IO.Put_Line (" choosed host address => " & get_address (tmp_socket_address) & " and choosed host port => " & get_address_port (tmp_socket_address));

    b1 :
    declare

      --  incomming_socket  : socket;

      task type recv_send_task (connected_sock  : socket_access);

      task body recv_send_task
      is
        task_socket     : aliased socket := connected_sock.all;

        remote_address  : aliased constant socket_address  := get_address (task_socket);

        use Task_Identification;

        this_task_id_str  : constant String := Image (Current_Task);

      begin

        if not (is_initialized (task_socket) or else is_connected (task_socket)) then
          Text_IO.Put_Line (this_task_id_str & " Incomming socket not initialized or connected.");
          Text_IO.Put_Line (this_task_id_str & " Quitting this working task.");

          goto finish1_task_label;
        end if;

        bt0 :
        declare

          recv_send_buffer  : aliased socket_buffer;
          recv_send_buffer2 : aliased socket_buffer;

          tmp_tmp_socket_address  : aliased socket_address := null_socket_address;

          size_tmp  : Interfaces.C.int  := 0;

          use  Ada.Strings.Unbounded;

          message : Unbounded_String := To_Unbounded_String ("");
        begin
          clear (recv_send_buffer);
          clear (recv_send_buffer2);

          Text_IO.Put_Line (" " & this_task_id_str & " remote host connected from [" &
            get_address (remote_address) & "]:" & get_address_port (remote_address));

          Text_IO.Put_Line (" " & this_task_id_str & " will wait 2 seconds to receive data.");

          size_tmp  :=  receive_buffer_with_timeout (
            sock  =>  task_socket,
            data_to_receive =>  recv_send_buffer,
            received_address  =>  tmp_tmp_socket_address,
            miliseconds_timeout =>  2000
          );

          if size_tmp = 0 then
            Text_IO.Put_Line (" " & this_task_id_str & " received zero length message.");
            Text_IO.Put_Line (" " & this_task_id_str & " nothing to do.");
            Text_IO.Put_Line (" " & this_task_id_str & " finishing.");

            goto finish1_task_label;
          end if;

          Text_IO.Put_Line (" " & this_task_id_str & " received messages!");

          Text_IO.Put_Line (" " & this_task_id_str & " message length " & size_tmp'Image & " bytes.");

          bt1 :
          begin
            String'Output (recv_send_buffer2'Access, "Thank you for send ");

            loop1 :
            loop
              message := To_Unbounded_String (String'Input (recv_send_buffer'Access));

              String'Output (recv_send_buffer2'Access, To_String (message));

              Text_IO.Put_Line (" " & this_task_id_str & " message |" & To_String (message) & "|");
            end loop loop1;

          exception

            when buffer_insufficient_space_error =>

              Text_IO.Put_Line (" " & this_task_id_str & " all messages showed.");

          end bt1;

          Text_IO.Put_Line (" " & this_task_id_str & " waiting 2 seconds to send data to remote host");

          size_tmp  := send_buffer_with_timeout (task_socket, recv_send_buffer2, 2000); -- block

          if size_tmp < 1 then
            Text_IO.Put_Line (" " & this_task_id_str & " failed in send data.");
            goto finish1_task_label;
          end if;

          Text_IO.Put_Line (" " & this_task_id_str & " sended messages !");

        end bt0;

        <<finish1_task_label>>

        if is_initialized (task_socket) then

          close (task_socket);
        end if;

      end recv_send_task;

      type recv_send_access is access all recv_send_task;

      working_task  : recv_send_access
        with Unreferenced;

      msg_seaa  : aliased stream_element_array_access := null;

      tmp_received_socket : aliased socket := null_socket;

    begin

      Text_IO.New_Line;

      Text_IO.Put_Line (" Start Accepting connect in Main Server.");
      Text_IO.Put_Line (" 15 seconds max timeout between clients.");
      Text_IO.New_Line (2);

      loop2 :
      loop

        tmp_received_socket :=  wait_connection_with_timeout (host_socket, 15000, msg_seaa, 50);

        if tmp_received_socket = null_socket then

          close (host_socket); -- to disable 'listen' too.

          Text_IO.New_Line (2);

          Text_IO.Put_Line (" Main event 15 seconds Time_out.");
          Text_IO.Put_Line (" Waiting 5 seconds to allow enough time for working tasks finish.");

          Text_IO.New_Line (2);

          delay 5.0;

          Text_IO.Put_Line (" Have a nice day and night. Bye!");
          Text_IO.New_Line (2);

          exit loop2;
        end if;

        -- For the curious: We believe the task(s) will not leak.
        -- Reason: ARM-2012 7.6 (9.2/2) :-)
        working_task  :=  new recv_send_task (new socket'(tmp_received_socket));

        Text_IO.New_Line (2);

        Text_IO.Put_Line (" restarting 15 seconds timeout.");

      end loop loop2;
    end b1;

    <<end_app_label1>>

    if is_initialized (host_socket) then

      close (host_socket);
    end if;

    Text_IO.Put (" " & Command_Line.Command_Name & " finished. ");

    Text_IO.New_Line;

  end b0;

  stop_adare_net;

end tcp_server_new;
