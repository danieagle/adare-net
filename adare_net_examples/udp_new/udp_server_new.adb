
-- Besides this is a multitask and reasonable complete example with Adare_net, you can do more, as:
--
-- (1) More que one listen sockets,
-- (2) Simultaneous listen event_types,
-- (3) Use of others types beyond String:
-- (3.1) From built-in types and records to
-- (3.2) Wide class(es) and tagged types
-- (3.3) And with a more fine treatment, all records, tagged types included, can be endian proof.
-- (4) Etc. ^^
-- But is yet up to you create a yet better real world champion software with Adare_net and you can do it!! ^^

-- Info about this software:
--
-- udp_server_new is an Adare_net example and work in pair with one or more udp_client_new clients.
-- the working address can be ipv6 or ipv4. Automatically the first working address will be picked.
-- mostly common choosen address in server part is "0.0.0.0" or "::" then use localhost or
-- other configured ip address. eg:
-- 127.0.0.1 or ::1  or ? :-) to connect.


with adare_net.base;  use adare_net.base;
with adare_net_init;  use adare_net_init;
with adare_net_exceptions;  use adare_net_exceptions;
with socket_types; use socket_types;

with Ada.Text_IO; use Ada;
with Ada.Command_Line;
with Ada.Task_Identification;
with Ada.Strings.Unbounded;
with Ada.Streams; use Ada.Streams;

procedure udp_server_new
is
begin

  start_adare_net;

  b0 :
  declare
    host_socket_addresses : aliased socket_addresses;
    tmp_socket_address    : aliased socket_address;
    host_socket           : aliased socket;
  begin

    if not create_addresses
      (host_or_ip => "",
      network_port_or_service => "25000",
      Addr_family => any,
      Addr_type => udp,
      response => host_socket_addresses)
    then
      Text_IO.Put_Line ("Failed to discover host addresses.");
      Text_IO.New_Line;
      Text_IO.Put_Line ("last error message => " & string_error);

      goto end_app_label1;
    end if;

    Text_IO.New_Line;

    Text_IO.Put_Line (" Addresses Discovered in this host:");

    while get_address (host_socket_addresses, tmp_socket_address) loop
      Text_IO.Put_Line ("type => " & get_address_type (tmp_socket_address) & " address => " &
        get_address (tmp_socket_address) & " and port => " & get_address_port (tmp_socket_address));
      Text_IO.New_Line;
    end loop;

    if not create_socket  (host_socket_addresses, host_socket, True, True, 35) then
      Text_IO.Put_Line (" Failed to initialize socket: " & string_error);

      goto end_app_label1;
    end if;

    get_address (host_socket, tmp_socket_address);

    Text_IO.New_Line;

    Text_IO.Put_Line (" choosed host address => " & get_address (tmp_socket_address) & " and choosed host port => " &
      get_address_port (tmp_socket_address));

    b1 :
    declare

      task type recv_send_task (connected_sock  : not null socket_access; pre_message : stream_element_array_access)
        with Dynamic_Predicate => is_initialized (connected_sock.all)
          or else is_connected (connected_sock.all);

      task body recv_send_task
      is

        remote_address  : aliased constant socket_address  := get_address (connected_sock.all);

        use Task_Identification;

        this_task_id_str  : constant String := Image (Current_Task);

        recv_send_buffer  : aliased socket_buffer;
        recv_send_buffer2 : aliased socket_buffer;

        tmp_tmp_socket_address  : aliased socket_address;

        size_tmp  : aliased ssize_t  := 0;

        use  Ada.Strings.Unbounded;

        message : Unbounded_String := To_Unbounded_String ("");
      begin
        clear (recv_send_buffer); -- optional, reset all data in buffer
        clear (recv_send_buffer2);  -- optional, reset all data in buffer

        if pre_message /= null then
          Stream_Element_Array'Write (recv_send_buffer'Access, pre_message.all);
        end if;

        Text_IO.New_Line (2);

        Text_IO.Put_Line (" " & this_task_id_str & " remote host connected from [" &
          get_address (remote_address) & "]:" & get_address_port (remote_address) & " and type => " &
          get_address_type (remote_address));

        Text_IO.Put_Line (" " & this_task_id_str & " will wait 2 seconds to start receive data.");
        Text_IO.Put_Line (" " & this_task_id_str & " will wait 0.5 seconds between continuous receive.");

        if not receive_buffer (sock => connected_sock.all,
          data_to_receive =>  recv_send_buffer,
          received_address  =>  tmp_tmp_socket_address,
          receive_count =>  size_tmp,
          miliseconds_start_timeout =>  2000,
          miliseconds_next_timeouts =>  500) or else size_tmp < 1
        then
          if pre_message = null or else pre_message.all'Length < 1 then
            Text_IO.Put_Line (" " & this_task_id_str & " there are a error while receiving or received zero length message.");
            Text_IO.Put_Line (" " & this_task_id_str & " nothing to do.");
            Text_IO.Put_Line (" " & this_task_id_str & " last error message => " & string_error);
            Text_IO.Put_Line (" " & this_task_id_str & " finishing.");

            goto finish1_task_label;
          end if;
        end if;

        size_tmp := size_tmp + (if pre_message = null then 0 else pre_message.all'Length);

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

        Text_IO.Put_Line (" " & this_task_id_str & " waiting 2 seconds to start send data to remote host");
        Text_IO.Put_Line (" " & this_task_id_str & " will wait 0.5 seconds between continuous send.");

        if not send_buffer  (sock => connected_sock.all,
          data_to_send  =>  recv_send_buffer2,
          send_count  =>  size_tmp,
          miliseconds_start_timeout =>  2000,
          miliseconds_next_timeouts =>  500) or else size_tmp < 1
        then
          Text_IO.Put_Line (" " & this_task_id_str & " there are a error while sending data to remote host.");
          Text_IO.Put_Line (" " & this_task_id_str & " nothing to do.");
          Text_IO.Put_Line (" " & this_task_id_str & " last error message => " & string_error);
          Text_IO.Put_Line (" " & this_task_id_str & " finishing.");

          goto finish1_task_label;
        end if;

        Text_IO.Put_Line (" " & this_task_id_str & " sended messages !");

        <<finish1_task_label>>

        if connected_sock /= null and then is_initialized (connected_sock.all) then

          close (connected_sock.all);
        end if;

      end recv_send_task;

      type recv_send_access is access all recv_send_task;

      working_task  : recv_send_access
        with Unreferenced;

      msg_seaa  : aliased stream_element_array_access := null;

      tmp_received_socket : aliased socket;
      tmp_received_socket_access : aliased socket_access := null;

    begin

      Text_IO.New_Line;

      Text_IO.Put_Line (" Start Accepting connect in Main Server.");
      Text_IO.Put_Line (" 20 seconds max timeout between clients.");
      Text_IO.New_Line (2);

      loop2 :
      loop
        if wait_connection  (sock =>  host_socket,  response  => tmp_received_socket,
          data_received =>  msg_seaa, miliseconds_start_timeout => 20000)
        then

          get_socket (tmp_received_socket, tmp_received_socket_access); -- a new copy and access

          -- For the curious: We believe the task(s) will not leak.
          -- Reason: ARM-2012 7.6 (9.2/2) :-)

          working_task  :=  new recv_send_task (tmp_received_socket_access,
            (if msg_seaa = null then null else new Stream_Element_Array'(msg_seaa.all)));

        else
          close (host_socket); -- to disable 'listen' too.

          Text_IO.New_Line (2);

          Text_IO.Put_Line (" Main event 20 seconds Time_out.");
          Text_IO.Put_Line (" Waiting 5 seconds to allow enough time for working tasks finish.");

          Text_IO.New_Line (2);

          delay 5.0;

          Text_IO.Put_Line (" Have a nice day and night. Bye!");
          Text_IO.New_Line (2);

          exit loop2;
        end if;

        Text_IO.New_Line (2);

        Text_IO.Put_Line (" restarting 20 seconds timeout.");

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

end udp_server_new;
