
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
-- tcp_server_new is an Adare_net example and work in pair with one or more tcp_client_new clients.
-- the working address can be ipv6 or ipv4. Automatically the first working address will be picked.
-- mostly common choosen address in server part is "0.0.0.0" or "::" then use localhost or
-- other configured ip address. eg:
-- 127.0.0.1 or ::1  or ? :-) to connect.


with adare_net.base;  use adare_net.base;
with adare_net_init;  use adare_net_init;
with adare_net_exceptions;  use adare_net_exceptions;

with Ada.Text_IO; use Ada;
with Ada.Command_Line;
with Ada.Task_Identification;
with Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;


use Ada.Task_Identification;

procedure tcp_server_new
is
  pragma Unsuppress (All_Checks);
begin

  start_adare_net;

  b0 :
  declare
    host_socket_addresses : socket_addresses_access;
    tmp_socket_address    : socket_address_access;
    host_socket           : socket_access;
  begin

    if not create_addresses
      (host_or_ip => "",
      network_port_or_service => "25000",
      Addr_family => any,
      Addr_type => tcp,
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

      Text_IO.Put_Line ("type => " & get_address_type (tmp_socket_address) &
        " family_type => " & get_family_label (tmp_socket_address) &
        " address => " & get_address (tmp_socket_address) &
        " and port => " & get_address_port (tmp_socket_address));

      Text_IO.New_Line;
    end loop;

    if not create_socket  (host_socket_addresses, host_socket, True, True, 35) then

      Text_IO.Put_Line (" Failed to initialize socket: " & string_error);

      goto end_app_label1;
    end if;

    get_address (host_socket, tmp_socket_address);

    Text_IO.New_Line;

    Text_IO.Put_Line (" choosed: host address => " & get_address (tmp_socket_address) & " port => " &
      get_address_port (tmp_socket_address) & " type => " & get_address_type (tmp_socket_address) &
      " family_type => " & get_family_label (tmp_socket_address));

    b1 :
    declare

      task type recv_send_task (connected_sock  : not null socket_access)
        with Dynamic_Predicate => is_initialized (connected_sock)
          and then is_connected (connected_sock);

      task body recv_send_task
      is
        task_sock       : constant socket_access          :=  connected_sock;
        remote_address  : constant socket_address_access  :=  get_address (task_sock);

        this_task_id_str  : constant String := Image (Current_Task);

        recv_send_buffer  : constant socket_buffer_access  := new socket_buffer;
        recv_send_buffer2 : constant socket_buffer_access  := new socket_buffer;

        tmp_tmp_socket_address  : socket_address_access :=  null;

        size_tmp  : int  := 0;

        use  Ada.Strings.Unbounded;

        message : Unbounded_String := To_Unbounded_String ("");
      begin

        -- Text_IO.Put_Line (" " & this_task_id_str & " all messages showed.");

        clear (recv_send_buffer);   -- optional, reset all data in buffer
        clear (recv_send_buffer2);  -- optional, reset all data in buffer

        Text_IO.New_Line (2);

        Text_IO.Put_Line (" " & this_task_id_str & " remote host connected from [" &
          get_address (remote_address) & "]:" & get_address_port (remote_address) &
          " type => " & get_address_type (tmp_socket_address) &
          " family_type => " & get_family_label (tmp_socket_address));

        Text_IO.Put_Line (" " & this_task_id_str & " will wait until 2 seconds to start receive data.");
        Text_IO.Put_Line (" " & this_task_id_str & " will wait until 0.5 seconds between continuous receive.");

        if not receive_buffer (sock => task_sock,
          data_to_receive =>  recv_send_buffer,
          received_address  =>  tmp_tmp_socket_address,
          receive_count =>  size_tmp,
          miliseconds_start_timeout =>  2000,
          miliseconds_next_timeouts =>  500) or else size_tmp < 1
        then
          Text_IO.Put_Line (" " & this_task_id_str & " An error occurred while receiving or the length of message received is zero.");
          Text_IO.Put_Line (" " & this_task_id_str & " Nothing to do.");
          Text_IO.Put_Line (" " & this_task_id_str & " Last error message => " & string_error);
          Text_IO.Put_Line (" " & this_task_id_str & " Finishing...");

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

        Text_IO.Put_Line (" " & this_task_id_str & " waiting until 2 seconds to start send data to remote host");
        Text_IO.Put_Line (" " & this_task_id_str & " will wait until 0.5 seconds between continuous send.");

        if not send_buffer  (sock => task_sock,
          data_to_send  =>  recv_send_buffer2,
          send_count  =>  size_tmp,
          miliseconds_start_timeout =>  2000,
          miliseconds_next_timeouts =>  500) or else size_tmp < 1
        then
          Text_IO.Put_Line (" " & this_task_id_str & " An error occurred while sending data to remote host.");
          Text_IO.Put_Line (" " & this_task_id_str & " Nothing to do.");
          Text_IO.Put_Line (" " & this_task_id_str & " Last error message => " & string_error);
          Text_IO.Put_Line (" " & this_task_id_str & " Finishing...");

          goto finish1_task_label;
        end if;

        Text_IO.Put_Line (" " & this_task_id_str & " sended messages !");

        <<finish1_task_label>>

        if is_initialized (task_sock) then

          close (task_sock);
        end if;

      end recv_send_task;

      type recv_send_access is access all recv_send_task;

      working_task  : recv_send_access
        with Unreferenced;

      msg_seaa  : stream_element_array_access := null;

      tmp_received_socket_access : socket_access := null;

    begin

      Text_IO.New_Line;

      Text_IO.Put_Line (" Start Accepting connect in Main Server.");
      Text_IO.Put_Line (" 20 seconds max timeout between clients.");
      Text_IO.New_Line (2);

      loop2 :
      loop
        if not wait_connection  (sock =>  host_socket,  response  => tmp_received_socket_access,
          data_received =>  msg_seaa, miliseconds_start_timeout => 20000)
        then
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

        -- For the curious: We believe the task(s) will not leak.
        -- Reason: ARM-2012 7.6 (9.2/2) :-)
        working_task  :=  new recv_send_task (tmp_received_socket_access);

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

end tcp_server_new;
