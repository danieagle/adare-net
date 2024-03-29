
-- This is an over simplified example of udp client with Adare_net, :-)
-- but is yet up to you create a real world champion software with Adare_net and you can do it!! ^^

-- Info about this software:
-- Udp client with Adare_net example. It work in pair with udp server


with Ada.Command_Line;
with Ada.Text_IO;
use Ada, Ada.Command_Line;

with adare_net.sockets.epolls;
with adare_net.sockets.utils;
use adare_net.sockets;
use adare_net.sockets.epolls;

with adare_net_exceptions;
use adare_net_exceptions;

with adare_net_init;
use  adare_net_init;

with socket_types;
use socket_types;

with Interfaces.C;
use Interfaces.C;

procedure udp_client_new
is
begin

  start_adare_net;

  if Argument_Count < 4 then

    Text_IO.New_Line;

    Text_IO.Put_Line (" Usage: " & Command_Name & " host port ""message1"" ""message2"" ""message_$n"" ");
    Text_IO.New_Line;
    Text_IO.Put_Line (" Minimum of 2 messages ");
    Text_IO.New_Line (2);
    Text_IO.Put_Line (" It will also show that 'buffer' can be read and writed offline ");

    Text_IO.New_Line;

    Set_Exit_Status (Failure);

    stop_adare_net;

    return;
  end if;

  Text_IO.New_Line;

  b0 :
  declare

    buffer  : constant socket_buffer_access  := new socket_buffer;

  begin

    clean (buffer);

    for qtd in 3 .. Argument_Count loop
      String'Output (buffer, Argument (qtd)); -- automatic conversion
    end loop;

    b1 :
    declare
      local_addr    : addresses_list_access := null;
      local_addr2   : addresses_access := null;
      remote_addr   : addresses_access  := null;
      remote_addr2  : addresses_access  := null;
      local_sock    : socket_access := null;

      bytes_tmp     : ssize_t :=  0;

      local_poll    : epoll_access  :=  null;

      poll_result   : int := 0;
      pol_ok        : Boolean := False with Unreferenced;

      ok  : Boolean := False;
    begin

      init_addresses
        (ip_or_host   =>  Argument (1),
         port         =>  Argument (2),
         ai_socktype  =>  udp,
         ai_family    =>  any,
         addr         => remote_addr);

      init_addresses
        (ip_or_host   =>  "",
         port         =>  "0", -- ignored without 'bind'
         ai_socktype  =>  udp,
         ai_family    =>  get_address_family (remote_addr),
         addr         =>  local_addr);

      if is_null (remote_addr) then

        Text_IO.New_Line;

        Text_IO.Put_Line (" Failed to discover remote host addresses. Quitting.");

        Text_IO.New_Line;

        goto end_app_label1;
      end if;

      if local_addr.all'Length < 1 then

        Text_IO.New_Line;

        Text_IO.Put_Line (" Failed to discover local host addresses. Quitting.");

        Text_IO.New_Line;

        goto end_app_label1;
      end if;

      Text_IO.Put_Line (" Remote host addresses discovered:");

      utils.show_address_and_port (remote_addr);

      Text_IO.New_Line;

      Text_IO.Put_Line (" Host addresses discovered:");

      utils.show_address_and_port (local_addr);

      Text_IO.New_Line;

      if not init_socket (local_sock, local_addr) then

        Text_IO.New_Line;

        Text_IO.Put_Line (" Error while trying initialize socket because: " & string_error & ".");
        Text_IO.Put_Line (" Quiting.");

        goto end_app_label1;
      end if;

      local_addr2 := get_addresses (local_sock);

      Text_IO.New_Line;

      Text_IO.Put_Line (" Host address choosed :=  "  & get_addresses (local_addr2) &
        " and at port := " & get_port (local_addr2));

      Text_IO.New_Line;

      if not init (local_poll) then
        Text_IO.New_Line;

        Text_IO.Put_Line (" Error while trying initialize local poll event: " & string_error & ".");
        Text_IO.Put_Line (" Quiting.");

        goto end_app_label1;
      end if;

      reset_poll_result (local_poll);

      if not add (local_poll, local_sock, send_event) then
        Text_IO.New_Line;

        Text_IO.Put_Line (" Error while trying to add send event in poll event: " & string_error & ".");
        Text_IO.Put_Line (" Quiting.");

        goto end_app_label1;
      end if;

      Text_IO.Put_Line (" Waiting to send messages. ");

      poll_result :=  poll_wait (local_poll, 2500); -- block, 2.5 seconds timeout.

      if poll_result > 0 and then confirm_send_event (local_poll, local_sock) then

        bytes_tmp := sendto (local_sock, remote_addr, buffer);

        if bytes_tmp = socket_error then

          Text_IO.Put_Line (" An error occurred during sending data.");
          Text_IO.Put_Line (" Finishing task.");

          goto end_app_label1;
        end if;

        Text_IO.Put_Line (" Successfull sended " & bytes_tmp'Image & " bytes.");

      else

        Text_IO.Put_Line (" Failed to send to remote host " & get_address_and_port (remote_addr));

        if poll_result = 0 then

          Text_IO.Put_Line (" But it is just only a normal 2.5 seconds time_out");

        else

          if is_hangup_error (local_poll, local_sock) then

            Text_IO.Put_Line (" Remote Host " & get_address_and_port (remote_addr) & " closed the connection. ");

            Text_IO.Put_Line (" Nothing more to do. Quitting.");

            goto end_app_label1;
          end if;
        end if;
      end if;

      Text_IO.New_Line;
      Text_IO.Put_Line (" Waiting to receive message(s). ");

      reset_poll_result (local_poll);

      if not update (local_poll, local_sock, receive_event) then
        Text_IO.New_Line;

        Text_IO.Put_Line (" Error while trying to update socket to receive event in poll event: " & string_error & ".");
        Text_IO.Put_Line (" Quiting.");

        goto end_app_label1;
      end if;

      poll_result := poll_wait (local_poll, 2500); -- block, 2.5 seconds timeout.

      if poll_result > 0 and then confirm_receive_event (local_poll, local_sock) then

        clean (buffer);

        bytes_tmp  := receive_from (local_sock, buffer, remote_addr2); -- block

        if bytes_tmp = socket_error then

          Text_IO.Put_Line (" An error occurred during receiving data.");
          Text_IO.Put_Line (" Finishing task.");

          goto end_app_label1;
        end if;

        if bytes_tmp > 0 then

          Text_IO.Put_Line (" Received message(s) from " & get_address_and_port (remote_addr2));

          Text_IO.Put_Line (" Messages length " & bytes_tmp'Image & " bytes.");

          Text_IO.New_Line;

          Text_IO.Put_Line (" Messages:");

          b2 :
          begin
            loop1 :
            loop

              Text_IO.Put_Line (" |" & String'Input (buffer) & "|");

            end loop loop1;

          exception
            when buffer_insufficient_space_error =>

              Text_IO.New_Line;

              Text_IO.Put_Line (" All messages received from " & get_address_and_port (remote_addr2) & " showed.");

          end b2;

          ok := True;

        else

          Text_IO.Put_Line (" Failed in receive messages from " & get_address_and_port (remote_addr));

          if bytes_tmp = 0 then

            Text_IO.Put_Line ("remote closed the socket");

          elsif bytes_tmp < 0 then

            Text_IO.Put_Line ("With Error => " & string_error);

          end if;
        end if;

        Text_IO.New_Line;

      else

        Text_IO.Put_Line (" Failed in receive messages from " & get_address_and_port (remote_addr));

        if poll_result = 0 then

          Text_IO.Put_Line (" But it is just only a normal 2.5 seconds " & " time_out");

          ok := True;

        else

          if is_hangup_error (local_poll, local_sock) then

            Text_IO.Put_Line (" Remote Host " & get_address_and_port (remote_addr) & " closed the connection. ");

            Text_IO.Put_Line (" Besides reconnect, nothing to do in this case." & " quitting.");

          end if;
        end if;
      end if;

      ok  :=  True;

      <<end_app_label1>>

      if is_initialized (local_poll) then

        pol_ok := close (local_poll);
      end if;

      if initialized (local_sock) then

        close (local_sock);
      end if;

      Text_IO.New_Line;

      Text_IO.Put (" " & Command_Line.Command_Name);

      if ok then
        Text_IO.Put (" successfull ");
      else
        Text_IO.Put (" unsuccess ");
      end if;

      Text_IO.Put_Line ("finalized.");

      Text_IO.New_Line;
    end b1;
  end b0;

  stop_adare_net;

end udp_client_new;
