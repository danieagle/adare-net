
-- This is an over simplified example of tcp client with Adare_net, :-)
-- but is yet up to you create a real world champion software with Adare_net and you can do it!! ^^

-- Info about this software:
-- Tcp client with Adare_net example. It work in pair with tcp server

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

procedure tcp_client_new
is
begin

  start_adare_net;

  if Argument_Count < 4 then

    Text_IO.New_Line;

    Text_IO.Put_Line (" Usage: " & Command_Name & " host port ""message1"" ""message2"" ""message_$n"" ");
    Text_IO.New_Line;
    Text_IO.Put_Line (" Minimum of 2 messages ");
    Text_IO.New_Line (2);
    Text_IO.Put_Line (" It will also show that 'buffer' can be read and written offline ");

    Text_IO.New_Line;

    Set_Exit_Status (Failure);

    stop_adare_net;

    return;
  end if;

  Text_IO.New_Line;

  b0 :
  declare
    buffer  : constant socket_buffer_access := new socket_buffer;
    ok      : Boolean := False;
  begin
    clean (buffer);

    for qtd in 3 .. Argument_Count loop
      String'Output (buffer, Argument (qtd)); -- automatic conversion
    end loop;

    b1 :
    declare
      remote_addr   : addresses_list_access :=  null;
      choosed_addr  : addresses_access :=  null;
      host_sock     : socket_access := null;

      bytes_tmp     : ssize_t :=  0;

      host_poll     : epoll_access := null;

      mi_poll_ok    : Boolean := False
        with Unreferenced;

      result_from_poll : int := 0;
    begin
      init_addresses
        (ip_or_host    =>  Argument (1),
         port         =>  Argument (2),
         ai_socktype  =>  tcp,
         ai_family    =>  any,
         addr         =>  remote_addr);

      if remote_addr.all'Length < 1 then

        Text_IO.New_Line;
        Text_IO.Put_Line (" Failed to discover remote host addresses.");
        Text_IO.Put_Line (" Quitting.");
        Text_IO.New_Line;

        goto end_app_label1;
      end if;

      Text_IO.Put_Line (" Remote host addresses discovered:");

      utils.show_address_and_port (remote_addr);

      if not init_socket (host_sock, remote_addr) then

        Text_IO.New_Line;
        Text_IO.Put_Line (" Error while trying initialize socket:");
        Text_IO.Put_Line (" " & string_error);
        Text_IO.Put_Line (" Quiting.");

        goto end_app_label1;
      end if;

      if not connect (host_sock) then

        Text_IO.New_Line;
        Text_IO.Put_Line (" Error while trying connect to remote host:");
        Text_IO.Put_Line (" " & string_error);
        Text_IO.Put_Line (" Quiting.");

        goto end_app_label1;
      end if;

      choosed_addr := get_addresses (host_sock);

      Text_IO.Put_Line (" Connected at address :=  "  & get_addresses (choosed_addr) &
        " and at port := " & get_port (choosed_addr));

      Text_IO.New_Line;

      Text_IO.Put_Line (" Waiting to send messages. ");


      if not init (host_poll) then
        Text_IO.Put_Line (" Failed to init event poll. terminating.");

        goto end_app_label1;
      end if;


      if not add (host_poll, host_sock, send_event) then
        Text_IO.Put_Line (" Failed to config send event. terminating.");

        goto end_app_label1;
      end if;

      Text_IO.Put_Line (" starting 2.5 seconds to send to server. ");

      result_from_poll := poll_wait (host_poll, 2500); -- block, 2.5 seconds timeout.

      if result_from_poll = 0 then
        Text_IO.Put_Line (" 2.5 seconds timeout without send event.");
        Text_IO.Put_Line (" terminating. ");

        goto end_app_label1;

      end if;

      if result_from_poll < 0 then
        Text_IO.Put_Line (string_error);
        Text_IO.Put_Line (" terminating. ");

        goto end_app_label1;

      end if;

      if not confirm_send_event (host_poll, host_sock) then
        Text_IO.Put_Line (" 2.5 seconds timeout without send event for host socket.");
        Text_IO.Put_Line (" terminating. ");

        goto end_app_label1;

      end if;

      bytes_tmp := send (host_sock, buffer);

      if bytes_tmp = socket_error then

        Text_IO.Put_Line (" An error occurred during sending data.");
        Text_IO.Put_Line (" Finishing task.");

        goto end_app_label1;
      end if;

      if bytes_tmp < 1 then

        Text_IO.Put_Line (" Failed in send messages to " & get_address_and_port (choosed_addr));

        if bytes_tmp = 0 then

          Text_IO.Put_Line ("remote closed the socket");

        else

          Text_IO.Put_Line ("With Error => " & string_error);

        end if;

        goto end_app_label1;

      end if;

      Text_IO.Put_Line (" Successfull sended " & bytes_tmp'Image & " bytes.");

      Text_IO.New_Line;

      reset_poll_result (host_poll);

      if not update (host_poll, host_sock, receive_event) then
        Text_IO.Put_Line (" failed to update to receive event.");
        Text_IO.Put_Line (" terminating.");
        goto end_app_label1;
      end if;

      Text_IO.Put_Line (" Waiting 2 seconds to receive message(s). ");

      result_from_poll := poll_wait (host_poll, 2000); -- block, 5 seconds timout

      if result_from_poll = 0 then
        Text_IO.Put_Line (" 2 seconds timeout without host socket receive event. bye! :-)");

        goto end_app_label1;
      end if;

      if result_from_poll < 0 then
        Text_IO.Put_Line (" 2 seconds timeout, with error: " & string_error);

        goto end_app_label1;
      end if;

      if not confirm_receive_event (host_poll, host_sock) then
        Text_IO.Put_Line (" 2 _ seconds without host socket receive event. bye! :-)");

        goto end_app_label1;
      end if;

      bytes_tmp  := receive (host_sock, buffer); -- block

      if bytes_tmp = socket_error or else bytes_tmp < 0 then

        Text_IO.Put_Line (" An error occurred during receiving data.");
        Text_IO.Put_Line (" Finishing task.");

        goto end_app_label1;
      end if;

      if bytes_tmp = 0 then

        Text_IO.Put_Line (" nothing received. bye! :-) ");
        Text_IO.Put_Line (" Finishing task.");

        goto end_app_label1;
      end if;

      Text_IO.Put_Line (" Received message(s) from " & get_address_and_port (choosed_addr));

      Text_IO.Put_Line (" Messages length " & bytes_tmp'Image & " bytes.");

      Text_IO.New_Line;

      Text_IO.Put_Line (" Messages:");

      b2 :
      begin

        loop3 :
        loop

          Text_IO.Put_Line (" |" & String'Input (buffer) & "|");

        end loop loop3;

      exception
        when buffer_insufficient_space_error =>

          Text_IO.New_Line;
          Text_IO.Put_Line (" All messages received from " & get_address_and_port (choosed_addr) & " showed.");
      end b2;

      ok := True;

      <<end_app_label1>>

      if is_initialized (host_poll) then
        mi_poll_ok := close (host_poll);
      end if;

      if initialized (host_sock) then
        close (host_sock);
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

end tcp_client_new;
