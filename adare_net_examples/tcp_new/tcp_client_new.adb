
-- This is an over simplified example of tcp client with Adare_net, :-)
-- but is yet up to you create a real world champion software with Adare_net and you can do it!! ^^

-- Info about this software:
-- Tcp client with Adare_net example. It work in pair with tcp server

with Ada.Command_Line;
with Ada.Text_IO;
use Ada, Ada.Command_Line;

with adare_net.base.waits;  use adare_net.base;  use adare_net.base.waits;
with adare_net_init;  use adare_net_init;
with adare_net_exceptions;  use adare_net_exceptions;

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
    buffer  : aliased socket_buffer;
    ok      : Boolean := False;
  begin
    clear (buffer);

    for qtd in 3 .. Argument_Count loop
      String'Output (buffer'Access, Argument (qtd)); -- automatic conversion
    end loop;

    b1 :
    declare
      remote_addr   : aliased socket_addresses;
      choosed_addr  : aliased socket_address  :=  null_socket_address;
      rcv_addr      : aliased socket_address  :=  null_socket_address;
      host_sock     : aliased socket          :=  null_socket;

      bytes_tmp     : ssize_t :=  0;

      host_poll     : aliased poll_of_events;

      mi_poll_ok    : Boolean := False
        with Unreferenced;

      result_from_poll : int := 0;

      tmp_msg : stream_element_array_access := null;
    begin
      if not create_address
        (host_or_ip   =>  Argument (1), network_port_or_service  =>  Argument (2),
        Addr_family   =>  any,  Addr_type =>  tcp,  response  =>  remote_addr)
      then

        Text_IO.New_Line;
        Text_IO.Put_Line (" Failed to discover remote host addresses.");
        Text_IO.Put_Line (" Quitting.");
        Text_IO.New_Line;

        goto end_app_label1;
      end if;


      Text_IO.Put_Line (" Remote host addresses discovered:");

      while get_address (remote_addr, choosed_addr) loop
        Text_IO.Put_Line (" address => " & get_address (choosed_addr) & " and port => " & get_address_port (choosed_addr));
        Text_IO.New_Line;
      end loop;


      if not create_socket  (remote_addr, host_sock) then

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

      choosed_addr := get_address (host_sock);

      Text_IO.Put_Line (" Connected at address :=  "  & get_address (choosed_addr) &
        " and at port := " & get_address_port (choosed_addr));

      Text_IO.New_Line;

      Text_IO.Put_Line (" Waiting to send messages. ");


      if not set_send (host_poll, host_sock) then
        Text_IO.Put_Line (" Failed to init event poll. terminating.");

        goto end_app_label1;
      end if;

      Text_IO.Put_Line (" starting 2.5 seconds to send to server. ");

      if not (poll_wait (host_poll, 2500) and then is_send (host_poll, host_sock)) then
        Text_IO.Put_Line (" 2.5 seconds timeout without send event.");
        Text_IO.Put_Line (" terminating. ");

        goto end_app_label1;
      end if;

      bytes_tmp := ssize_t (send_buffer (host_sock, buffer));

      if bytes_tmp = socket_error then

        Text_IO.Put_Line (" An error occurred during sending data.");
        Text_IO.Put_Line (" Finishing task.");

        goto end_app_label1;
      end if;

      if bytes_tmp < 1 then

        Text_IO.Put_Line (" Failed in send messages to " & get_address (choosed_addr) &
        " and at port := " & get_address_port (choosed_addr));

        if bytes_tmp = 0 then

          Text_IO.Put_Line ("remote closed the socket");

        else

          Text_IO.Put_Line ("With Error => " & string_error);

        end if;

        goto end_app_label1;

      end if;

      Text_IO.Put_Line (" Successfull sended " & bytes_tmp'Image & " bytes.");

      Text_IO.New_Line;

      reset_results (host_poll);

      if not set_receive (host_poll, host_sock) then

        Text_IO.Put_Line (" failed to update to receive event.");
        Text_IO.Put_Line (" terminating.");
        goto end_app_label1;
      end if;

      Text_IO.Put_Line (" Waiting 5 seconds to receive message(s). ");

      if not (poll_wait (host_poll, 5000) and then is_receive (host_poll, host_sock)) then
        Text_IO.Put_Line (" 2.5 seconds timeout without receive event.");
        Text_IO.Put_Line (" terminating. ");

        goto end_app_label1;
      end if;

      bytes_tmp  := ssize_t (receive_buffer (host_sock, buffer, rcv_addr)); -- block

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

      Text_IO.Put_Line (" Received message(s) from " & get_address (choosed_addr) &
        " and at port := " & get_address_port (choosed_addr));

      Text_IO.Put_Line (" Messages length " & bytes_tmp'Image & " bytes.");

      Text_IO.New_Line;

      Text_IO.Put_Line (" Messages:");

      b2 :
      begin

        loop3 :
        loop

          Text_IO.Put_Line (" |" & String'Input (buffer'Access) & "|");

        end loop loop3;

      exception
        when buffer_insufficient_space_error =>

          Text_IO.New_Line;
          Text_IO.Put_Line (" All messages received from " & get_address (choosed_addr) &
        " and at port := " & get_address_port (choosed_addr) & " showed.");
      end b2;

      ok := True;

      <<end_app_label1>>

      if is_initialized (host_poll) then
        close (host_poll);
      end if;

      if is_initialized (host_sock) then
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
