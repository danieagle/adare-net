
-- This is an over simplified, but complete enough, example of tcp client with Adare_net, :-)
-- but is yet up to you create a real world champion software with Adare_net
-- and you can do it!! ^^

-- Info about this software:
-- Tcp client with Adare_net example. It work in pair with tcp server

with Ada.Command_Line;
with Ada.Text_IO;
use Ada, Ada.Command_Line;

with adare_net.base;  use adare_net.base;
with adare_net_init;  use adare_net_init;
with adare_net_exceptions;  use adare_net_exceptions;

with Interfaces.C; use Interfaces, Interfaces.C;

procedure tcp_client_new
is
  pragma Unsuppress (All_Checks); -- just to testing, optional in production code.
begin

  start_adare_net;

  if Argument_Count < 4 then

    Text_IO.New_Line;

    Text_IO.Put_Line (" Usage: " & Command_Name &
      " host port ""message1"" ""message2"" ""message_$n"" ");
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
    clear (buffer); -- optional

    for qtd in 3 .. Argument_Count loop
      String'Output (buffer, Argument (qtd)); -- automatic conversion
    end loop;

    b1 :
    declare
      remote_addr   : socket_addresses_access;
      choosed_addr  : socket_address_access;
      rcv_addr      : socket_address_access;
      host_sock     : socket_access;

      bytes_tmp     : int :=  0;

    begin
      if not create_addresses
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
        Text_IO.Put_Line ("type => " & get_address_type (choosed_addr) &
          " , family_type => " & get_family_label (choosed_addr) &
          " , address => " & get_address (choosed_addr) &
          " , and port => " & get_address_port (choosed_addr));

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

      get_address (host_sock, choosed_addr);

      Text_IO.Put_Line ("type => " & get_address_type (choosed_addr) &
        " , family_type => " & get_family_label (choosed_addr) &
        " Connected at address :=  "  & get_address (choosed_addr) &
        " and at port := " & get_address_port (choosed_addr));

      Text_IO.New_Line;

      Text_IO.Put_Line (" Waiting until 2 seconds to start sending messages. ");
      Text_IO.Put_Line (" with until 0,5 seconds between sending remaining messages.  ");

      Text_IO.Put_Line (" buffer size  " & Integer_64'(actual_data_size (buffer))'Image);

      if not send_buffer  (sock => host_sock,
        data_to_send  =>  buffer,
        send_count  =>  bytes_tmp,
        miliseconds_start_timeout =>  2000,
        miliseconds_next_timeouts =>  500) or else bytes_tmp < 1
      then
        Text_IO.Put_Line (" An error occurred while sending data to remote server.");
        Text_IO.Put_Line (" Nothing to do.");
        Text_IO.Put_Line (" Last error message => " & string_error);
        Text_IO.Put_Line (" Finishing.");

        goto end_app_label1;
      end if;

      Text_IO.Put_Line (" Successfull sended " & bytes_tmp'Image & " bytes.");

      Text_IO.New_Line;

      Text_IO.Put_Line (" Waiting until 5 seconds to receive message(s). ");
      Text_IO.Put_Line (" with until 0,5 seconds between receive remaining messages. ");

      if not receive_buffer (sock => host_sock,
        data_to_receive =>  buffer,
        received_address  =>  rcv_addr,
        receive_count =>  bytes_tmp,
        miliseconds_start_timeout =>  5000,
        miliseconds_next_timeouts =>  500) or else bytes_tmp < 1
      then
        Text_IO.Put_Line (" An error occurred while receiving or the length of " &
          "message received is zero.");
        Text_IO.Put_Line (" Nothing to do.");
        Text_IO.Put_Line (" Last error message => " & string_error);
        Text_IO.Put_Line (" Finishing.");

        goto end_app_label1;
      end if;

      Text_IO.Put_Line (" Received message(s) from " & get_address (choosed_addr) &
        " and at port := " & get_address_port (choosed_addr) &
        " , type => " & get_address_type (choosed_addr) &
        " , family type => " & get_family_label (choosed_addr));

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
          Text_IO.Put_Line (" All messages received from " & get_address (choosed_addr) &
            " and at port := " & get_address_port (choosed_addr) &
            " and type => " & get_address_type (choosed_addr) &
            " and family type => " & get_family_label (choosed_addr) & " showed.");
      end b2;

      ok := True;

      <<end_app_label1>>

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
