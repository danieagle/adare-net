
-- This is an over simplified example of tcp client with Adare_net, :-)
-- but is yet up to you create a real world champion software with Adare_net and you can do it!! ^^

-- Info about this software:
-- Tcp client with Adare_net example. It work in pair with server1

with Ada.Command_Line;
with Ada.Text_IO;
use Ada, Ada.Command_Line;

with adare_net.sockets.polls;
with adare_net.sockets.utils;
use adare_net.sockets;

with adare_net_exceptions;
use adare_net_exceptions;

with adare_net_init;
use  adare_net_init;

with socket_types;
use socket_types;

with Interfaces.C;
use Interfaces.C;

procedure client1
is
begin

  start_adare_net;

  if Argument_Count < 4 then

    Text_IO.New_Line;

    Text_IO.Put_Line (" Usage: " & Command_Name & " host port ""message1"" ""message2"" ""message_$n"" ");
    Text_IO.New_Line;
    Text_IO.Put_Line (" Minimum of 2 messages ");
    Text_IO.New_Line (2);
    Text_IO.Put_Line (" It will also show that 'buffer' can be readed and written offline ");

    Text_IO.New_Line;

    Set_Exit_Status (Failure);

    stop_adare_net;

    return;
  end if;

  Text_IO.New_Line;

  b0 :
  declare
    buffer  : aliased socket_buffer;
  begin

    for qtd in 3 .. Argument_Count loop
      String'Output (buffer'Access, Argument (qtd)); -- automatic conversion
    end loop;

    b1 :
    declare
      remote_addr  : aliased addresses_list :=
        init_addresses
          (ip_or_host   =>  Argument (1),
           port         =>  Argument (2),
           ai_socktype  =>  tcp,
           ai_family    =>  any
          );

      ok  :  Boolean := False;

      host_sock             : aliased socket;
      choosed_remote_addr   : aliased addresses;

      bytes_tmp : ssize_t :=  0;

      host_poll   : aliased polls.poll_type (2);

      result_from_poll : int := 0;

    begin
      if remote_addr'Length < 1 then

        Text_IO.New_Line;
        Text_IO.Put_Line (" Failed to discover remote host addresses.");
        Text_IO.Put_Line (" Quitting.");
        Text_IO.New_Line;

        goto end_app_label1;
      end if;

      Text_IO.Put_Line (" Remote host addresses discovered:");

      utils.show_address_and_port (remote_addr'Access);

      if not init_socket (host_sock, remote_addr'Access) then

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

      choosed_remote_addr  :=  get_addresses (host_sock);

      Text_IO.New_Line;

      Text_IO.Put_Line (" Connected at address :=  "  & get_addresses (choosed_remote_addr'Access) &
        " and at port := " & get_port (choosed_remote_addr'Access));

      Text_IO.New_Line;

      Text_IO.Put_Line (" Waiting to send messages. ");

      polls.add_events (host_poll'Access, host_sock'Access, polls.send_ev);

      result_from_poll := polls.start_events_listen (host_poll'Access, 2500); -- block, 2.5 seconds timeout.

      if result_from_poll > 0 then

        bytes_tmp := send (host_sock, buffer);

        if bytes_tmp > 0 then

          Text_IO.Put_Line (" Successfull sended " & bytes_tmp'Image & " bytes.");

        else

          Text_IO.Put_Line (" Failed in send messages to " & get_address_and_port (choosed_remote_addr'Access));

          if bytes_tmp = 0 then

            Text_IO.Put_Line ("remote closed the socket");

          else

            Text_IO.Put_Line ("With Error => " & string_error);

          end if;
        end if;

      else

        Text_IO.Put_Line (" Failed to send to remote host " & get_address_and_port (choosed_remote_addr'Access));

        if result_from_poll = 0 then

          Text_IO.Put_Line (" But it is just only a normal 2.5 seconds time_out");

        else

          if polls.hang_up_error (host_poll'Access, host_sock'Access) then

            Text_IO.Put_Line (" Remote Host " & get_address_and_port (choosed_remote_addr'Access) & " closed the connection. ");

            Text_IO.Put_Line (" Nothing more to do. Quitting.");

            goto end_app_label1;

          end if;
        end if;
      end if;

      Text_IO.New_Line;

      Text_IO.Put_Line (" Waiting to receive message(s). ");

      polls.update (host_poll'Access, host_sock'Access, polls.receive_ev);

      result_from_poll := polls.start_events_listen (host_poll'Access, 2500); --block, 2.5 seconds timout

      if result_from_poll > 0 then

        bytes_tmp  := receive (host_sock, buffer); -- block

        if bytes_tmp > 0 then

          Text_IO.Put_Line (" Received message(s) from " & get_address_and_port (choosed_remote_addr'Access));

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
              Text_IO.Put_Line (" All messages received from " & get_address_and_port (choosed_remote_addr'Access) & " showed.");
          end b2;

          ok := True;

        else

          Text_IO.Put_Line (" Failed in receive messages from " & get_address_and_port (choosed_remote_addr'Access));

          if bytes_tmp = 0 then

            Text_IO.Put_Line ("remote host closed the socket");

          else

            Text_IO.Put_Line ("With Error => " & string_error);

          end if;

        end if;

      else

        Text_IO.Put_Line (" Failed in receive messages from " & get_address_and_port (choosed_remote_addr'Access));

        if result_from_poll = 0 then

          Text_IO.Put_Line (" But it is just only a normal 2.5 seconds " & " time_out");

          ok := True;

        else

          if polls.hang_up_error (host_poll'Access, host_sock'Access) then

            Text_IO.Put_Line (" Remote Host " & get_address_and_port (choosed_remote_addr'Access) & " closed the connection. ");

            Text_IO.Put_Line (" Besides reconnect, nothing to do in this case." & " quitting.");

          end if;
        end if;
      end if;

      <<end_app_label1>>

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

end client1;
