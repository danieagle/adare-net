
separate (adare_net.base)
function receive_buffer
 (sock  : aliased socket;
  data_to_receive   : aliased in out socket_buffer;
  received_address  : aliased out socket_address;
  receive_count     : aliased out ssize_t;
  miliseconds_start_timeout : Unsigned_32 :=  0;
  miliseconds_next_timeouts : Unsigned_32 :=  0
) return Boolean
is
  use adare_net.base.waits;

  received_length : ssize_t := 0;
  total_received  : ssize_t := 0;

  proto : constant Address_type_label := a_type_label (sock.storage.socktype);

  receive_data         : Stream_Element_Array := (1 .. (2**16 + 5) * 3 => 0);
  receive_data_address : constant Address     := receive_data (1)'Address;
  receive_data_length  : constant size_t      := receive_data'Length;

  socket_address_length : Interfaces.C.int  :=  Interfaces.C.int (storage_size);

  tmp_received_address : aliased socket_address :=
   (if proto = tcp then null_socket_address else sock.storage);

  poll : aliased poll_of_events;
begin

  receive_count    := 0;
  received_address := null_socket_address;

  if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
    if not set_receive (poll, sock) then
      return False;
    end if;

    if miliseconds_start_timeout > 0 then
      if not
       (poll_wait (poll, int (miliseconds_start_timeout))
        and then is_receive (poll, sock))
      then
        return False;
      end if;
    end if;
  end if;

  loop1 :
  loop

    case proto is
      when tcp =>

        received_length :=  ssize_t (inner_recv (sock.sock, receive_data_address,
          Interfaces.C.int (receive_data_length), 0));

      when udp =>

        received_length :=  ssize_t (inner_recvfrom (sock.sock, receive_data_address,
          Interfaces.C.int (receive_data_length), 0,
             tmp_received_address.storage'Address, socket_address_length'Address));

        tmp_received_address.addr_length := socklen_t (socket_address_length);

        socket_address_length := Interfaces.C.int (storage_size);

    end case;

    exit loop1 when received_length < 1 or else received_length = socket_error;

    total_received := total_received + received_length;

    Stream_Element_Array'Write
     (data_to_receive'Access,
      receive_data (1 .. Stream_Element_Offset (received_length)));

    if miliseconds_next_timeouts > 0 then
      reset_results (poll);

      if not (poll_wait (poll, int (miliseconds_next_timeouts)) and then is_receive (poll, sock)) then
        exit loop1;
      end if;
    end if;

  end loop loop1;

  if proto = udp then
    received_address := tmp_received_address;
  end if;

  receive_count := total_received;

  if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
    close (poll);
  end if;

  return True;
end receive_buffer;
