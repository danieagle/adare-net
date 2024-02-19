  separate (adare_net.base)

  function receive_stream
    (sock : aliased socket;
     data_to_receive  : aliased out stream_element_array_access;
     received_address : aliased out socket_address;
     receive_count    : aliased out ssize_t;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;

    proto : constant Address_type_label :=  a_type_label (sock.storage.socktype);

    tmp_received_address  : aliased socket_address :=
      (if proto = tcp then null_socket_address else sock.storage);

    received_length : ssize_t :=  0;
    total_received  : ssize_t :=  0;

    receive_data    : stream_element_array_access :=  new Stream_Element_Array'(1 .. (2**16 + 5) * 3 => 0);
    pos             : Stream_Element_Offset       := receive_data.all'First;

    socket_address_length : socklen_t             :=  storage_size;

    poll          : aliased poll_of_events;
  begin

    receive_count     :=  0;
    received_address  :=  null_socket_address;
    data_to_receive   :=  null;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      if not set_receive (poll, sock) then
        return False;
      end if;

      if miliseconds_start_timeout > 0  then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_receive (poll, sock)) then
          return False;
        end if;
      end if;
    end if;

    loop1 :
    loop

      case proto is
        when tcp =>

          received_length := ssize_t (inner_recv (sock.sock, receive_data.all (pos)'Address,
            receive_data.all'Length - size_t (pos), 0));

        when udp =>

          received_length :=  ssize_t (inner_recvfrom (sock.sock, receive_data.all (pos)'Address,
            receive_data.all'Length - size_t (pos), 0,
            tmp_received_address.storage'Address, socket_address_length));

          tmp_received_address.addr_length := socket_address_length;

          socket_address_length :=  storage_size;

      end case;

      exit loop1 when received_length < 1 or else received_length = socket_error;

      total_received  :=  total_received  + received_length;

      pos :=  pos + Stream_Element_Offset (received_length);

      if pos + ((2**16 + 5) * 2) > receive_data.all'Length then
        b1 :
        declare
          receive_data_old  : constant Stream_Element_Array := receive_data.all (1 .. pos - 1);
        begin

          receive_data :=  new Stream_Element_Array'(1 .. receive_data_old'Length + ((2**16 + 5) * 3) => 0);
          receive_data.all (receive_data_old'Range) := receive_data_old;
        end b1;
      end if;

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

    data_to_receive := new Stream_Element_Array'(receive_data.all (1 .. Stream_Element_Offset (total_received)));
    receive_count := total_received;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      close (poll);
    end if;

    return True;
  end receive_stream;
