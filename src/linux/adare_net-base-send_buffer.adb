  separate (adare_net.base)
  function send_buffer
    (sock : aliased socket;
     data_to_send : aliased in out socket_buffer;
     send_count   : aliased out ssize_t;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;
    pos           : Stream_Element_Offset :=  data_to_send.head_first;
    remaining     : ssize_t :=  ssize_t (actual_data_size (data_to_send));
    sended_length : ssize_t :=  0;
    total_sended  : ssize_t :=  0;
    proto         : constant Address_type_label :=  a_type_label (sock.storage.socktype);
    poll          : aliased poll_of_events;
  begin
    send_count := 0;

    if remaining = 0 then
      return True;
    end if;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      if not set_send (poll, sock) then
        return False;
      end if;

      if miliseconds_start_timeout > 0 then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_send (poll, sock)) then
          return False;
        end if;
      end if;
    end if;

    loop1 :
    loop
      case proto is

        when tcp =>

          sended_length := ssize_t (inner_send (sock.sock, data_to_send.data (pos)'Address,
            size_t (remaining), 0));

        when udp =>

          sended_length := ssize_t (inner_sendto (sock.sock, data_to_send.data (pos)'Address,
            size_t (remaining), 0,
            sock.storage.storage'Address,
            sock.storage.addr_length));

      end case;

      exit loop1 when sended_length < 1 or else sended_length = socket_error;

      pos := pos + Stream_Element_Offset (sended_length);

      total_sended  := total_sended + sended_length;

      exit loop1 when remaining = sended_length;

      remaining :=  remaining - sended_length;

      exit loop1 when remaining < 1;

      if miliseconds_next_timeouts > 0 then
        reset_results (poll);

        if not (poll_wait (poll, int (miliseconds_next_timeouts)) and then is_send (poll, sock)) then
          exit loop1;
        end if;
      end if;

    end loop loop1;

    data_to_send.head_first := data_to_send.head_first + Stream_Element_Count (total_sended);

    send_count := total_sended;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      close (poll);
    end if;

    return True;
  end send_buffer;
