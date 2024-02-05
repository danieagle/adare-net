

  separate (adare_net.base)
  function send_stream_with_timeout
    (sock : aliased socket;
     data_to_send : aliased Stream_Element_Array;
     miliseconds_timeout : Unsigned_32
    ) return Interfaces.C.int
  is
    pos           : Stream_Element_Offset :=  data_to_send'First;
    remaining     : ssize_t :=  data_to_send'Length;
    sended_length : ssize_t :=  0;
    total_sended  : ssize_t :=  0;
    proto         : constant Address_type_label :=  a_type_label (sock.storage.socktype);
  begin
    if remaining = 0 then
      return 0;
    end if;

    b0 :
    declare
      mi_epoll_handle : constant handle_type := inner_epoll_create1;

    begin

      if mi_epoll_handle = failed_handle then
        return 0;
      end if;

      b01 :
      declare
        mi_socket_event : constant unsigned_long
          with Convention => C, import,
          External_Name   =>  "adare_epoll_epollout";

        event           : epoll_event := (others => <>);

        event_response  : array (1 .. 2) of epoll_event := (others => <>);

        acc   : Interfaces.C.int := 0
          with Unreferenced;

      begin

        event.events  := Unsigned_32 (mi_socket_event);
        event.data.fd := Interfaces.C.int (sock.sock);

        if 0 /= inner_epoll_ctl (mi_epoll_handle, Interfaces.C.int (cmd_add), sock.sock, event'Address) then
          acc := inner_epoll_close (mi_epoll_handle);

          return 0;
        end if;

        if inner_epoll_wait (mi_epoll_handle, event_response'Address, 1,
            Interfaces.C.int (miliseconds_timeout)) < 1
        then
          acc := inner_epoll_ctl (mi_epoll_handle, Interfaces.C.int (cmd_del), sock.sock, Null_Address);
          acc := inner_epoll_close (mi_epoll_handle);

          return 0;
        end if;

        acc := inner_epoll_ctl (mi_epoll_handle, Interfaces.C.int (cmd_del), sock.sock, Null_Address);
        acc := inner_epoll_close (mi_epoll_handle);

      end b01;
    end b0;

    loop1 :
    loop
      case proto is

        when tcp =>

          sended_length := ssize_t (inner_send (sock.sock, data_to_send (pos)'Address,
            size_t (remaining), 0));

        when udp =>

          sended_length := ssize_t (inner_sendto (sock.sock, data_to_send (pos)'Address,
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

    end loop loop1;

    return Interfaces.C.int (total_sended);

  end send_stream_with_timeout;
