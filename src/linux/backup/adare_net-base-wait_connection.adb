
  separate (adare_net.base)

  function wait_connection
    (sock           : aliased in out socket;
     response       : out socket;
     data_received  : aliased out stream_element_array_access;
     miliseconds_start_timeout  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;

    mi_response     : socket    := sock;
    mi_storage_size : socklen_t := storage_size;
    proto           : constant Address_type_label := a_type_label (sock.storage.socktype);
    poll            : aliased poll_of_events;
  begin

    response      :=  null_socket;
    data_received := null;

    if not sock.listened then
      return False;
    end if;

    if miliseconds_start_timeout > 0  then
      if not set_receive (poll, sock) then
        return False;
      end if;

      if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_receive (poll, sock)) then
        close (poll);

        return False;
      end if;

      close (poll);
    end if;

    if proto = tcp then
      mi_response.sock := inner_accept (sock.sock,
        mi_response.storage.storage'Address, mi_storage_size'Address);

      if mi_response.sock = invalid_socket then
        return False;
      end if;

      mi_response.storage.addr_length :=  mi_storage_size;

      mi_response.connected :=  True;
      mi_response.binded    :=  False;
      mi_response.listened  :=  False;

      response  := mi_response;

      return True;
    end if;

    if proto = udp then
      b1 :
      declare
        data_tmp  : Stream_Element_Array := (1 .. (2**16 + 5) * 3 => 0);
        len       : ssize_t;
        len_tmp   : aliased socklen_t := storage_size;
        acc       : Interfaces.C.int := 0
          with Unreferenced;

        mi_socket_fd  : socket_type := 0;
      begin

        len :=  inner_recvfrom (sock.sock, data_tmp'Address, data_tmp'Length, 0,
          mi_response.storage.storage'Address, len_tmp'Address);

        if len = socket_error or else len < 1 then
          return False;
        end if;

        mi_response.storage.addr_length := len_tmp;

        mi_socket_fd :=
          inner_socket (int (mi_response.storage.storage.ss_family),
            Interfaces.C.int (mi_response.storage.socktype),
            Interfaces.C.int (mi_response.storage.protocol));

        if mi_socket_fd = invalid_socket then
          return False;
        end if;

        mi_response.sock := mi_socket_fd;

        data_received := new Stream_Element_Array'(data_tmp (1 .. Ada.Streams.Stream_Element_Offset (len)));

        response := mi_response;

        return True;

      end b1;
    end if;

    return False;
  end wait_connection;
