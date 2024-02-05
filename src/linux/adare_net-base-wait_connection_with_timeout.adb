

  separate (adare_net.base)
  function wait_connection_with_timeout
    (sock : aliased in out socket;
     miliseconds_timeout  : Unsigned_32;
     data_received  : aliased out stream_element_array_access;
     backlog  : Unsigned_16 :=  10) return socket
  is
    mi_response     : socket    := sock;
    mi_storage_size : socklen_t := storage_size;

    mi_epoll_handle : constant handle_type := inner_epoll_create1;

    proto : constant Address_type_label :=  a_type_label (sock.storage.socktype);

    acc   : Interfaces.C.int := 0
      with Unreferenced;
  begin

    data_received := null;

    if mi_epoll_handle = failed_handle then
      return null_socket;
    end if;

    if proto = tcp and then not sock.listened then
      if inner_listen (sock.sock, Interfaces.C.int (backlog)) /= 0  then
        acc := inner_epoll_close (mi_epoll_handle);

        return null_socket;
      end if;

      sock.listened := True;
    end if;

    b0 :
    declare
      mi_socket_event : constant unsigned_long
        with Convention => C, import,
        External_Name   =>  "adare_epoll_epollin";

      event           : epoll_event := (others => <>);

      event_response  : array (1 .. 2) of epoll_event := (others => <>);

    begin

      event.events  := Unsigned_32 (mi_socket_event);
      event.data.fd := Interfaces.C.int (sock.sock);

      if 0 /= inner_epoll_ctl (mi_epoll_handle, Interfaces.C.int (cmd_add), sock.sock, event'Address) then
        acc := inner_epoll_close (mi_epoll_handle);

        return null_socket;
      end if;

      if inner_epoll_wait (mi_epoll_handle, event_response'Address, 1,
          Interfaces.C.int (miliseconds_timeout)) < 1
      then
        acc := inner_epoll_ctl (mi_epoll_handle, Interfaces.C.int (cmd_del), sock.sock, Null_Address);
        acc := inner_epoll_close (mi_epoll_handle);

        return null_socket;
      end if;

      -- done :)
      acc := inner_epoll_ctl (mi_epoll_handle, Interfaces.C.int (cmd_del), sock.sock, Null_Address);
      acc := inner_epoll_close (mi_epoll_handle);


      if proto = tcp then

        mi_response.sock := inner_accept (sock.sock,
          mi_response.storage.storage'Address,
          mi_storage_size);

        if mi_response.sock = invalid_socket then
          return null_socket;
        end if;

        mi_response.storage.addr_length := mi_storage_size;

        mi_response.connected := True;
        mi_response.binded    := False;
        mi_response.listened  := False;

        return mi_response;
      end if;

      if proto = udp then
        b1 :
        declare
          mi_socket_fd  : socket_type := 0;
          data_tmp  : Stream_Element_Array := (1 .. (2**16 + 5) * 3 => 0);
          len_tmp   : aliased socklen_t := storage_size;
          len       : ssize_t;
        begin
          len := ssize_t (inner_recvfrom (sock.sock, data_tmp'Address, data_tmp'Length, 0,
            mi_response.storage.storage'Address, len_tmp));

          if socket_error = len then
            return null_socket;
          end if;

          mi_response.storage.addr_length := len_tmp;

          mi_socket_fd :=
            inner_socket (int (mi_response.storage.storage.ss_family),
              Interfaces.C.int (mi_response.storage.socktype),
              Interfaces.C.int (mi_response.storage.protocol));

          if mi_socket_fd = invalid_socket then
            return null_socket;
          end if;

          mi_response.sock := mi_socket_fd;

          data_received := new Stream_Element_Array'(data_tmp (1 .. Ada.Streams.Stream_Element_Offset (len)));

          return mi_response;
        end b1;
      end if;
    end b0;

    return null_socket;
  end wait_connection_with_timeout;
