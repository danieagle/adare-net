
  separate (adare_net.base)

  function receive_buffer_with_timeout
    (sock : aliased socket;
      data_to_receive : aliased in out socket_buffer;
      received_address : aliased out socket_address;
      miliseconds_timeout : Unsigned_32
    ) return Interfaces.C.int
  is

    received_length : ssize_t :=  0;
    total_received  : ssize_t :=  0;

    proto           : constant Address_type_label :=  a_type_label (sock.storage.socktype);

    receive_data          : Stream_Element_Array  :=  (1 .. (2**16 + 5) * 3 => 0);
    receive_data_address  : constant Address :=  receive_data (1)'Address;
    receive_data_length   : constant size_t  :=  receive_data'Length;

    socket_address_length : socklen_t :=  storage_size;

    tmp_received_address  : aliased socket_address :=
      (if proto = tcp then null_socket_address else sock.storage);

    tmp_received_address_test  : aliased socket_address :=  tmp_received_address;

    tmp_address_test  : Boolean :=  False;

    tmp_storage_union0 : storage_union := (others => <>);
    tmp_storage_union1 : storage_union := (others => <>);
  begin

    received_address := null_socket_address;

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
          External_Name   =>  "adare_epoll_epollin";

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

        -- Done. :)
        acc := inner_epoll_ctl (mi_epoll_handle, Interfaces.C.int (cmd_del), sock.sock, Null_Address);
        acc := inner_epoll_close (mi_epoll_handle);

      end b01;
    end b0;

    loop1 :
    loop

      case proto is
        when tcp =>

          received_length := ssize_t (inner_recv (sock.sock, receive_data_address,
            receive_data_length, 0));

        when udp =>

          received_length :=  ssize_t (inner_recvfrom (sock.sock, receive_data_address, receive_data_length, 0,
            tmp_received_address.storage'Address, socket_address_length));

          tmp_received_address.addr_length := socket_address_length;

          socket_address_length :=  storage_size;

      end case;


      exit loop1 when received_length < 1 or else received_length = socket_error;

      total_received  :=  total_received  + received_length;

      Stream_Element_Array'Write (data_to_receive'Access, receive_data (1 .. Stream_Element_Offset (received_length)));


      case proto is
        when tcp =>

          received_length := ssize_t (inner_recv (sock.sock, receive_data_address,
            receive_data_length, msg_peek_flag));

        when udp =>

          received_length :=  ssize_t (inner_recvfrom (sock.sock, receive_data_address, receive_data_length, msg_peek_flag,
            tmp_received_address_test.storage'Address, socket_address_length));

          tmp_received_address_test.addr_length := socket_address_length;

          socket_address_length :=  storage_size;

          tmp_storage_union0.ss := tmp_received_address.storage;
          tmp_storage_union1.ss := tmp_received_address_test.storage;

          tmp_address_test  :=
            (tmp_storage_union0.ss.ss_family /= tmp_storage_union1.ss.ss_family) or else
            (if family_label (Address_family (tmp_storage_union0.ss.ss_family)) = ipv4 then
            (tmp_storage_union0.i4.sin_addr /= tmp_storage_union1.i4.sin_addr) else
            (tmp_storage_union0.i6.sin6_addr /= tmp_storage_union1.i6.sin6_addr));

          exit loop1 when tmp_address_test;

      end case;

      exit loop1 when received_length < 1 or else received_length = socket_error;

    end loop loop1;

    if proto = udp then
      received_address := tmp_received_address;
    end if;

    return Interfaces.C.int (total_received);
  end receive_buffer_with_timeout;
