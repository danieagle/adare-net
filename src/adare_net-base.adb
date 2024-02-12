
with adare_net_exceptions;  use adare_net_exceptions;

with adare_net.base.inners; use adare_net.base.inners;

package body adare_net.base
  with Preelaborate
is

  function actual_data_size
    (buffer : not null socket_buffer_access) return Integer_64
  is (if buffer.data /= null then (Integer_64 ((buffer.tail_end + 1) - buffer.head_first)) else 0);

  function actual_data_size
    (buffer : aliased socket_buffer) return Integer_64
  is (if buffer.data /= null then (Integer_64 ((buffer.tail_end + 1) - buffer.head_first)) else 0);


  function max_data_length
    (buffer : aliased socket_buffer) return Stream_Element_Offset
  is (if buffer.data /= null then (buffer.head_first - buffer.data'First) + (buffer.data'Last - buffer.tail_end) else 0);

  function data_tail_length
    (buffer : aliased socket_buffer) return Stream_Element_Offset
  is (if buffer.data /= null then buffer.data'Last - buffer.tail_end else 0);

  function is_initialized
    (sock : aliased in socket) return Boolean
  is (sock.sock /= 0);

  function is_connected
    (sock : aliased in socket) return Boolean
  is (sock.connected);

  function is_binded
    (sock : aliased in socket) return Boolean
  is (sock.binded);

  function is_listened
    (sock : aliased in socket) return Boolean
  is (sock.listened);

  function get_socket
    (sock : aliased in socket) return socket_type
  is (sock.sock);


  function a_type_label (a_type : Address_type := tmp_tcp) return Address_type_label
  is
  begin

    if a_type = tmp_tcp then
      return tcp;
    end if;

    if a_type = tmp_udp then
      return udp;
    end if;

    raise Constraint_Error with " unknown Address_Type ";
  end a_type_label;

  function family_label (a_family : Address_family := tmp_any) return Address_family_label
  is
  begin

    if a_family = tmp_any then
      return any;
    end if;

    if a_family = tmp_ipv4 then
      return ipv4;
    end if;

    if a_family = tmp_ipv6 then
      return ipv6;
    end if;

    raise Constraint_Error with " unknown Address_family ";
  end family_label;

  function storage_size return socklen_t
  is
    tmp_size  : constant socket_address := (others => <>);
  begin
    return tmp_size.storage'Size / 8;
  end storage_size;


  function create_address
    (host_or_ip : String;
     network_port_or_service  : String;
     Addr_family  : Address_family_label;
     Addr_type    : Address_type_label;
     response     : out socket_address) return Boolean
  is
    ai_passive  : constant Interfaces.C.int
      with Import => True, Convention => C, External_Name => "c_ai_passive";

    hint  : aliased constant addr_info
      :=  (ai_flags => (if host_or_ip'Length /= 0 then 0 else ai_passive),
           ai_family => Interfaces.C.int (family (Addr_family)),
           ai_socktype => Interfaces.C.int (a_type (Addr_type)),
           ai_protocol => 0,
           ai_addrlen => 0,
           ai_addr => Null_Address,
           ai_canonname => Null_Address,
           ai_next => Null_Address);

    tmp_getted_response  : Address  := Null_Address;

    tmp_addr  : aliased socket_address := null_socket_address;

    h_or_i    : aliased constant char_array  := To_C (host_or_ip);
    n_p_or_s  : aliased constant char_array  := To_C (network_port_or_service);

    ret_value : constant Interfaces.C.int :=  inner_getaddrinfo
      (host_or_ip_i =>  (if host_or_ip'Length /= 0 then h_or_i'Address else Null_Address),
       port_i       =>  (if network_port_or_service'Length /= 0 then n_p_or_s'Address else Null_Address),
       hints_i      =>  hint'Address,
       response_i   =>  tmp_getted_response'Address); --  just tmp_getted_response without 'Address ?

    tmp_addrinfo : aliased addr_info;

    tmp_sockaddr_storage  : ainfo2.Object_Pointer;

  begin
    response := null_socket_address;

    if ret_value /= 0 or else tmp_getted_response = Null_Address then
      return False;
    end if;

    tmp_addrinfo := To_Pointer (tmp_getted_response).all;
    tmp_sockaddr_storage := To_Pointer (tmp_addrinfo.ai_addr);

    tmp_addr.storage.ss_family := Interfaces.Unsigned_16 (tmp_addrinfo.ai_family);

    if tmp_addrinfo.ai_addrlen > 2 then
      tmp_addr.storage.padding (1 .. size_t (tmp_addrinfo.ai_addrlen - 2))
        := tmp_sockaddr_storage.padding (1 .. size_t (tmp_addrinfo.ai_addrlen - 2));
    else
      tmp_addr.storage.padding := (others => char'Val (0));
    end if;

    tmp_addr.socktype :=  Address_type (tmp_addrinfo.ai_socktype);

    tmp_addr.protocol :=  Address_family (tmp_addrinfo.ai_protocol);

    tmp_addr.addr_length  :=  tmp_addrinfo.ai_addrlen;

    inner_free_addrinfo (tmp_getted_response);

    response := tmp_addr;

    return True;
  end create_address;


  function create_address
    (host_or_ip : String;
     network_port_or_service  : String;
     Addr_family  : Address_family_label;
     Addr_type    : Address_type_label;
     response     : out socket_addresses) return Boolean
  is
    ai_passive  : constant Interfaces.C.int
      with Import => True, Convention => C, External_Name => "c_ai_passive";

    hint  : aliased constant addr_info
      :=  (ai_flags => (if host_or_ip'Length /= 0 then 0 else ai_passive),
           ai_family => Interfaces.C.int (family (Addr_family)),
           ai_socktype => Interfaces.C.int (a_type (Addr_type)),
           ai_protocol => 0,
           ai_addrlen => 0,
           ai_addr => Null_Address,
           ai_canonname => Null_Address,
           ai_next => Null_Address);

    tmp_getted_response : Address  := Null_Address;
    mi_system_address   : Address  := Null_Address;
    mi_socket_addresses : socket_addresses;

    tmp_addr  : aliased socket_address;

    h_or_i    : aliased constant char_array  := To_C (host_or_ip);
    n_p_or_s  : aliased constant char_array  := To_C (network_port_or_service);

    ret_value : constant Interfaces.C.int :=  inner_getaddrinfo
      (host_or_ip_i =>  (if host_or_ip'Length /= 0 then h_or_i'Address else Null_Address),
       port_i       =>  (if network_port_or_service'Length /= 0 then n_p_or_s'Address else Null_Address),
       hints_i      =>  hint'Address,
       response_i   =>  tmp_getted_response'Address);

  begin
    response := mi_socket_addresses;

    if ret_value /= 0 or else tmp_getted_response = Null_Address then
      return False;
    end if;

    mi_system_address := tmp_getted_response;

    loop1 :
    loop
      tmp_addr  :=  null_socket_address;

      b0 :
      declare
        mi_addrinfo : aliased constant addr_info := To_Pointer (mi_system_address).all;
        tmp_sockaddr_storage_ptr  : ainfo2.Object_Pointer := To_Pointer (mi_addrinfo.ai_addr);
      begin

        tmp_addr.storage.ss_family := Interfaces.Unsigned_16 (mi_addrinfo.ai_family);

        if mi_addrinfo.ai_addrlen > 2 then
          tmp_addr.storage.padding (1 .. size_t (mi_addrinfo.ai_addrlen - 2))
            := tmp_sockaddr_storage_ptr.padding (1 .. size_t (mi_addrinfo.ai_addrlen - 2));
        else
          tmp_addr.storage.padding := (others => char'Val (0));
        end if;

        tmp_addr.socktype :=  Address_type (mi_addrinfo.ai_socktype);

        tmp_addr.protocol :=  Address_family (mi_addrinfo.ai_protocol);

        tmp_addr.addr_length  :=  mi_addrinfo.ai_addrlen;

        Prepend (mi_socket_addresses.mi_list, tmp_addr, 1);

        exit loop1 when mi_addrinfo.ai_next = Null_Address;

        mi_system_address := mi_addrinfo.ai_next;

      end b0;

    end loop loop1;

    inner_free_addrinfo (tmp_getted_response);

    response  :=  mi_socket_addresses;

    return True;
  end create_address;

  function create_socket
    (sock_address : aliased socket_address;
     response     : out socket;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
  is
    mi_response   : aliased socket  := null_socket;

    mi_socket_fd  : socket_type;
    proto : constant Address_type_label := a_type_label (sock_address.socktype);

    acc           : Interfaces.C.int := 0
      with Unreferenced;
  begin

    response  :=  null_socket;

    mi_response.storage  := sock_address;

    mi_socket_fd :=
      inner_socket (int (mi_response.storage.storage.ss_family),
        Interfaces.C.int (mi_response.storage.socktype),
        Interfaces.C.int (mi_response.storage.protocol));

    if mi_socket_fd = invalid_socket or else mi_socket_fd = socket_type (socket_error) then
      return False;
    end if;

    mi_response.sock := mi_socket_fd;

    if bind_socket then
      reuse_address (mi_response);

      if inner_bind (mi_response.sock, mi_response.storage.storage'Address,
        Interfaces.C.int (mi_response.storage.addr_length)) /= 0
      then
        acc := inner_close (mi_response.sock);
        return False;
      end if;

      mi_response.binded := True;
    end if;

    if listen_socket and then proto = tcp then

      if inner_listen (mi_response.sock, int (backlog)) /= 0  then
        acc := inner_close (mi_response.sock);
        return False;
      end if;

      mi_response.listened := True;
    end if;

    response := mi_response;

    return True;
  end create_socket;

  function create_socket
    (sock_address : aliased in out socket_addresses;
     response     : out socket;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
  is
    mi_response   : aliased socket  := null_socket;
    mi_address    : aliased socket_address  := null_socket_address;
    mi_socket_fd  : socket_type;
    acc           : Interfaces.C.int := 0
      with Unreferenced;
    ok  : Boolean := False;
  begin

    response := null_socket;

    loop1 :
    while get_address (sock_address, mi_address) loop

      mi_response := null_socket;

      mi_response.storage  := mi_address;

      mi_socket_fd :=
        inner_socket (int (mi_response.storage.storage.ss_family),
          Interfaces.C.int (mi_response.storage.socktype),
          Interfaces.C.int (mi_response.storage.protocol));

      if mi_socket_fd = invalid_socket or else mi_socket_fd = socket_type (socket_error) then
        goto end_loop1_label;
      end if;

      mi_response.sock := mi_socket_fd;

      if bind_socket then
        reuse_address (mi_response);

        if inner_bind (mi_response.sock, mi_response.storage.storage'Address,
          Interfaces.C.int (mi_response.storage.addr_length)) /= 0
        then
          acc := inner_close (mi_response.sock);

          goto end_loop1_label;
        end if;

        mi_response.binded := True;
      end if;

      if listen_socket then
        if a_type_label (mi_response.storage.socktype) = tcp then
          if inner_listen (mi_response.sock, int (backlog)) /= 0  then
            acc := inner_close (mi_response.sock);
            goto end_loop1_label;
          end if;
        end if;

        mi_response.listened := True;
      end if;

      ok := True;

      exit loop1;

      <<end_loop1_label>> -- a missing continue :)

    end loop loop1;

    if ok then
      response := mi_response;
    end if;

    return ok;
  end create_socket;

  function connect
    (sock : aliased in out socket) return Boolean
  is
  begin
    if sock.binded or else sock.connected or else sock.listened then
      return False;
    end if;

    if inner_connect (sock.sock, sock.storage.storage'Address, size_t (sock.storage.addr_length)) /= 0 then
      return False;
    end if;

    sock.connected := True;

    return True;
  end connect;


  function wait_connection
    (sock           : aliased in out socket;
     response       : out socket;
     data_received  : aliased out stream_element_array_access
    ) return Boolean
  is
    mi_response     : socket    := sock;
    mi_storage_size : socklen_t := storage_size;
    proto : constant Address_type_label := a_type_label (sock.storage.socktype);
  begin

    response  :=  null_socket;
    data_received := null;

    if not sock.listened then
      return False;
    end if;

    if proto = tcp then
      mi_response.sock := inner_accept (sock.sock,
        mi_response.storage.storage'Address, mi_storage_size);

      if mi_response.sock = invalid_socket  or else mi_response.sock = socket_type (socket_error) then
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

        len :=  ssize_t (inner_recvfrom (sock.sock, data_tmp'Address, data_tmp'Length, 0,
          mi_response.storage.storage'Address, len_tmp));

        if len = socket_error then
          return False;
        end if;

        mi_response.storage.addr_length := len_tmp;

        mi_socket_fd :=
          inner_socket (int (mi_response.storage.storage.ss_family),
            Interfaces.C.int (mi_response.storage.socktype),
            Interfaces.C.int (mi_response.storage.protocol));

        if mi_socket_fd = invalid_socket or else mi_socket_fd = socket_type (socket_error) then
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

  function send_buffer
    (sock : aliased socket;
     data_to_send : aliased in out socket_buffer
    ) return Interfaces.C.int
  is
    pos           : Stream_Element_Offset :=  data_to_send.head_first;
    remaining     : ssize_t :=  ssize_t (actual_data_size (data_to_send));
    sended_length : ssize_t :=  0;
    total_sended  : ssize_t :=  0;
    proto         : constant Address_type_label :=  a_type_label (sock.storage.socktype);
  begin
    if remaining = 0 then
      return 0;
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
    end loop loop1;

    data_to_send.head_first := data_to_send.head_first + Stream_Element_Count (total_sended);

    return Interfaces.C.int (total_sended);

  end send_buffer;

  function send_stream
    (sock : aliased socket;
     data_to_send : aliased Stream_Element_Array
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
  end send_stream;

  --  function send_buffer_with_timeout
  --    (sock : aliased socket;
  --     data_to_send : aliased in out socket_buffer;
  --     miliseconds_timeout : Unsigned_32
  --    ) return Interfaces.C.int is separate;

  --  function send_stream_with_timeout
  --    (sock : aliased socket;
  --     data_to_send : aliased Stream_Element_Array;
  --     miliseconds_timeout : Unsigned_32
  --    ) return Interfaces.C.int is separate;


  function receive_buffer
    (sock : aliased socket;
     data_to_receive  : aliased in out socket_buffer;
     received_address : aliased out socket_address
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
  end receive_buffer;

  function receive_stream
    (sock : aliased socket;
     data_to_receive : aliased out stream_element_array_access;
     received_address : aliased out socket_address
    ) return Interfaces.C.int
  is
    proto : constant Address_type_label :=  a_type_label (sock.storage.socktype);

    tmp_received_address  : aliased socket_address :=
      (if proto = tcp then null_socket_address else sock.storage);

    tmp_received_address_test  : aliased socket_address :=  tmp_received_address;

    received_length : ssize_t :=  0;
    total_received  : ssize_t :=  0;

    receive_data  : stream_element_array_access :=  new Stream_Element_Array'(1 .. (2**16 + 5) * 3 => 0);
    pos : Stream_Element_Offset := receive_data.all'First;
    tmp_address_test  : Boolean :=  False;

    socket_address_length : socklen_t :=  storage_size;

    tmp_storage_union0 : storage_union := (others => <>);
    tmp_storage_union1 : storage_union := (others => <>);

  begin

    received_address := null_socket_address;

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

      case proto is
        when tcp =>

          received_length := ssize_t (inner_recv (sock.sock,  receive_data.all (pos)'Address,
            receive_data.all'Length - size_t (pos), msg_peek_flag));

        when udp =>

          received_length :=  ssize_t (inner_recvfrom (sock.sock, receive_data.all (pos)'Address,
            receive_data.all'Length - size_t (pos), msg_peek_flag,
            tmp_received_address_test.storage'Address, socket_address_length));

          tmp_received_address_test.addr_length := socket_address_length;

          socket_address_length :=  storage_size;

          -- ToDo ? Is necessary compare 'network port' too?
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

    data_to_receive := new Stream_Element_Array'(receive_data.all (1 .. Stream_Element_Offset (total_received)));

    return Interfaces.C.int (total_received);
  end receive_stream;

  --  function receive_buffer_with_timeout
  --    (sock : aliased socket;
  --     data_to_receive : aliased in out socket_buffer;
  --     received_address : aliased out socket_address;
  --     miliseconds_timeout : Unsigned_32
  --    ) return Interfaces.C.int is separate;

  --  function receive_stream_with_timeout
  --    (sock : aliased socket;
  --     data_to_receive : aliased out stream_element_array_access;
  --     received_address : aliased out socket_address;
  --     miliseconds_timeout : Unsigned_32
  --    ) return Interfaces.C.int is separate;

  procedure clear
    (sock_address : aliased in out socket_address)
  is
  begin
    sock_address  :=  null_socket_address;
  end clear;

  procedure clear  -- remove all stored socket_address's
    (sock_address : aliased in out socket_addresses)
  is
  begin
    if not Is_Empty (sock_address.mi_list) then
      Clear (sock_address.mi_list);
    end if;
    sock_address.mi_next_cursor := Socket_Addresses_Lists.No_Element;
    sock_address.mi_initialized := False;
  end clear;

  procedure clear
    (buffer : aliased in out socket_buffer)
  is
  begin
    buffer.head_first := 0;
    buffer.tail_end := 0;
    buffer.data := null;
  end clear;

  function get_address
    (sock : aliased in socket) return socket_address
  is (sock.storage);

  function get_address
    (sock_address : aliased in out socket_addresses;
     result : aliased out socket_address) return Boolean
  is
  begin
    result := null_socket_address;

    if Is_Empty (sock_address.mi_list) then
      return False;
    end if;

    if not sock_address.mi_initialized then
      sock_address.mi_next_cursor := First (sock_address.mi_list);
      sock_address.mi_initialized := True;
    end if;

    if Has_Element (sock_address.mi_next_cursor) then
      result := Element (sock_address.mi_next_cursor);

      Next (sock_address.mi_next_cursor);

      return True;
    end if;

    sock_address.mi_initialized := False;

    return False;
  end get_address;

  procedure rewind  -- rewind to the first socket_address in socket_addresses
    (sock_address : aliased in out socket_addresses)
  is
  begin
    sock_address.mi_initialized := False;
  end rewind;


  function get_address_port
    (sock_address : aliased in socket_address) return ports
  is
    tmp_addr_union : storage_union := (others => <>);
    stype :  Address_family_label;
  begin
    tmp_addr_union.ss := sock_address.storage;

    stype := family_label (Address_family (tmp_addr_union.ss.ss_family));

    return ports (inner_ntohs ((if stype = ipv6 then tmp_addr_union.i6.sin6_port
      elsif stype = ipv4 then tmp_addr_union.i4.sin_port else 0)));
  end get_address_port;


  function get_address_port
    (sock_address : aliased in socket_address) return String
  is
    string_port : constant String :=  ports'(get_address_port (sock_address))'Image;
  begin
    return string_port (string_port'First + 1 .. string_port'Last);
  end get_address_port;


  function get_address
    (sock_address : aliased in socket_address) return String
  is
      tmp_addr_union : storage_union := (others => <>);
      stype          : Address_family_label;

  begin
    tmp_addr_union.ss := sock_address.storage;
    stype := family_label (Address_family (tmp_addr_union.ss.ss_family));

    b0 :
    declare
      dest : char_array :=
      (1 .. size_t (if stype = ipv4  then ipv4_length elsif stype = ipv6  then ipv6_length else 0) => char'Val (0));

      dest_length : size_t := dest'Length;
    begin
      if dest_length = 0 then
        return "unknown";
      end if;

      if stype = ipv6 then
        inner_inet_ntop (int (tmp_addr_union.i6.sin6_family), tmp_addr_union.i6.sin6_addr'Address, dest'Address, socklen_t (dest'Length));
      end if;

      if stype = ipv4 then
        inner_inet_ntop (int (tmp_addr_union.i4.sin_family), tmp_addr_union.i4.sin_addr'Address, dest'Address, socklen_t (dest'Length));
      end if;

      loop1 :
      for E of reverse dest loop

        exit loop1 when E /= char'Val (0);

        dest_length := dest_length - 1;
      end loop loop1;

      return To_Ada (dest (1 .. dest_length), False);
    end b0;
  end get_address;

  procedure reuse_address
    (sock  : aliased in out socket) is
  begin
    inner_reuse_address (sock.sock);
  end reuse_address;

  procedure close (sock : in out socket) is
    socket_fd : constant int := inner_close (sock.sock)
      with unreferenced;
  begin
    sock := null_socket;
  end close;


  overriding
  procedure Read
    (Stream : in out socket_buffer;
     Item   : out Stream_Element_Array;
     Last   : out Stream_Element_Offset)
  is
  begin
    if Stream.data = null
      or else Stream.tail_end <  Stream.data'First
      or else Stream.head_first < Stream.data'First
    then
      raise buffer_empty_error;
    end if;

    if Item'Length = 0 then
      b1 :
      declare
        mi_empty  : constant Stream_Element_Array (1 .. 0) := (others => 0);
      begin
        Item := mi_empty;
        Last  := Item'Last;
        return;
      end b1;
    end if;

    if Item'Length > (Stream.tail_end + 1) - Stream.head_first then
        raise buffer_insufficient_space_error with " Insufficient amount of data left in buffer to fill the request. ";
    end if;

    Item  :=  Stream.data (Stream.head_first .. (Stream.head_first + Item'Length) - 1);
    Last  :=  Item'Last;
    Stream.head_first :=  Stream.head_first + Item'Length;
  end Read;


  overriding
  procedure Write
    (Stream : in out socket_buffer;
     Item   : in Stream_Element_Array)
  is
    item_len : constant Stream_Element_Count := Item'Length;

    old_data : stream_element_array_access := null;

  begin
    if item_len = 0 then
      return;
    end if;

    if Stream.data /= null and then (item_len > data_tail_length (Stream) or else item_len > max_data_length (Stream)) then
      old_data := new Stream_Element_Array (1 .. Stream_Element_Count (actual_data_size (Stream)));

      old_data.all := Stream.data (Stream.head_first .. Stream.tail_end);
    end if;

    if Stream.data = null or else old_data /= null then

      Stream.data := new Stream_Element_Array'(1 ..
        Stream_Element_Count'Max (40, Stream_Element_Count (actual_data_size (Stream)) + item_len + 40) => 0);

      Stream.head_first := 0;
      Stream.tail_end := 0;

    end if;

    if old_data /= null then

      Stream.data (old_data.all'Range) := old_data.all;
      Stream.data (old_data.all'Last + 1 .. Stream.data'Last) := (others => 0);

      Stream.head_first := 1;
      Stream.tail_end := old_data'Last;
    end if;

    if Stream.head_first < Stream.data.all'First then
      Stream.head_first := Stream.data.all'First;
    end if;

    Stream.data (Stream.tail_end + 1 .. Stream.tail_end + item_len)  :=  Item;
    Stream.tail_end :=  Stream.tail_end + item_len;
  end Write;

  function string_error return String is
    message_a : aliased char_array (1 .. 260) := (others => char'Val (0));
    length_a  : aliased int :=  int (message_a'Last) - 1;
  begin
    inner_show_error (message_a, length_a);

    return To_Ada (message_a (message_a'First .. message_a'First + size_t (length_a)));
  end string_error;

end adare_net.base;
