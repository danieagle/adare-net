
with adare_net_exceptions;  use adare_net_exceptions;

with adare_net.base.inners; use adare_net.base.inners;

with adare_net.base.waits;

package body adare_net.base
  with Preelaborate
is

  tmp_any   : constant Address_family with Import => True, Convention => C, external_name => "c_af_unspec";
  tmp_ipv4  : constant Address_family with Import => True, Convention => C, external_name => "c_af_inet";
  tmp_ipv6  : constant Address_family with Import => True, Convention => C, external_name => "c_af_inet6";

  tmp_tcp : constant Address_type with Import => True, Convention => C, external_name => "c_sock_stream";
  tmp_udp : constant Address_type with Import => True, Convention => C, external_name => "c_sock_dgram";

  ipv4_length : constant socklen_t with Import => True, Convention => C, External_Name => "c_v4_addrstrlen";
  ipv6_length : constant socklen_t with Import => True, Convention => C, External_Name => "c_v6_str_length";

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
  is (sock.sock /= 0 and then sock.storage.stor /= null);

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

  function get_raw_length
    (from : aliased in socket_addresses) return size_t
  is (from.stor.all'Length);

  function is_empty
    (sock_address : aliased in socket_address) return Boolean
  is (sock_address.stor = null);

  function is_empty
    (sock_address : aliased in socket_addresses) return Boolean
  is (sock_address.stor = null);

  function a_family (a_family_label : Address_family_label) return Address_family
  is (case a_family_label is when any => tmp_any, when ipv4 => tmp_ipv4, when ipv6 => tmp_ipv6);

  function a_family_label (a_family : Address_family) return Address_family_label
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
  end a_family_label;

  function a_type (type_label : Address_type_label) return Address_type
  is (case type_label is when tcp => tmp_tcp, when udp => tmp_udp);

  function a_type_label (a_type : Address_type) return Address_type_label
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


  function get_address_type
    (from : not null char_array_access) return Address_type_label
  is (a_type_label (Address_type (a_int.To_Pointer (from.all'Address).all)));

  function get_address_type
    (from : aliased in char_array) return Address_type_label
  is (a_type_label (Address_type (a_int.To_Pointer (from'Address).all)));

  function get_address_type
    (from : aliased in socket) return Address_type_label
  is (get_address_type (from.storage.stor));

  function get_address_type
    (from : aliased in socket_address) return Address_type_label
  is (get_address_type (from.stor));

  function get_address_type
    (from : aliased in socket_addresses) return Address_type_label
  is (get_address_type (from.stor));


  function get_address_protocol
    (from : not null char_array_access) return int
  is (a_int.To_Pointer (from.all (from.all'First + size_t (sizint))'Address).all);

  function get_address_protocol
    (from : aliased in char_array) return int
  is (a_int.To_Pointer (from (from'First + size_t (sizint))'Address).all);

  function get_address_protocol
    (from : aliased in socket) return int
  is (get_address_protocol (from.storage.stor));

  function get_address_protocol
    (from : aliased in socket_address) return int
  is (get_address_protocol (from.stor));

  function get_address_protocol
    (from : aliased in socket_addresses;
     address_start_at : size_t) return int
  is (a_int.To_Pointer (from.stor.all (address_start_at + size_t (sizint))'Address).all);


  function get_address_length
    (from : not null char_array_access) return Unsigned_16
  is (a_uint16.To_Pointer (from.all (from.all'First + (2 * size_t (sizint)))'Address).all);

  function get_address_length
    (from : aliased in char_array) return Unsigned_16
  is (a_uint16.To_Pointer (from (from'First + (2 * size_t (sizint)))'Address).all);

  function get_address_length
    (from : aliased in socket) return Unsigned_16
  is (get_address_length (from.storage.stor));

  function get_address_length
    (from : aliased in socket_address) return Unsigned_16
  is (get_address_length (from.stor));

  function get_address_length
    (from : aliased in socket_addresses;
     address_start_at : size_t) return Unsigned_16
  is (a_uint16.To_Pointer (from.stor.all (address_start_at + (2 * size_t (sizint)))'Address).all);


  function get_address_and_family
    (from : not null char_array_access) return Address
  is (from.all (from.all'First + (2 * size_t (sizint)))'Address);

  function get_address_and_family
    (from : aliased in char_array) return Address
  is (from (from'First + (2 * size_t (sizint)))'Address);

  function get_address_and_family
    (from : aliased in socket) return Address
  is (get_address_and_family (from.storage.stor));

  function get_address_and_family
    (from : aliased in socket_address) return Address
  is (get_address_and_family (from.stor));

  function get_address_and_family
    (from : aliased in socket_addresses;
     address_start_at : size_t) return Address
  is (from.stor.all (address_start_at + (2 * size_t (sizint)))'Address);


  procedure set_address_length
    (from : not null char_array_access;
     length : Unsigned_16)
  is
  begin
    a_uint16.To_Pointer (from.all (from.all'First + (2 * size_t (sizint)))'Address).all := length;
  end set_address_length;

  procedure set_address_length
    (from : aliased in char_array;
     length : Unsigned_16)
  is
  begin
    a_uint16.To_Pointer (from (from'First + (2 * size_t (sizint)))'Address).all := length;
  end set_address_length;

  procedure set_address_length
    (from : aliased in socket;
     length : Unsigned_16)
  is
  begin
    set_address_length (from.storage.stor, length);
  end set_address_length;

  procedure set_address_length
    (from : aliased in socket_address;
     length : Unsigned_16)
  is
  begin
    set_address_length (from.stor, length);
  end set_address_length;

  procedure set_address_length
    (from : aliased in socket_addresses;
     address_start_at : size_t;
     length : Unsigned_16)
  is
  begin
    a_uint16.To_Pointer (from.stor.all (address_start_at + (2 * size_t (sizint)))'Address).all := length;
  end set_address_length;


  function create_addresses
    (host_or_ip   : String;
     network_port_or_service  : String;
     Addr_family  : Address_family_label;
     Addr_type    : Address_type_label;
     response     : aliased out socket_addresses;
     quantity     : Unsigned_16 := 9) return Boolean
  is
    tmp_data        : char_array  := (1 .. 270 * size_t (quantity) => char'Val (0));
    tmp_data_length : size_t      :=  tmp_data'Length;
    tmp_host        : char_array  := To_C (host_or_ip);
    tmp_service     : char_array  := To_C (network_port_or_service);
  begin
    response := (stor => null, next_cursor => 0, initialized => False);

    inner_create_addresses ((if host_or_ip'Length > 0 then tmp_host'Address else Null_Address),
     (if network_port_or_service'Length > 0 then tmp_service'Address else Null_Address),
     tmp_data'Address, tmp_data_length'Address, Addr_family, Addr_type);

    if tmp_data_length <  2 * (size_t (sizint) + size_t (sizuint16)) then
      return False;
    end if;

    response.stor := new char_array'(tmp_data (1 .. tmp_data_length));
    response.next_cursor  :=  0;

    return True;
  end create_addresses;

  function create_socket
    (sock_address : aliased socket_address;
     response     : aliased out socket;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
  is
  begin

    if sock_address.stor.all'Length < 2 * (size_t (sizint) + size_t (sizuint16)) then
      return False;
    end if;

    response.storage.stor  := new char_array'(sock_address.stor.all);

    b0 :
    declare
      a_start     : constant size_t   :=  response.storage.stor.all'First;

      a_type_addr : constant Address  :=  response.storage.stor.all (a_start)'Address;
      a_protocol_addr : constant Address  :=  response.storage.stor.all (a_start + size_t (sizint))'Address;
      a_addr_length   : constant Address  :=  response.storage.stor.all (a_start + size_t (sizint) + size_t (sizint))'Address;
      a_family_addr   : constant Address  :=
        response.storage.stor.all (a_start  + size_t (sizint) + size_t (sizint) + size_t (sizuint16))'Address;

      --  a_addr  : constant Address  :=  a_family_addr;

      proto   : constant Address_type_label := get_address_type (response.storage.stor);

      mi_fd : constant socket_type :=
        inner_socket (int (a_uint16.To_Pointer (get_address_and_family (response.storage.stor)).all), -- family
        int (if proto = tcp then tmp_tcp else tmp_udp), -- type
        get_address_protocol (response.storage.stor)); -- protocol

      acc : int := 0
        with Unreferenced;

    begin
      if mi_fd = invalid_socket then
        response := (others => <>);
        return False;
      end if;

      response.sock := mi_fd;

      if bind_socket then
        reuse_address (response);

        if inner_bind (response.sock, get_address_and_family (response.storage.stor),
          int (get_address_length (response.storage.stor))) /= 0
        then
          acc := inner_close (response.sock);

          response := (others => <>);
          return False;
        end if;

        response.binded := True;
      end if;

      if listen_socket and then proto = tcp then

        if inner_listen (response.sock, int (backlog)) /= 0  then
          acc := inner_close (response.sock);

          response := (others => <>);
          return False;
        end if;

        response.listened := True;
      end if;

      if listen_socket and then proto = udp then
        response.listened := True;
      end if;

      return True;

    end b0;
  end create_socket;

  function create_socket
    (sock_address : aliased in out socket_addresses;
     response     : aliased out socket;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
  is
    mi_response   : aliased socket          := (others => <>);
    mi_address    : aliased socket_address  := (others => <>);
  begin

    rewind (sock_address);

    response := (others => <>);

    loop1 :
    while get_address (sock_address, mi_address) loop

      if create_socket (mi_address, mi_response, bind_socket, listen_socket, backlog) then
        response := mi_response;

        return True;
      end if;
    end loop loop1;

    return False;
  end create_socket;

  function connect
    (sock : aliased in out socket) return Boolean
  is
  begin
    if get_address_type (sock) = udp then
      sock.connected := True;

      return True;
    end if;

    if inner_connect (sock.sock, get_address_and_family (sock), int (get_address_length (sock))) /= 0 then
      return False;
    end if;

    sock.connected := True;

    return True;
  end connect;

  function wait_connection
    (sock           : aliased in out socket;
     response       : aliased out socket;
     data_received  : aliased out stream_element_array_access;
     miliseconds_start_timeout  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;

    proto       : constant Address_type_label := get_address_type (sock.storage.stor);

    poll        : aliased poll_of_events;
  begin

    response  := (others => <>);
    data_received := null;

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

    b0 :
    declare
      storage     : aliased char_array  := (1 .. 270 => char'Val (0));
      storage_len : aliased size_t      := storage'Length - (size_t (sizint) + size_t (sizint) + size_t (sizuint16));
    begin
      storage (1 .. size_t (sizint) + size_t (sizint))
            := sock.storage.stor.all (sock.storage.stor.all'First .. sock.storage.stor.all'First +
              size_t (sizint) + size_t (sizint) - 1);

      if proto = tcp then

        response.sock := inner_accept (sock.sock, get_address_and_family (storage), storage_len'Address);

        if response.sock = invalid_socket then
          response  := (others => <>);

          return False;
        end if;

        set_address_length (storage, Unsigned_16 (storage_len));

        response.connected :=  True;
        response.binded    :=  False;
        response.listened  :=  False;

        response.storage.stor  := new char_array'(storage (1 .. storage_len + size_t (sizint) + size_t (sizint) + size_t (sizuint16)));

        return True;
      end if;

      if proto = udp then
        b1 :
        declare
          data_tmp  : Stream_Element_Array := (1 .. (2**16 + 5) * 3 => 0);
          len       : ssize_t;
          addr_sock : aliased socket_address  := (stor => null);
        begin

          len :=  inner_recvfrom (sock.sock, data_tmp'Address, data_tmp'Length, 0,
            get_address_and_family (storage), storage_len'Address);

          if len = socket_error or else len < 1 then
            return False;
          end if;

          set_address_length (storage, Unsigned_16 (storage_len));

          addr_sock.stor  := new char_array'(storage (1 .. storage_len + size_t (sizint) + size_t (sizint) + size_t (sizuint16)));

          if not create_socket
               (sock_address  => addr_sock,
                response      => response,
                bind_socket   => False,
                listen_socket => False,
                backlog       => 10)
          then
            return False;
          end if;

          data_received := new Stream_Element_Array'(data_tmp (1 .. Ada.Streams.Stream_Element_Offset (len)));

          response.connected :=  True;

          return True;
        end b1;
      end if;
    end b0;

    return False;
  end wait_connection;

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
    proto     : constant Address_type_label := get_address_type (sock);
    addr_len  : constant int      := int (get_address_length (sock));
    addr      : constant Address  := get_address_and_family (sock);

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

          sended_length := inner_send (sock.sock, data_to_send.data (pos)'Address, size_t (remaining), 0);

        when udp =>

          sended_length := inner_sendto (sock.sock, data_to_send.data (pos)'Address, size_t (remaining), 0,
            addr, addr_len);

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

  function send_stream
    (sock : aliased socket;
     data_to_send : aliased Stream_Element_Array;
     send_count   : aliased out ssize_t;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;
    pos           : Stream_Element_Offset :=  data_to_send'First;
    remaining     : ssize_t :=  data_to_send'Length;
    sended_length : ssize_t :=  0;
    total_sended  : ssize_t :=  0;

    proto     : constant Address_type_label := get_address_type (sock);
    addr_len  : constant int      := int (get_address_length (sock));
    addr      : constant Address  := get_address_and_family (sock);

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

          sended_length := inner_send (sock.sock, data_to_send (pos)'Address, size_t (remaining), 0);

        when udp =>

          sended_length := inner_sendto (sock.sock, data_to_send (pos)'Address, size_t (remaining), 0,
            addr, addr_len);

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

    send_count := total_sended;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      close (poll);
    end if;

    return True;
  end send_stream;

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

    storage           : aliased char_array  := (1 .. 270 => char'Val (0));
    storage_addr_len  : constant Address    := storage (1 + size_t (sizint) + size_t (sizint))'Address;
    storage_addr  : constant Address  := storage (storage'First + size_t (sizint) + size_t (sizint) + size_t (sizuint16))'Address;
    storage_len   : aliased size_t    := storage'Length - (size_t (sizint) + size_t (sizint) + size_t (sizuint16));
    stor_len      : aliased constant size_t :=  storage_len;

    proto         : constant Address_type_label :=
      a_type_label (Address_type (a_int.To_Pointer (sock.storage.stor.all (sock.storage.stor.all'First)'Address).all));

    receive_data         : Stream_Element_Array := (1 .. (2**16 + 5) * 3 => 0);
    receive_data_address : constant Address     := receive_data (1)'Address;
    receive_data_length  : constant size_t      := receive_data'Length;

    poll : aliased poll_of_events;
  begin

    if proto = udp then
      storage (1 .. size_t (sizint) + size_t (sizint)) :=
        sock.storage.stor.all (sock.storage.stor.all'First .. (sock.storage.stor.all'First + size_t (sizint) + size_t (sizint)) - 1);
    end if;

    receive_count    := 0;
    received_address := (others => <>);

    if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
      if not set_receive (poll, sock) then
        return False;
      end if;

      if miliseconds_start_timeout > 0 then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_receive (poll, sock)) then
          return False;
        end if;
      end if;
    end if;

    loop1 :
    loop

      case proto is
        when tcp =>

          received_length :=  inner_recv (sock.sock, receive_data_address, receive_data_length, 0);

        when udp =>

          received_length :=  inner_recvfrom (sock.sock, receive_data_address, receive_data_length, 0,
            storage_addr, storage_len'Address);

          tmp_received_address.addr_length := socket_address_length;

          socket_address_length := storage_size;

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



  --  function receive_stream
  --    (sock : aliased socket;
  --     data_to_receive  : aliased out stream_element_array_access;
  --     received_address : aliased out socket_address;
  --     receive_count    : aliased out ssize_t;
  --     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
  --     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
  --    ) return Boolean
  --  is separate;

  procedure clear
    (sock_address : aliased in out socket_address)
  is
  begin
    sock_address.stor  :=  null;
  end clear;

  procedure clear  -- remove all stored socket_address's
    (sock_address : aliased in out socket_addresses)
  is
  begin
    sock_address.stor :=  null;
    sock_address.next_cursor := 0;
    sock_address.initialized := False;
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
  is (socket_address'(stor => new char_array'(sock.storage.stor.all)));

  function get_address
    (sock_address : aliased in out socket_addresses;
     result : aliased out socket_address) return Boolean
  is
  begin

    if not sock_address.initialized then
      sock_address.next_cursor := sock_address.stor.all'First + size_t (sizint);
      sock_address.initialized := True;
    end if;

    if (sock_address.next_cursor + size_t (sizint) + (2 * size_t (sizuint16))) < sock_address.stor.all'Last then
      b0 :
      declare
        pos_first : constant size_t := sock_address.stor.all'First;
        pos_last  : constant size_t := sock_address.next_cursor + size_t (sizint) + size_t (sizuint16) +
          size_t (a_uint16.To_Pointer (sock_address.stor.all (sock_address.next_cursor + size_t (sizint) + size_t (sizuint16))'Address).all);
      begin
        result.stor := new char_array'(
          sock_address.stor.all (pos_first .. (pos_first + size_t (sizint)) - 1) &
          sock_address.stor.all (sock_address.next_cursor .. pos_last - 1));

        sock_address.next_cursor  :=  sock_address.next_cursor + pos_last;

        return True;
      end b0;
    end if;

    sock_address.initialized := False;

    return False;
  end get_address;

  procedure rewind  -- rewind to the first socket_address in socket_addresses
    (sock_address : aliased in out socket_addresses)
  is
  begin
    sock_address.next_cursor := sock_address.stor.all'First + size_t (sizint);
    sock_address.initialized := False;
  end rewind;

  function get_address_port
    (sock_address : aliased in socket_address) return ports
  is
    a_start     : constant size_t   :=  sock_address.stor.all'First;
    a_family_addr : constant Address  :=
      sock_address.stor.all (a_start  + size_t (sizint) + size_t (sizint) + size_t (sizuint16))'Address;
    a_family_label2  : constant Address_family_label := a_family_label (Address_family (a_uint16.To_Pointer (a_family_addr).all));
  begin

    if a_family_label2 = ipv4 then
      b4 :
      declare
        tmp_addr  : constant sockaddr_in  := a_sockaddr_in4.To_Pointer (a_family_addr).all;
      begin
        return ports (inner_ntohs (tmp_addr.sin_port));
      end b4;
    end if;

    if a_family_label2 = ipv6 then
      b6 :
      declare
        tmp_addr  : constant sockaddr_in6  := a_sockaddr_in6.To_Pointer (a_family_addr).all;
      begin
        return ports (inner_ntohs (tmp_addr.sin6_port));
      end b6;
    end if;

    return 0;
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
    a_start       : constant size_t   :=  sock_address.stor.all'First;
    a_family_addr : constant Address  :=  sock_address.stor.all (a_start  + size_t (sizint) +
      size_t (sizint) + size_t (sizuint16))'Address;
    a_family_label2  : constant Address_family_label := a_family_label (Address_family
      (a_uint16.To_Pointer (a_family_addr).all));

    dest : char_array :=
      (1 .. size_t (if a_family_label2 = ipv4  then ipv4_length elsif a_family_label2 = ipv6  then ipv6_length else 0) => char'Val (0));

    dest_length : size_t := dest'Length;

    acc : Address := Null_Address
      with Unreferenced;

  begin
    if dest'Length < 1 then
      return "Unknown";
    end if;

    if a_family_label2 = ipv4 then
      b4 :
      declare
        tmp_addr  : constant sockaddr_in  := a_sockaddr_in4.To_Pointer (a_family_addr).all;
      begin
        acc := inner_inet_ntop (int (tmp_addr.sin_family), tmp_addr.sin_addr'Address, dest'Address,
          dest'Length);
      end b4;
    end if;

    if a_family_label2 = ipv6 then
      b6 :
      declare
        tmp_addr  : constant sockaddr_in6  := a_sockaddr_in6.To_Pointer (a_family_addr).all;
      begin
        acc := inner_inet_ntop (int (tmp_addr.sin6_family), tmp_addr.sin6_addr'Address, dest'Address,
          dest'Length);
      end b6;
    end if;

    loop1 :
    for E of reverse dest loop

      exit loop1 when E /= char'Val (0);

      dest_length := dest_length - 1;
    end loop loop1;

    return To_Ada (dest (1 .. dest_length), False);
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
    sock := (others => <>);
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
