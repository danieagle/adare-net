
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


  function is_empty
    (sock_address : aliased in socket_address) return Boolean
  is (sock_address.stor /= null);

  function is_empty
    (sock_address : aliased in socket_addresses) return Boolean
  is (sock_address.stor /= null);


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

  function family (family_label : Address_family_label) return Address_family
  is (case family_label is when any => tmp_any, when ipv4 => tmp_ipv4, when ipv6 => tmp_ipv6);

  function family_label (a_family : Address_family) return Address_family_label
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

  function create_addresses
    (host_or_ip : String;
     network_port_or_service  : String;
     Addr_family  : Address_family_label;
     Addr_type    : Address_type_label;
     response     : aliased out socket_addresses;
     quantity     : Unsigned_16 := 9) return Boolean
  is
    tmp_data        : char_array  := (1 .. 255 * size_t (quantity) => char'Val (0));
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

  function create_socket_with_address
    (sock_address : aliased socket_address;
     response     : aliased out socket;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
  is
  begin

    if sock_address.stor = null or else sock_address.stor.all'Length < 2 * (size_t (sizint) + size_t (sizuint16)) then
      return False;
    end if;

    response.storage.stor  := new char_array'(sock_address.stor.all);

    b0 :
    declare

      a_start     : constant size_t   :=  response.storage.stor.all'First;

      a_type_addr : constant Address  :=  response.storage.stor.all (a_start)'Address;
      a_protocol_addr : constant Address  :=  response.storage.stor.all (a_start + size_t (sizint))'Address;
      a_addr_length   : constant Address  :=  response.storage.stor.all (a_start + size_t (sizint) + size_t (sizint))'Address;
      a_family_addr : constant Address  :=
        response.storage.stor.all (a_start  + size_t (sizint) + size_t (sizint) + size_t (sizuint16))'Address;

      a_addr  : constant Address  :=  a_family_addr;

      proto   : constant Address_type_label :=
        a_type_label (Address_type (a_int.To_Pointer (a_type_addr).all));

      mi_fd : constant socket_type := inner_socket (int (a_uint16.To_Pointer (a_family_addr).all), -- family
        int (if proto = tcp then tmp_tcp else tmp_udp), -- type
        a_int.To_Pointer (a_protocol_addr).all); -- protocol

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

        if inner_bind (response.sock, response.storage.stor.all'Address,
          socklen_t (a_uint16.To_Pointer (a_addr_length).all)) /= 0
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
  end create_socket_with_address;


  --  function create_socket
  --    (sock_address : aliased in out socket_addresses;
  --     response     : out socket;
  --     bind_socket  : Boolean := False;
  --     listen_socket  : Boolean := False;
  --     backlog        : Unsigned_16 := 10) return Boolean
  --  is
  --    mi_response   : aliased socket  := null_socket;
  --    mi_address    : aliased socket_address  := null_socket_address;
  --  begin

  --    rewind (sock_address);

  --    response := null_socket;

  --    loop1 :
  --    while get_address (sock_address, mi_address) loop

  --      if create_socket_with_address (mi_address, mi_response, bind_socket, listen_socket, backlog) then
  --        response := mi_response;

  --        return True;
  --      end if;
  --    end loop loop1;

  --    return False;
  --  end create_socket;

  --  function connect
  --    (sock : aliased in out socket) return Boolean
  --  is separate;

  --  function wait_connection
  --    (sock           : aliased in out socket;
  --     response       : out socket;
  --     data_received  : aliased out stream_element_array_access;
  --     miliseconds_start_timeout  : Unsigned_32 := 0 -- default is wait forever.
  --    ) return Boolean
  --  is separate;
  --  --    use adare_net.base.waits;

  --  --    mi_response     : socket    := sock;
  --  --    mi_storage_size : socklen_t := storage_size;
  --  --    proto           : constant Address_type_label := a_type_label (sock.storage.socktype);
  --  --    poll            : aliased poll_of_events;
  --  --  begin

  --  --    response      :=  null_socket;
  --  --    data_received := null;

  --  --    if not sock.listened then
  --  --      return False;
  --  --    end if;

  --  --    if miliseconds_start_timeout > 0  then
  --  --      if not set_receive (poll, sock) then
  --  --        return False;
  --  --      end if;

  --  --      if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_receive (poll, sock)) then
  --  --        close (poll);

  --  --        return False;
  --  --      end if;

  --  --      close (poll);
  --  --    end if;

  --  --    if proto = tcp then
  --  --      mi_response.sock := inner_accept (sock.sock,
  --  --        mi_response.storage.storage'Address, mi_storage_size'Address);

  --  --      if mi_response.sock = invalid_socket then
  --  --        return False;
  --  --      end if;

  --  --      mi_response.storage.addr_length :=  mi_storage_size;

  --  --      mi_response.connected :=  True;
  --  --      mi_response.binded    :=  False;
  --  --      mi_response.listened  :=  False;

  --  --      response  := mi_response;

  --  --      return True;
  --  --    end if;

  --  --    if proto = udp then
  --  --      b1 :
  --  --      declare
  --  --        data_tmp  : Stream_Element_Array := (1 .. (2**16 + 5) * 3 => 0);
  --  --        len       : ssize_t;
  --  --        len_tmp   : aliased socklen_t := storage_size;
  --  --        acc       : Interfaces.C.int := 0
  --  --          with Unreferenced;

  --  --        mi_socket_fd  : socket_type := 0;
  --  --      begin

  --  --        len :=  ssize_tinner_recvfrom (sock.sock, data_tmp'Address, data_tmp'Length, 0,
  --  --          mi_response.storage.storage'Address, len_tmp'Address);

  --  --        if len = socket_error or else len < 1 then
  --  --          return False;
  --  --        end if;

  --  --        mi_response.storage.addr_length := len_tmp;

  --  --        mi_socket_fd :=
  --  --          inner_socket (int (mi_response.storage.storage.ss_family),
  --  --            Interfaces.C.int (mi_response.storage.socktype),
  --  --            Interfaces.C.int (mi_response.storage.protocol));

  --  --        if mi_socket_fd = invalid_socket then
  --  --          return False;
  --  --        end if;

  --  --        mi_response.sock := mi_socket_fd;

  --  --        data_received := new Stream_Element_Array'(data_tmp (1 .. Ada.Streams.Stream_Element_Offset (len)));

  --  --        response := mi_response;

  --  --        return True;

  --  --      end b1;
  --  --    end if;

  --  --    return False;
  --  --  end wait_connection;

  --  function send_buffer
  --    (sock : aliased socket;
  --     data_to_send : aliased in out socket_buffer;
  --     send_count   : aliased out ssize_t;
  --     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
  --     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
  --    ) return Boolean
  --  is separate;

  --  function send_stream
  --    (sock : aliased socket;
  --     data_to_send : aliased Stream_Element_Array;
  --     send_count   : aliased out ssize_t;
  --     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
  --     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
  --    ) return Boolean
  --  is separate;

  --  function receive_buffer
  --    (sock : aliased socket;
  --     data_to_receive  : aliased in out socket_buffer;
  --     received_address : aliased out socket_address;
  --     receive_count    : aliased out ssize_t;
  --     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
  --     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
  --    ) return Boolean
  --  is separate;

  --  function receive_stream
  --    (sock : aliased socket;
  --     data_to_receive  : aliased out stream_element_array_access;
  --     received_address : aliased out socket_address;
  --     receive_count    : aliased out ssize_t;
  --     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
  --     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
  --    ) return Boolean
  --  is separate;

  --  procedure clear
  --    (sock_address : aliased in out socket_address)
  --  is
  --  begin
  --    sock_address  :=  null_socket_address;
  --  end clear;

  --  procedure clear  -- remove all stored socket_address's
  --    (sock_address : aliased in out socket_addresses)
  --  is
  --  begin
  --    if not Is_Empty (sock_address.mi_list) then
  --      Clear (sock_address.mi_list);
  --    end if;
  --    sock_address.mi_next_cursor := Socket_Addresses_Lists.No_Element;
  --    sock_address.mi_initialized := False;
  --  end clear;

  --  procedure clear
  --    (buffer : aliased in out socket_buffer)
  --  is
  --  begin
  --    buffer.head_first := 0;
  --    buffer.tail_end := 0;
  --    buffer.data := null;
  --  end clear;

  --  function get_address
  --    (sock : aliased in socket) return socket_address
  --  is (sock.storage);

  function get_address
    (sock_address : aliased in out socket_addresses;
     result : aliased out socket_address) return Boolean
  is
  begin

    if is_empty (sock_address) then
      return False;
    end if;

    if not sock_address.initialized then
      sock_address.next_cursor := sock_address.stor.all'First + size_t (sizint);
      sock_address.initialized := True;
    end if;

    if (sock_address.next_cursor + size_t (sizint) + (2 * size_t (sizuint16))) < sock_address.stor.all'Last then
      b0 :
      declare
        pos_first : constant size_t := sock_address.stor.all'First;
        pos_last  : constant size_t := pos_address  (sock_address.next_cursor) +
          size_t (a_uint16.To_Pointer (sock_address.stor.all (pos_addrlen (sock_address.next_cursor))'Address).all);
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

  --  procedure rewind  -- rewind to the first socket_address in socket_addresses
  --    (sock_address : aliased in out socket_addresses)
  --  is
  --  begin
  --    sock_address.mi_initialized := False;
  --  end rewind;

  --  function get_address_port
  --    (sock_address : aliased in socket_address) return ports
  --  is
  --    tmp_addr_union : storage_union := (others => <>);
  --    stype :  Address_family_label;
  --  begin
  --    tmp_addr_union.ss := sock_address.storage;

  --    stype := family_label (Address_family (tmp_addr_union.ss.ss_family));

  --    return ports (inner_ntohs ((if stype = ipv6 then tmp_addr_union.i6.sin6_port
  --      elsif stype = ipv4 then tmp_addr_union.i4.sin_port else 0)));
  --  end get_address_port;


  --  function get_address_port
  --    (sock_address : aliased in socket_address) return String
  --  is
  --    string_port : constant String :=  ports'(get_address_port (sock_address))'Image;
  --  begin
  --    return string_port (string_port'First + 1 .. string_port'Last);
  --  end get_address_port;


  --  function get_address
  --    (sock_address : aliased in socket_address) return String
  --  is
  --    tmp_addr_union : storage_union := (others => <>);
  --    stype          : Address_family_label;
  --    acc            : Address  := Null_Address
  --      with Unreferenced;
  --  begin
  --    tmp_addr_union.ss := sock_address.storage;
  --    stype := family_label (Address_family (tmp_addr_union.ss.ss_family));

  --    b0 :
  --    declare
  --      dest : char_array :=
  --      (1 .. size_t (if stype = ipv4  then ipv4_length elsif stype = ipv6  then ipv6_length else 0) => char'Val (0));

  --      dest_length : size_t := dest'Length;
  --    begin
  --      if dest_length = 0 then
  --        return "unknown";
  --      end if;

  --      if stype = ipv6 then
  --        acc := inner_inet_ntop (int (tmp_addr_union.i6.sin6_family), tmp_addr_union.i6.sin6_addr'Address, dest'Address,
  --          dest'Length);
  --      end if;

  --      if stype = ipv4 then
  --        acc := inner_inet_ntop (int (tmp_addr_union.i4.sin_family), tmp_addr_union.i4.sin_addr'Address, dest'Address,
  --          dest'Length);
  --      end if;

  --      loop1 :
  --      for E of reverse dest loop

  --        exit loop1 when E /= char'Val (0);

  --        dest_length := dest_length - 1;
  --      end loop loop1;

  --      return To_Ada (dest (1 .. dest_length), False);
  --    end b0;
  --  end get_address;

  procedure reuse_address
    (sock  : aliased in out socket) is
  begin
    inner_reuse_address (sock.sock);
  end reuse_address;

  procedure close (sock : in out socket) is
    socket_fd : constant int := inner_close (sock.sock)
      with unreferenced;
  begin
    --  sock := null_socket;
    null;
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
