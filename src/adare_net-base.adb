
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

  ipv4_length : constant size_t with Import => True, Convention => C, External_Name => "c_v4_addrstrlen";
  ipv6_length : constant size_t with Import => True, Convention => C, External_Name => "c_v6_str_length";

  function actual_data_size
    (buffer : socket_buffer) return Integer_64
  is (if buffer.data /= null then (Integer_64 ((buffer.tail_end + 1) - buffer.head_first)) else 0);

  function actual_data_size
    (buffer : not null socket_buffer_access) return Integer_64
  is (if buffer.data /= null then (Integer_64 ((buffer.tail_end + 1) - buffer.head_first)) else 0);

  function max_data_length
    (buffer : socket_buffer) return Stream_Element_Offset
  is (if buffer.data /= null then (buffer.head_first - buffer.data.all'First) + (buffer.data.all'Last - buffer.tail_end) else 0);

  function max_data_length
    (buffer : not null socket_buffer_access) return Stream_Element_Offset
  is (if buffer.data /= null then (buffer.head_first - buffer.data.all'First) + (buffer.data.all'Last - buffer.tail_end) else 0);

  function data_tail_length
    (buffer : socket_buffer) return Stream_Element_Offset
  is (if buffer.data /= null then buffer.data.all'Last - buffer.tail_end else 0);

  function data_tail_length
    (buffer : not null socket_buffer_access) return Stream_Element_Offset
  is (if buffer.data /= null then buffer.data.all'Last - buffer.tail_end else 0);

  function get_buffer (from : socket_buffer) return socket_buffer
  is (socket_buffer'(Root_Stream_Type with
      data => (if from.data = null then null else new Stream_Element_Array'(from.data.all)),
      head_first => from.head_first, tail_end => from.tail_end));

  function get_buffer (from : not null socket_buffer_access) return socket_buffer
  is (socket_buffer'(Root_Stream_Type with
       data => (if from.data = null then null else new Stream_Element_Array'(from.data.all)),
       head_first => from.head_first, tail_end => from.tail_end));

  function get_buffer (from : socket_buffer) return socket_buffer_access
  is (new socket_buffer'(Root_Stream_Type with
      data => (if from.data = null then null else new Stream_Element_Array'(from.data.all)),
      head_first => from.head_first, tail_end => from.tail_end));

  function get_buffer (from : not null socket_buffer_access) return socket_buffer_access
  is (new socket_buffer'(Root_Stream_Type with
      data => (if from.data = null then null else new Stream_Element_Array'(from.data.all)),
      head_first => from.head_first, tail_end => from.tail_end));

  function is_initialized
    (sock : socket) return Boolean
  is (sock.sock /= 0 and then sock.storage.stor /= null);

  function is_initialized
    (sock : not null socket_access) return Boolean
  is (sock.sock /= 0 and then sock.storage.stor /= null);

  function is_connected
    (sock : socket) return Boolean
  is (sock.connected);

  function is_connected
    (sock : not null socket_access) return Boolean
  is (sock.connected);

  function is_binded
    (sock : socket) return Boolean
  is (sock.binded);

  function is_binded
    (sock : not null socket_access) return Boolean
  is (sock.binded);

  function is_listened
    (sock : socket) return Boolean
  is (sock.listened);

  function is_listened
    (sock : not null socket_access) return Boolean
  is (sock.listened);

  function get_socket
    (sock : socket) return socket_type
  is (sock.sock);

  function get_socket
    (sock : not null socket_access) return socket_type
  is (sock.sock);


  function get_socket
    (sock : socket) return socket
  is (socket'(storage => (stor => new char_array'(sock.storage.stor.all)),
        sock => sock.sock,  connected => sock.connected,
        binded => sock.binded, listened => sock.listened));

  function get_socket
    (sock : not null socket_access) return socket
  is (socket'(storage => (stor => new char_array'(sock.storage.stor.all)),
        sock => sock.sock,  connected => sock.connected,
        binded => sock.binded, listened => sock.listened));

  function get_socket
    (sock : socket) return socket_access
  is (new socket'(storage => (stor => new char_array'(sock.storage.stor.all)),
        sock => sock.sock,  connected => sock.connected,
        binded => sock.binded, listened => sock.listened));

  function get_socket
    (sock : not null socket_access) return socket_access
  is (new socket'(storage => (stor => new char_array'(sock.storage.stor.all)),
        sock => sock.sock,  connected => sock.connected,
        binded => sock.binded, listened => sock.listened)
  );

  function get_raw_length
    (from : socket_addresses) return size_t
  is (from.stor.all'Length);

  function get_raw_length
    (from : not null socket_addresses_access) return size_t
  is (from.stor.all'Length);

  function get_raw_left_length
    (from : socket_addresses) return size_t
  is (if from.next_cursor >= from.stor.all'Last then 0 else from.stor.all'Last - from.next_cursor);

  function get_raw_left_length
    (from : not null socket_addresses_access) return size_t
  is (if from.next_cursor >= from.stor.all'Last then 0 else from.stor.all'Last - from.next_cursor);

  function is_empty
    (sock_address : socket_address) return Boolean
  is (sock_address.stor = null);

  function is_empty
    (sock_address : not null socket_address_access) return Boolean
  is (sock_address.stor = null);

  function is_empty
    (sock_address : socket_addresses) return Boolean
  is (sock_address.stor = null);

  function is_empty
    (sock_address : not null socket_addresses_access) return Boolean
  is (sock_address.stor = null);

  function get_family (a_family_label : Address_family_label) return Address_family
  is (case a_family_label is when any => tmp_any, when ipv4 => tmp_ipv4, when ipv6 => tmp_ipv6);

  function get_family_label (a_family : Address_family) return Address_family_label
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
  end get_family_label;

  function get_family_label (a_family : aliased char_array) return Address_family_label
  is (get_family_label (Address_family (a_uint16.To_Pointer (get_address_and_family_address (a_family)).all)));

  function get_family_label (a_family : not null char_array_access) return Address_family_label
  is (get_family_label (Address_family (a_uint16.To_Pointer (get_address_and_family_address (a_family)).all)));


  function get_family_label (from : socket) return Address_family_label
  is (get_family_label (from.storage.stor));

  function get_family_label (from : not null socket_access) return Address_family_label
  is (get_family_label (from.storage.stor));

  function get_family_label (from : socket_address) return Address_family_label
  is (get_family_label (from.stor));

  function get_family_label (from : not null socket_address_access) return Address_family_label
  is (get_family_label (from.stor));

  function get_family_label (from : socket_addresses) return Address_family_label
  is (get_family_label (Address_family (a_uint16.To_Pointer (get_address_and_family_address (from)).all)));

  function get_family_label (from : not null socket_addresses_access) return Address_family_label
  is (get_family_label (Address_family (a_uint16.To_Pointer (get_address_and_family_address (from)).all)));


  function get_family_label (from : socket) return String
  is (Address_family_label'(get_family_label (from))'Image);

  function get_family_label (from : not null socket_access) return String
  is (Address_family_label'(get_family_label (from))'Image);

  function get_family_label (from : socket_address) return String
  is (Address_family_label'(get_family_label (from))'Image);

  function get_family_label (from : not null socket_address_access) return String
  is (Address_family_label'(get_family_label (from))'Image);

  function get_family_label (from : socket_addresses) return String
  is (Address_family_label'(get_family_label (from))'Image);

  function get_family_label (from : not null socket_addresses_access) return String
  is (Address_family_label'(get_family_label (from))'Image);


  function get_address_type (type_label : Address_type_label) return Address_type
  is (case type_label is when tcp => tmp_tcp, when udp => tmp_udp);

  function get_address_type_label (a_type : Address_type) return Address_type_label
  is
  begin

    if a_type = tmp_tcp then
      return tcp;
    end if;

    if a_type = tmp_udp then
      return udp;
    end if;

    raise Constraint_Error with " unknown Address_Type ";
  end get_address_type_label;


  function get_address_type
    (from : not null char_array_access) return Address_type_label
  is (get_address_type_label (Address_type (a_int.To_Pointer (from.all'Address).all)));

  function get_address_type
    (from : aliased in char_array) return Address_type_label
  is (get_address_type_label (Address_type (a_int.To_Pointer (from'Address).all)));

  function get_address_type
    (from : socket) return Address_type_label
  is (get_address_type (from.storage.stor));

  function get_address_type
    (from : not null socket_access) return Address_type_label
  is (get_address_type (from.storage.stor));

  function get_address_type
    (from : socket_address) return Address_type_label
  is (get_address_type (from.stor));

  function get_address_type
    (from : not null socket_address_access) return Address_type_label
  is (get_address_type (from.stor));

  function get_address_type
    (from : socket_addresses) return Address_type_label
  is (get_address_type (from.stor));

  function get_address_type
    (from : not null socket_addresses_access) return Address_type_label
  is (get_address_type (from.stor));


  function get_address_type
    (from : not null char_array_access) return String
  is (Address_type_label'(get_address_type_label (Address_type (a_int.To_Pointer (from.all'Address).all)))'Image);

  function get_address_type
    (from : aliased in char_array) return String
  is (Address_type_label'(get_address_type_label (Address_type (a_int.To_Pointer (from'Address).all)))'Image);

  function get_address_type
    (from : socket) return String
  is (get_address_type (from.storage.stor));

  function get_address_type
    (from : not null socket_access) return String
  is (get_address_type (from.storage.stor));

  function get_address_type
    (from : socket_address) return String
  is (get_address_type (from.stor));

  function get_address_type
    (from : not null socket_address_access) return String
  is (get_address_type (from.stor));

  function get_address_type
    (from : socket_addresses) return String
  is (get_address_type (from.stor));

  function get_address_type
    (from : not null socket_addresses_access) return String
  is (get_address_type (from.stor));


  function get_address_protocol
    (from : not null char_array_access) return int
  is (a_int.To_Pointer (from.all (from.all'First + sizint)'Address).all);

  function get_address_protocol
    (from : aliased in char_array) return int
  is (a_int.To_Pointer (from (from'First + sizint)'Address).all);

  function get_address_protocol
    (from : socket) return int
  is (get_address_protocol (from.storage.stor));

  function get_address_protocol
    (from : not null socket_access) return int
  is (get_address_protocol (from.storage.stor));

  function get_address_protocol
    (from : socket_address) return int
  is (get_address_protocol (from.stor));

  function get_address_protocol
    (from : not null socket_address_access) return int
  is (get_address_protocol (from.stor));

  function get_address_protocol
    (from : socket_addresses) return int
  is (a_int.To_Pointer (from.stor.all (from.next_cursor)'Address).all); -- start already at 'protocol'

  function get_address_protocol
    (from : not null socket_addresses_access) return int
  is (a_int.To_Pointer (from.stor.all (from.next_cursor)'Address).all); -- start already at 'protocol'


  function get_address_length
    (from : not null char_array_access) return Unsigned_16
  is (a_uint16.To_Pointer (from.all (from.all'First + (2 * sizint))'Address).all);

  function get_address_length
    (from : aliased in char_array) return Unsigned_16
  is (a_uint16.To_Pointer (from (from'First + (2 * sizint))'Address).all);

  function get_address_length
    (from : socket) return Unsigned_16
  is (get_address_length (from.storage.stor));

  function get_address_length
    (from : not null socket_access) return Unsigned_16
  is (get_address_length (from.storage.stor));

  function get_address_length
    (from : socket_address) return Unsigned_16
  is (get_address_length (from.stor));

  function get_address_length
    (from : not null socket_address_access) return Unsigned_16
  is (get_address_length (from.stor));

  function get_address_length
    (from : socket_addresses) return Unsigned_16
  is (a_uint16.To_Pointer (from.stor.all (from.next_cursor + sizint)'Address).all); -- start_at at 'protocol'

  function get_address_length
    (from : not null socket_addresses_access) return Unsigned_16
  is (a_uint16.To_Pointer (from.stor.all (from.next_cursor + sizint)'Address).all); -- start_at at 'protocol'


  function get_address_and_family_address
    (from : not null char_array_access) return Address
  is (from.all (from.all'First + (2 * sizint) + sizuint16)'Address); -- ?

  function get_address_and_family_address
    (from : aliased in char_array) return Address
  is (from (from'First + (2 * sizint) + sizuint16)'Address);

  function get_address_and_family_address
    (from : socket) return Address
  is (get_address_and_family_address (from.storage.stor));

  function get_address_and_family_address
    (from : not null socket_access) return Address
  is (get_address_and_family_address (from.storage.stor));

  function get_address_and_family_address
    (from : socket_address) return Address
  is (get_address_and_family_address (from.stor));

  function get_address_and_family_address
    (from : not null socket_address_access) return Address
  is (get_address_and_family_address (from.stor));

  function get_address_and_family_address
    (from : socket_addresses) return Address
  is (from.stor.all (from.next_cursor + sizint + sizuint16)'Address); -- start_at at 'protocol'

  function get_address_and_family_address
    (from : not null socket_addresses_access) return Address
  is (from.stor.all (from.next_cursor + sizint + sizuint16)'Address); -- start_at at 'protocol'


  procedure set_address_length
    (from : not null char_array_access;
     length : Unsigned_16)
  is
  begin
    a_uint16.To_Pointer (from.all (from.all'First + (2 * sizint))'Address).all := length;
  end set_address_length;

  procedure set_address_length
    (from : aliased in out char_array;
     length : Unsigned_16)
  is
  begin
    a_uint16.To_Pointer (from (from'First + (2 * sizint))'Address).all := length;
  end set_address_length;

  procedure set_address_length
    (from : in out socket;
     length : Unsigned_16)
  is
  begin
    set_address_length (from.storage.stor, length);
  end set_address_length;

  procedure set_address_length
    (from : not null socket_access;
     length : Unsigned_16)
  is
  begin
    set_address_length (from.storage.stor, length);
  end set_address_length;

  procedure set_address_length
    (from : in out socket_address;
     length : Unsigned_16)
  is
  begin
    set_address_length (from.stor, length);
  end set_address_length;

  procedure set_address_length
    (from : not null socket_address_access;
     length : Unsigned_16)
  is
  begin
    set_address_length (from.stor, length);
  end set_address_length;

  procedure set_address_length
    (from : in out socket_addresses;
     length : Unsigned_16)
  is
  begin
    a_uint16.To_Pointer (from.stor.all (from.next_cursor + sizint)'Address).all := length;  -- start_at at 'protocol'
  end set_address_length;

  procedure set_address_length
    (from : not null socket_addresses_access;
     length : Unsigned_16)
  is
  begin
    a_uint16.To_Pointer (from.stor.all (from.next_cursor + sizint)'Address).all := length;  -- start_at at 'protocol'
  end set_address_length;


  function create_addresses
    (host_or_ip   : String;
     network_port_or_service  : String;
     Addr_family  : Address_family_label;
     Addr_type    : Address_type_label;
     response     : out socket_addresses;
     quantity     : Unsigned_16 := 9) return Boolean
  is
    tmp_data        : char_array  := (1 .. 270 * size_t (quantity) => char'Val (0));
    tmp_data_length : size_t      :=  tmp_data'Length;
    tmp_host        : char_array  := To_C (host_or_ip);
    tmp_service     : char_array  := To_C (network_port_or_service);
  begin
    response := socket_addresses'(stor => null, next_cursor => 0, initialized => False);

    inner_create_addresses ((if host_or_ip'Length > 0 then tmp_host'Address else Null_Address),
     (if network_port_or_service'Length > 0 then tmp_service'Address else Null_Address),
     tmp_data'Address, tmp_data_length'Address, Addr_family, Addr_type);

    if tmp_data_length <  2 * (sizint + sizuint16) then
      return False;
    end if;

    response := socket_addresses'(stor => new char_array'(tmp_data (1 .. tmp_data_length)), initialized => True, next_cursor => 0);
    response.next_cursor  :=  response.stor.all'First + sizint;

    return True;
  end create_addresses;

  function create_addresses
    (host_or_ip   : String;
     network_port_or_service  : String;
     Addr_family  : Address_family_label;
     Addr_type    : Address_type_label;
     response     : out socket_addresses_access;
     quantity     : Unsigned_16 := 9) return Boolean
  is
    tmp_data        : char_array  := (1 .. 270 * size_t (quantity) => char'Val (0));
    tmp_data_length : size_t      :=  tmp_data'Length;
    tmp_host        : char_array  := To_C (host_or_ip);
    tmp_service     : char_array  := To_C (network_port_or_service);
  begin
    response := null;

    inner_create_addresses ((if host_or_ip'Length > 0 then tmp_host'Address else Null_Address),
     (if network_port_or_service'Length > 0 then tmp_service'Address else Null_Address),
     tmp_data'Address, tmp_data_length'Address, Addr_family, Addr_type);

    if tmp_data_length <  2 * (sizint + sizuint16) then
      return False;
    end if;

    response  :=  new socket_addresses'(stor => new char_array'(tmp_data (1 .. tmp_data_length)), initialized => True, next_cursor => 0);
    response.next_cursor  :=  response.stor.all'First + sizint;

    return True;
  end create_addresses;

  function create_socket
    (sock_address : socket_address;
     response     : out socket;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
  is
    proto   : Address_type_label;
    acc     : int := 0
      with Unreferenced;

    mi_socket_typ : socket_type := 0;
  begin

    response := socket'(storage => (stor => null), sock => 0, connected => False, binded => False, listened => False);

    if sock_address.stor.all'Length < 2 * (sizint + sizuint16) then
      return False;
    end if;

    response  := socket'(storage => (stor => new char_array'(sock_address.stor.all)), sock => 0,
      connected => False, binded => False, listened => False);

    proto := get_address_type (response.storage.stor);

    mi_socket_typ :=
      inner_socket (int (a_uint16.To_Pointer (get_address_and_family_address (response.storage.stor)).all), -- family
        int (if proto = tcp then tmp_tcp else tmp_udp), -- type
        get_address_protocol (response.storage.stor) -- protocol
      );

    if mi_socket_typ = invalid_socket then
      response.storage.stor := null;
      return False;
    end if;

    if bind_socket then
      reuse_address (mi_socket_typ);

      if inner_bind (mi_socket_typ, get_address_and_family_address (response.storage.stor),
        int (get_address_length (response.storage.stor))) /= 0
      then
        acc := inner_close (mi_socket_typ);

        response.storage.stor := null;

        return False;
      end if;

      response.binded := True;
    end if;

    if listen_socket and then proto = tcp then

      if inner_listen (mi_socket_typ, int (backlog)) /= 0  then
        acc := inner_close (mi_socket_typ);

        response.storage.stor := null;

        return False;
      end if;

      response.listened := True;
    end if;

    if listen_socket and then proto = udp then
      response.listened := True;
    end if;

    response.sock := mi_socket_typ;

    return True;
  end create_socket;

  function create_socket
    (sock_address : socket_address;
     response     : out socket_access;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
  is
    proto   : Address_type_label;
    acc     : int := 0
      with Unreferenced;

    mi_socket_typ : socket_type := 0;
  begin

    response  := null;

    if sock_address.stor.all'Length < 2 * (sizint + sizuint16) then
      return False;
    end if;

    response  := new socket'(storage => (stor => new char_array'(sock_address.stor.all)), sock => 0,
      connected => False, binded => False, listened => False);

    proto := get_address_type (response.storage.stor);

    mi_socket_typ :=
      inner_socket (int (a_uint16.To_Pointer (get_address_and_family_address (response.storage.stor)).all), -- family
        int (if proto = tcp then tmp_tcp else tmp_udp), -- type
        get_address_protocol (response.storage.stor) -- protocol
      );

    if mi_socket_typ = invalid_socket then
      response := null;
      return False;
    end if;

    if bind_socket then
      reuse_address (mi_socket_typ);

      if inner_bind (mi_socket_typ, get_address_and_family_address (response.storage.stor),
        int (get_address_length (response.storage.stor))) /= 0
      then
        acc := inner_close (mi_socket_typ);

        response := null;
        return False;
      end if;

      response.binded := True;
    end if;

    if listen_socket and then proto = tcp then

      if inner_listen (mi_socket_typ, int (backlog)) /= 0  then
        acc := inner_close (mi_socket_typ);

        response := null;
        return False;
      end if;

      response.listened := True;
    end if;

    if listen_socket and then proto = udp then
      response.listened := True;
    end if;

    response.sock :=  mi_socket_typ;

    return True;
  end create_socket;

  function create_socket
    (sock_address : not null socket_address_access;
     response     : out socket;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
  is
    proto   : Address_type_label;
    acc     : int := 0
      with Unreferenced;

    mi_socket_typ : socket_type := 0;
  begin

    response := socket'(storage => (stor => null), sock => 0,
      connected => False, binded => False, listened => False);

    if sock_address.stor.all'Length < 2 * (sizint + sizuint16) then
      return False;
    end if;

    response  := socket'(storage => (stor => new char_array'(sock_address.stor.all)), sock => 0,
      connected => False, binded => False, listened => False);

    proto := get_address_type (response.storage.stor);

    mi_socket_typ :=
      inner_socket (int (a_uint16.To_Pointer (get_address_and_family_address (response.storage.stor)).all), -- family
        int (if proto = tcp then tmp_tcp else tmp_udp), -- type
        get_address_protocol (response.storage.stor) -- protocol
      );

    if mi_socket_typ = invalid_socket then
      response.storage.stor := null;

      return False;
    end if;

    if bind_socket then
      reuse_address (mi_socket_typ);

      if inner_bind (mi_socket_typ, get_address_and_family_address (response.storage.stor),
        int (get_address_length (response.storage.stor))) /= 0
      then
        acc := inner_close (mi_socket_typ);

        response.storage.stor := null;

        return False;
      end if;

      response.binded := True;
    end if;

    if listen_socket and then proto = tcp then

      if inner_listen (mi_socket_typ, int (backlog)) /= 0  then
        acc := inner_close (mi_socket_typ);

        response.storage.stor := null;

        return False;
      end if;

      response.listened := True;
    end if;

    if listen_socket and then proto = udp then
      response.listened := True;
    end if;

    response.sock :=  mi_socket_typ;

    return True;
  end create_socket;

  function create_socket
    (sock_address : not null socket_address_access;
     response     : out socket_access;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
  is
    proto   : Address_type_label;
    acc     : int := 0
      with Unreferenced;

    mi_socket_typ : socket_type := 0;
  begin

    response  := null;

    if sock_address.stor.all'Length < 2 * (sizint + sizuint16) then
      return False;
    end if;

    response  := new socket'(storage => (stor => new char_array'(sock_address.stor.all)), sock => 0,
      connected => False, binded => False, listened => False);

    proto := get_address_type (response.storage.stor);

    mi_socket_typ :=
      inner_socket (int (a_uint16.To_Pointer (get_address_and_family_address (response.storage.stor)).all), -- family
        int (if proto = tcp then tmp_tcp else tmp_udp), -- type
        get_address_protocol (response.storage.stor) -- protocol
      );

    if mi_socket_typ = invalid_socket then
      response := null;
      return False;
    end if;

    if bind_socket then
      reuse_address (mi_socket_typ);

      if inner_bind (mi_socket_typ, get_address_and_family_address (response.storage.stor),
        int (get_address_length (response.storage.stor))) /= 0
      then
        acc := inner_close (mi_socket_typ);

        response := null;
        return False;
      end if;

      response.binded := True;
    end if;

    if listen_socket and then proto = tcp then

      if inner_listen (mi_socket_typ, int (backlog)) /= 0  then
        acc := inner_close (mi_socket_typ);

        response := null;
        return False;
      end if;

      response.listened := True;
    end if;

    if listen_socket and then proto = udp then
      response.listened := True;
    end if;

    response.sock :=  mi_socket_typ;

    return True;
  end create_socket;

  function create_socket
    (sock_address : in out socket_addresses;
     response     : out socket;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
  is

    mi_response : socket  := socket'(storage => (stor => null), sock => 0, connected => False, binded => False, listened => False);

    mi_address  : socket_address  := socket_address'(stor => null);
  begin

    rewind (sock_address);

    response := socket'(storage => (stor => null), sock => 0,
      connected => False, binded => False, listened => False);

    loop1 :
    while get_address (sock_address, mi_address) loop

      if create_socket (mi_address, mi_response, bind_socket, listen_socket, backlog) then
        response := socket'(storage => (stor => new char_array'(mi_response.storage.stor.all)),
          sock => mi_response.sock, connected => mi_response.connected, binded => mi_response.binded,
          listened => mi_response.listened);

        return True;
      end if;
    end loop loop1;

    return False;
  end create_socket;

  function create_socket
    (sock_address : in out socket_addresses;
     response     : out socket_access;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
  is

    mi_response : socket  := socket'(storage => (stor => null), sock => 0, connected => False, binded => False, listened => False);
    --  mi_response : socket_access  := null; -- TBD

    mi_address  : socket_address  := socket_address'(stor => null);
    --  mi_address  : socket_address_access  := null; -- TBD
  begin

    rewind (sock_address);

    response := null;

    loop1 :
    while get_address (sock_address, mi_address) loop

      if create_socket (mi_address, mi_response, bind_socket, listen_socket, backlog) then
        response := new socket'(storage => (stor => new char_array'(mi_response.storage.stor.all)),
          sock => mi_response.sock, connected => mi_response.connected, binded => mi_response.binded,
          listened => mi_response.listened);

        return True;
      end if;
    end loop loop1;

    return False;
  end create_socket;

  function create_socket
    (sock_address : not null socket_addresses_access;
     response     : out socket;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
  is

    mi_response : socket  := socket'(storage => (stor => null), sock => 0, connected => False, binded => False, listened => False);
    --  mi_response : socket_access  := null; -- TBD

    mi_address  : socket_address  := socket_address'(stor => null);
    --  mi_address  : socket_address_access  := null; -- TBD
  begin

    rewind (sock_address);

    response := socket'(storage => (stor => null), sock => 0, connected => False, binded => False, listened => False);

    loop1 :
    while get_address (sock_address, mi_address) loop

      if create_socket (mi_address, mi_response, bind_socket, listen_socket, backlog) then
        response := socket'(storage => (stor => new char_array'(mi_response.storage.stor.all)),
          sock => mi_response.sock, connected => mi_response.connected, binded => mi_response.binded,
          listened => mi_response.listened);

        return True;
      end if;
    end loop loop1;

    return False;
  end create_socket;

  function create_socket
    (sock_address : not null socket_addresses_access;
     response     : out socket_access;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
  is

    mi_response : socket  := socket'(storage => (stor => null), sock => 0, connected => False, binded => False, listened => False);
    --  mi_response : socket_access  := null; -- TBD

    mi_address  : socket_address  := socket_address'(stor => null);
    --  mi_address  : socket_address_access  := null; -- TBD
  begin

    rewind (sock_address);

    response := null;

    loop1 :
    while get_address (sock_address, mi_address) loop

      if create_socket (mi_address, mi_response, bind_socket, listen_socket, backlog) then
        response := new socket'(storage => (stor => new char_array'(mi_response.storage.stor.all)),
          sock => mi_response.sock, connected => mi_response.connected, binded => mi_response.binded,
          listened => mi_response.listened);

        return True;
      end if;
    end loop loop1;

    return False;
  end create_socket;


  function connect
    (sock : in out socket) return Boolean
  is
  begin
    if get_address_type (sock) = udp then
      sock.connected := True;

      return True;
    end if;

    if inner_connect (sock.sock, get_address_and_family_address (sock), int (get_address_length (sock))) /= 0 then
      return False;
    end if;

    sock.connected := True;

    return True;
  end connect;

  function connect
    (sock : not null socket_access) return Boolean
  is
  begin
    if get_address_type (sock) = udp then
      sock.connected := True;

      return True;
    end if;

    if inner_connect (sock.sock, get_address_and_family_address (sock), int (get_address_length (sock))) /= 0 then
      return False;
    end if;

    sock.connected := True;

    return True;
  end connect;

  function wait_connection
    (sock           : socket;
     response       : out socket;
     data_received  : out stream_element_array_access;
     miliseconds_start_timeout  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;

    proto       : constant Address_type_label := get_address_type (sock.storage.stor);

    storage     : aliased char_array  := (1 .. 270 => char'Val (0));
    storage_len : aliased size_t      := storage'Length - (sizint + sizint + sizuint16);
    mi_socket_typ : socket_type       := 0;

    sock_addr   : socket_address  := socket_address'(stor => null);

    poll        : aliased poll_of_events;

  begin

    response  := socket'(storage => (stor => null), sock => 0, connected => False, binded => False, listened => False);
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

    storage (1 .. sizint + sizint) :=
      sock.storage.stor.all (sock.storage.stor.all'First .. sock.storage.stor.all'First + sizint + sizint - 1);

    if proto = tcp then

      mi_socket_typ := inner_accept (sock.sock, get_address_and_family_address (storage), storage_len'Address);

      if mi_socket_typ = invalid_socket then
        return False;
      end if;

      set_address_length (storage, Unsigned_16 (storage_len));

      response  := socket'(storage => (stor => new char_array'(storage (1 .. storage_len + sizint + sizint + sizuint16))),
        sock => mi_socket_typ, connected => True, binded => False, listened => False);

      return True;
    end if;

    if proto = udp then
      b0 :
      declare
        data_tmp  : Stream_Element_Array := (1 .. ((2**16) + 5) * 3 => 0);
        len       : int;
      begin

        len := inner_recvfrom (sock.sock, data_tmp'Address, data_tmp'Length, 0,
          get_address_and_family_address (storage), storage_len'Address);

        if len = socket_error or else len < 1 then
          response  := socket'(storage => (stor => null), sock => 0, connected => False, binded => False, listened => False);
          data_received := null;

          return False;
        end if;

        set_address_length (storage, Unsigned_16 (storage_len));

        sock_addr  := socket_address'(stor => new char_array'(storage (1 .. storage_len + sizint + sizint + sizuint16)));

        if not create_socket
              (sock_address  => sock_addr,
              response      => response,
              bind_socket   => False,
              listen_socket => False,
              backlog       => 10)
        then
          response  := socket'(storage => (stor => null), sock => 0, connected => False, binded => False, listened => False);
          data_received := null;

          return False;
        end if;

        data_received := new Stream_Element_Array'(data_tmp (1 .. Ada.Streams.Stream_Element_Offset (len)));

        response.connected :=  True;

        return True;
      end b0;
    end if;

    response      := socket'(storage => (stor => null), sock => 0, connected => False, binded => False, listened => False);
    data_received := null;

    return False;
  end wait_connection;

  function wait_connection
    (sock           : not null socket_access;
     response       : out socket;
     data_received  : out stream_element_array_access;
     miliseconds_start_timeout  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;

    proto       : constant Address_type_label := get_address_type (sock.storage.stor);

    storage     : aliased char_array  := (1 .. 270 => char'Val (0));
    storage_len : aliased size_t      := storage'Length - (sizint + sizint + sizuint16);
    mi_socket_typ : socket_type       := 0;

    sock_addr   : socket_address  := socket_address'(stor => null);

    poll        : aliased poll_of_events;

  begin

    response  := socket'(storage => (stor => null), sock => 0, connected => False, binded => False, listened => False);
    data_received := null;

    if miliseconds_start_timeout > 0  then
      if not set_receive (poll, sock.all) then
        return False;
      end if;

      if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_receive (poll, sock.all)) then
        close (poll);

        return False;
      end if;

      close (poll);
    end if;

    storage (1 .. sizint + sizint) :=
      sock.storage.stor.all (sock.storage.stor.all'First .. sock.storage.stor.all'First + sizint + sizint - 1);

    if proto = tcp then

      mi_socket_typ := inner_accept (sock.sock, get_address_and_family_address (storage), storage_len'Address);

      if mi_socket_typ = invalid_socket then
        return False;
      end if;

      set_address_length (storage, Unsigned_16 (storage_len));

      response  := socket'(storage => (stor => new char_array'(storage (1 .. storage_len + sizint + sizint + sizuint16))),
        sock => mi_socket_typ, connected => True, binded => False, listened => False);

      return True;
    end if;

    if proto = udp then
      b0 :
      declare
        data_tmp  : Stream_Element_Array := (1 .. ((2**16) + 5) * 3 => 0);
        len       : int;
      begin

        len :=  inner_recvfrom (sock.sock, data_tmp'Address, data_tmp'Length, 0,
          get_address_and_family_address (storage), storage_len'Address);

        if len = socket_error or else len < 1 then
          response  := socket'(storage => (stor => null), sock => 0, connected => False, binded => False, listened => False);
          data_received := null;

          return False;
        end if;

        set_address_length (storage, Unsigned_16 (storage_len));

        sock_addr  := socket_address'(stor => new char_array'(storage (1 .. storage_len + sizint + sizint + sizuint16)));

        if not create_socket
              (sock_address  => sock_addr,
              response      => response,
              bind_socket   => False,
              listen_socket => False,
              backlog       => 10)
        then
          response  := socket'(storage => (stor => null), sock => 0, connected => False, binded => False, listened => False);
          data_received := null;

          return False;
        end if;

        data_received := new Stream_Element_Array'(data_tmp (1 .. Ada.Streams.Stream_Element_Offset (len)));

        response.connected :=  True;

        return True;
      end b0;
    end if;

    response      := socket'(storage => (stor => null), sock => 0, connected => False, binded => False, listened => False);
    data_received := null;

    return False;
  end wait_connection;


  function wait_connection
    (sock           : socket;
     response       : out socket_access;
     data_received  : out stream_element_array_access;
     miliseconds_start_timeout  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;

    proto       : constant Address_type_label := get_address_type (sock.storage.stor);

    storage     : aliased char_array  := (1 .. 270 => char'Val (0));
    storage_len : aliased size_t      := storage'Length - (sizint + sizint + sizuint16);
    mi_socket_typ : socket_type       := 0;

    sock_addr   : socket_address  := socket_address'(stor => null);

    poll        : aliased poll_of_events;

  begin

    response      := null;
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

    storage (1 .. sizint + sizint) :=
      sock.storage.stor.all (sock.storage.stor.all'First .. sock.storage.stor.all'First + sizint + sizint - 1);

    if proto = tcp then

      mi_socket_typ := inner_accept (sock.sock, get_address_and_family_address (storage), storage_len'Address);

      if mi_socket_typ = invalid_socket then
        return False;
      end if;

      set_address_length (storage, Unsigned_16 (storage_len));

      response  := new socket'(storage => (stor => new char_array'(storage (1 .. storage_len + sizint + sizint + sizuint16))),
        sock => mi_socket_typ, connected => True, binded => False, listened => False);

      return True;
    end if;

    if proto = udp then
      b0 :
      declare
        data_tmp  : Stream_Element_Array := (1 .. ((2**16) + 5) * 3 => 0);
        len       : int;
      begin

        len :=  inner_recvfrom (sock.sock, data_tmp'Address, data_tmp'Length, 0,
          get_address_and_family_address (storage), storage_len'Address);

        if len = socket_error or else len < 1 then
          response      := null;
          data_received := null;

          return False;
        end if;

        set_address_length (storage, Unsigned_16 (storage_len));

        sock_addr  := socket_address'(stor => new char_array'(storage (1 .. storage_len + sizint + sizint + sizuint16)));

        if not create_socket
              (sock_address  => sock_addr,
              response      => response,
              bind_socket   => False,
              listen_socket => False,
              backlog       => 10)
        then
          response      := null;
          data_received := null;

          return False;
        end if;

        data_received := new Stream_Element_Array'(data_tmp (1 .. Ada.Streams.Stream_Element_Offset (len)));

        response.connected :=  True;

        return True;
      end b0;
    end if;

    response      := new socket'(storage => (stor => null), sock => 0, connected => False, binded => False, listened => False);
    data_received := null;

    return False;
  end wait_connection;

  function wait_connection
    (sock           : not null socket_access;
     response       : out socket_access;
     data_received  : out stream_element_array_access;
     miliseconds_start_timeout  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;

    proto       : constant Address_type_label := get_address_type (sock.storage.stor);

    storage     : aliased char_array  := (1 .. 270 => char'Val (0));
    storage_len : aliased size_t      := storage'Length - (sizint + sizint + sizuint16);
    mi_socket_typ : socket_type       := 0;

    sock_addr   : socket_address  := socket_address'(stor => null);

    poll        : aliased poll_of_events;

  begin

    response      := null;
    data_received := null;

    if miliseconds_start_timeout > 0  then
      if not set_receive (poll, sock.all) then
        return False;
      end if;

      if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_receive (poll, sock.all)) then
        close (poll);

        return False;
      end if;

      close (poll);
    end if;

    storage (1 .. sizint + sizint) :=
      sock.storage.stor.all (sock.storage.stor.all'First .. sock.storage.stor.all'First + sizint + sizint - 1);

    if proto = tcp then

      mi_socket_typ := inner_accept (sock.sock, get_address_and_family_address (storage), storage_len'Address);

      if mi_socket_typ = invalid_socket then
        return False;
      end if;

      set_address_length (storage, Unsigned_16 (storage_len));

      response  := new socket'(storage => (stor => new char_array'(storage (1 .. storage_len + sizint + sizint + sizuint16))),
        sock => mi_socket_typ, connected => True, binded => False, listened => False);

      return True;
    end if;

    if proto = udp then
      b0 :
      declare
        data_tmp  : Stream_Element_Array := (1 .. ((2**16) + 5) * 3 => 0);
        len       : int;
      begin

        len :=  inner_recvfrom (sock.sock, data_tmp'Address, data_tmp'Length, 0,
          get_address_and_family_address (storage), storage_len'Address);

        if len = socket_error or else len < 1 then

          return False;
        end if;

        set_address_length (storage, Unsigned_16 (storage_len));

        sock_addr  := socket_address'(stor => new char_array'(storage (1 .. storage_len + sizint + sizint + sizuint16)));

        if not create_socket
              (sock_address  => sock_addr,
              response      => response,
              bind_socket   => False,
              listen_socket => False,
              backlog       => 10)
        then
          response      := null;

          return False;
        end if;

        data_received := new Stream_Element_Array'(data_tmp (1 .. Ada.Streams.Stream_Element_Offset (len)));

        response.connected :=  True;

        return True;
      end b0;
    end if;

    --  response      := null;
    --  data_received := null;

    return False;
  end wait_connection;

  function send_buffer
    (sock : socket;
     data_to_send : aliased in out socket_buffer;
     send_count   : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;
    pos           : Stream_Element_Offset :=  data_to_send.head_first;
    remaining     : int :=  int (actual_data_size (data_to_send));
    sended_length : int :=  0;
    total_sended  : int :=  0;

    proto     : constant Address_type_label := get_address_type (sock);

    addr_len  : constant int      := int (get_address_length (sock));
    addr      : constant Address  := get_address_and_family_address (sock);

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
          close (poll);

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

  function send_buffer
    (sock : socket;
     data_to_send : not null socket_buffer_access;
     send_count   : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;

    mi_data_to_send : constant socket_buffer_access := data_to_send;

    pos           : Stream_Element_Offset :=  mi_data_to_send.head_first;

    remaining     : int :=  int (actual_data_size (mi_data_to_send));

    sended_length : int :=  0;
    total_sended  : int :=  0;

    proto     : constant Address_type_label := get_address_type (sock);

    addr_len  : constant int      := int (get_address_length (sock));
    addr      : constant Address  := get_address_and_family_address (sock);

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
          close (poll);

          return False;
        end if;
      end if;
    end if;

    loop1 :
    loop
      case proto is

        when tcp =>

          sended_length := inner_send (sock.sock, mi_data_to_send.data (pos)'Address, size_t (remaining), 0);

        when udp =>

          sended_length := inner_sendto (sock.sock, mi_data_to_send.data (pos)'Address, size_t (remaining), 0,
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

    mi_data_to_send.head_first := mi_data_to_send.head_first + Stream_Element_Count (total_sended);

    send_count := total_sended;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      close (poll);
    end if;

    return True;
  end send_buffer;

  function send_stream
    (sock : socket;
     data_to_send : aliased Stream_Element_Array;
     send_count   : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;
    pos           : Stream_Element_Offset :=  data_to_send'First;
    remaining     : int :=  data_to_send'Length;
    sended_length : int :=  0;
    total_sended  : int :=  0;

    proto     : constant Address_type_label := get_address_type (sock);

    addr_len  : constant int      := int (get_address_length (sock));
    addr      : constant Address  := get_address_and_family_address (sock);

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
          close (poll);

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

  function send_stream
    (sock : socket;
     data_to_send : not null stream_element_array_access;
     send_count   : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    mi_data_to_send : constant stream_element_array_access := data_to_send;

    use adare_net.base.waits;

    pos           : Stream_Element_Offset :=  mi_data_to_send.all'First;

    remaining     : int :=  mi_data_to_send.all'Length;
    sended_length : int :=  0;
    total_sended  : int :=  0;

    proto     : constant Address_type_label := get_address_type (sock);

    addr_len  : constant int      := int (get_address_length (sock));
    addr      : constant Address  := get_address_and_family_address (sock);

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
          close (poll);

          return False;
        end if;
      end if;
    end if;

    loop1 :
    loop
      case proto is

        when tcp =>

          sended_length := inner_send (sock.sock, mi_data_to_send.all (pos)'Address, size_t (remaining), 0);

        when udp =>

          sended_length := inner_sendto (sock.sock, mi_data_to_send.all (pos)'Address, size_t (remaining), 0,
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


  function send_buffer
    (sock : not null socket_access;
     data_to_send : aliased in out socket_buffer;
     send_count   : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;
    pos           : Stream_Element_Offset :=  data_to_send.head_first;
    remaining     : int :=  int (actual_data_size (data_to_send));
    sended_length : int :=  0;
    total_sended  : int :=  0;

    proto     : constant Address_type_label := get_address_type (sock);

    addr_len  : constant int      := int (get_address_length (sock));
    addr      : constant Address  := get_address_and_family_address (sock);

    poll          : aliased poll_of_events;
  begin
    send_count := 0;

    if remaining = 0 then
      return True;
    end if;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      if not set_send (poll, sock.all) then
        return False;
      end if;

      if miliseconds_start_timeout > 0 then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_send (poll, sock.all)) then
          close (poll);

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

        if not (poll_wait (poll, int (miliseconds_next_timeouts)) and then is_send (poll, sock.all)) then
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

  function send_buffer
    (sock : not null socket_access;
     data_to_send : not null socket_buffer_access;
     send_count   : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;

    mi_data_to_send : constant socket_buffer_access := data_to_send;

    pos           : Stream_Element_Offset :=  mi_data_to_send.head_first;

    remaining     : int :=  int (actual_data_size (mi_data_to_send));

    sended_length : int :=  0;
    total_sended  : int :=  0;

    proto     : constant Address_type_label := get_address_type (sock);

    addr_len  : constant int      := int (get_address_length (sock));
    addr      : constant Address  := get_address_and_family_address (sock);

    poll          : aliased poll_of_events;
  begin
    send_count := 0;

    if remaining = 0 then
      return True;
    end if;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      if not set_send (poll, sock.all) then
        return False;
      end if;

      if miliseconds_start_timeout > 0 then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_send (poll, sock.all)) then
          close (poll);

          return False;
        end if;
      end if;
    end if;

    loop1 :
    loop
      case proto is

        when tcp =>

          sended_length := inner_send (sock.sock, mi_data_to_send.data (pos)'Address, size_t (remaining), 0);

        when udp =>

          sended_length := inner_sendto (sock.sock, mi_data_to_send.data (pos)'Address, size_t (remaining), 0,
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

        if not (poll_wait (poll, int (miliseconds_next_timeouts)) and then is_send (poll, sock.all)) then
          exit loop1;
        end if;
      end if;

    end loop loop1;

    mi_data_to_send.head_first := mi_data_to_send.head_first + Stream_Element_Count (total_sended);

    send_count := total_sended;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      close (poll);
    end if;

    return True;
  end send_buffer;

  function send_stream
    (sock : not null socket_access;
     data_to_send : aliased Stream_Element_Array;
     send_count   : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;
    pos           : Stream_Element_Offset :=  data_to_send'First;
    remaining     : int :=  data_to_send'Length;
    sended_length : int :=  0;
    total_sended  : int :=  0;

    proto     : constant Address_type_label := get_address_type (sock);

    addr_len  : constant int      := int (get_address_length (sock));
    addr      : constant Address  := get_address_and_family_address (sock);

    poll          : aliased poll_of_events;
  begin
    send_count := 0;

    if remaining = 0 then
      return True;
    end if;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      if not set_send (poll, sock.all) then
        return False;
      end if;

      if miliseconds_start_timeout > 0 then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_send (poll, sock.all)) then
          close (poll);

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

        if not (poll_wait (poll, int (miliseconds_next_timeouts)) and then is_send (poll, sock.all)) then
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

  function send_stream
    (sock : not null socket_access;
     data_to_send : not null stream_element_array_access;
     send_count   : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    mi_data_to_send : constant stream_element_array_access := data_to_send;

    use adare_net.base.waits;

    pos           : Stream_Element_Offset :=  mi_data_to_send.all'First;

    remaining     : int :=  mi_data_to_send.all'Length;
    sended_length : int :=  0;
    total_sended  : int :=  0;

    proto     : constant Address_type_label := get_address_type (sock);

    addr_len  : constant int      := int (get_address_length (sock));
    addr      : constant Address  := get_address_and_family_address (sock);

    poll          : aliased poll_of_events;
  begin
    send_count := 0;

    if remaining = 0 then
      return True;
    end if;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      if not set_send (poll, sock.all) then
        return False;
      end if;

      if miliseconds_start_timeout > 0 then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_send (poll, sock.all)) then
          close (poll);

          return False;
        end if;
      end if;
    end if;

    loop1 :
    loop
      case proto is

        when tcp =>

          sended_length := inner_send (sock.sock, mi_data_to_send.all (pos)'Address, size_t (remaining), 0);

        when udp =>

          sended_length := inner_sendto (sock.sock, mi_data_to_send.all (pos)'Address, size_t (remaining), 0,
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

        if not (poll_wait (poll, int (miliseconds_next_timeouts)) and then is_send (poll, sock.all)) then
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
    (sock  : socket;
     data_to_receive   : in out socket_buffer;
     received_address  : out socket_address;
     receive_count     : out int;
     miliseconds_start_timeout : Unsigned_32 :=  0;
     miliseconds_next_timeouts : Unsigned_32 :=  0
    ) return Boolean
  is
    use adare_net.base.waits;

    received_length : int := 0;
    total_received  : int := 0;

    proto : constant Address_type_label := get_address_type (sock);

    storage       : aliased char_array  := (1 .. 270 => char'Val (0));
    storage_len   : aliased size_t      := storage'Length - (sizint + sizint + sizuint16);
    storage_addr  : aliased constant Address  := get_address_and_family_address (storage);
    stor_len      : aliased constant size_t   :=  storage_len;

    receive_data         : Stream_Element_Array := (1 .. (2**16 + 5) * 3 => 0);
    receive_data_address : constant Address     := receive_data (1)'Address;
    receive_data_length  : constant size_t      := receive_data'Length;

    poll : aliased poll_of_events;
  begin

    if proto = udp then
      storage (1 .. sizint + sizint) :=
        sock.storage.stor.all (sock.storage.stor.all'First .. (sock.storage.stor.all'First + sizint + sizint) - 1);
    end if;

    receive_count    := 0;
    received_address := socket_address'(stor => null);

    if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
      if not set_receive (poll, sock) then
        return False;
      end if;

      if miliseconds_start_timeout > 0 then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_receive (poll, sock)) then
          close (poll);

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

          set_address_length (storage, Unsigned_16 (storage_len));

          storage_len :=  stor_len;

      end case;

      exit loop1 when received_length < 1 or else received_length = socket_error;

      total_received := total_received + received_length;

      --  bme :
      --  begin
        Stream_Element_Array'Write
        (data_to_receive'Access,
          receive_data (1 .. Stream_Element_Offset (received_length)));
      --  exception
      --    when Constraint_Error => exit loop1;
      --  end bme;


      if miliseconds_next_timeouts > 0 then
        reset_results (poll);

        if not (poll_wait (poll, int (miliseconds_next_timeouts)) and then is_receive (poll, sock)) then
          exit loop1;
        end if;
      end if;

    end loop loop1;

    if proto = udp then
      received_address := socket_address'(stor => new char_array'(storage (1 .. sizint + sizint + sizuint16 +
        size_t (get_address_length (storage)))));
    end if;

    receive_count := total_received;

    if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
      close (poll);
    end if;

    return True;
  end receive_buffer;

  function receive_buffer
    (sock  : socket;
     data_to_receive   : not null socket_buffer_access;
     received_address  : out socket_address;
     receive_count     : out int;
     miliseconds_start_timeout : Unsigned_32 :=  0;
     miliseconds_next_timeouts : Unsigned_32 :=  0
    ) return Boolean
  is
    use adare_net.base.waits;

    received_length : int := 0;
    total_received  : int := 0;

    proto : constant Address_type_label := get_address_type (sock);

    storage       : aliased char_array  := (1 .. 270 => char'Val (0));
    storage_len   : aliased size_t      := storage'Length - (sizint + sizint + sizuint16);
    storage_addr  : aliased constant Address  := get_address_and_family_address (storage);
    stor_len      : aliased constant size_t   :=  storage_len;

    receive_data         : Stream_Element_Array := (1 .. (2**16 + 5) * 3 => 0);
    receive_data_address : constant Address     := receive_data (1)'Address;
    receive_data_length  : constant size_t      := receive_data'Length;

    poll : aliased poll_of_events;
  begin

    if proto = udp then
      storage (1 .. sizint + sizint) :=
        sock.storage.stor.all (sock.storage.stor.all'First .. (sock.storage.stor.all'First + sizint + sizint) - 1);
    end if;

    receive_count    := 0;
    received_address := socket_address'(stor => null);

    if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
      if not set_receive (poll, sock) then
        return False;
      end if;

      if miliseconds_start_timeout > 0 then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_receive (poll, sock)) then
          close (poll);

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

          set_address_length (storage, Unsigned_16 (storage_len));

          storage_len :=  stor_len;

      end case;

      exit loop1 when received_length < 1 or else received_length = socket_error;

      total_received := total_received + received_length;

      --  bme :
      --  begin
        Stream_Element_Array'Write
        (data_to_receive,
          receive_data (1 .. Stream_Element_Offset (received_length)));
      --  exception
        --  when Constraint_Error => exit loop1;
      --  end bme;


      if miliseconds_next_timeouts > 0 then
        reset_results (poll);

        if not (poll_wait (poll, int (miliseconds_next_timeouts)) and then is_receive (poll, sock)) then
          exit loop1;
        end if;
      end if;

    end loop loop1;

    if proto = udp then
      received_address := socket_address'(stor => new char_array'(storage (1 .. sizint + sizint + sizuint16 +
        size_t (get_address_length (storage)))));
    end if;

    receive_count := total_received;

    if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
      close (poll);
    end if;

    return True;
  end receive_buffer;

  function receive_stream
    (sock : socket;
     data_to_receive  : out stream_element_array_access;
     received_address : out socket_address;
     receive_count    : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;

    received_length : int := 0;
    total_received  : int := 0;

    proto     : constant Address_type_label := get_address_type (sock);

    storage       : aliased char_array  := (1 .. 270 => char'Val (0));
    storage_len   : aliased size_t      := storage'Length - (sizint + sizint + sizuint16);
    storage_addr  : aliased constant Address  := get_address_and_family_address (storage);
    stor_len      : aliased constant size_t   :=  storage_len;

    poll : aliased poll_of_events;

    receive_data    : stream_element_array_access :=  new Stream_Element_Array'(1 .. ((2**16) + 5) * 3 => 0);
    pos             : Stream_Element_Offset       := receive_data.all'First;

  begin

    receive_count     :=  0;
    received_address  :=  socket_address'(stor => null);
    data_to_receive   :=  null;

    if proto = udp then
      storage (1 .. sizint + sizint) :=
        sock.storage.stor.all (sock.storage.stor.all'First .. (sock.storage.stor.all'First + sizint + sizint) - 1);
    end if;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      if not set_receive (poll, sock) then
        return False;
      end if;

      if miliseconds_start_timeout > 0  then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_receive (poll, sock)) then
          close (poll);

          return False;
        end if;
      end if;
    end if;

    loop1 :
    loop

      case proto is
        when tcp =>

          received_length := inner_recv (sock.sock, receive_data.all (pos)'Address,
            receive_data.all'Length - size_t (pos), 0);

        when udp =>

          received_length := inner_recvfrom (sock.sock, receive_data.all (pos)'Address,
            receive_data.all'Length - size_t (pos), 0, storage_addr, storage_len'Address);

          set_address_length (storage, Unsigned_16 (storage_len));

          storage_len :=  stor_len;

      end case;

      exit loop1 when received_length < 1 or else received_length = socket_error;

      total_received  :=  total_received + received_length;

      pos :=  pos + Stream_Element_Offset (received_length);

      if pos + (((2**16) + 5) * 2) > receive_data.all'Length then
        b1 :
        declare
          receive_data_old  : constant Stream_Element_Array := receive_data.all (1 .. pos - 1);
        begin

          receive_data :=  new Stream_Element_Array'(1 .. receive_data_old'Length + (((2**16) + 5) * 3) => 0);
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
      received_address := socket_address'(stor => new char_array'(storage (1 .. sizint + sizint + sizuint16 +
        size_t (get_address_length (storage)))));
    end if;

    data_to_receive := new Stream_Element_Array'(receive_data.all (1 .. Stream_Element_Offset (total_received)));
    receive_count := total_received;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      close (poll);
    end if;

    return True;
  end receive_stream;

-- to
  function receive_buffer
    (sock  : not null socket_access;
     data_to_receive   : in out socket_buffer;
     received_address  : out socket_address;
     receive_count     : out int;
     miliseconds_start_timeout : Unsigned_32 :=  0;
     miliseconds_next_timeouts : Unsigned_32 :=  0
    ) return Boolean
  is
    use adare_net.base.waits;

    received_length : int := 0;
    total_received  : int := 0;

    proto : constant Address_type_label := get_address_type (sock);

    storage       : aliased char_array  := (1 .. 270 => char'Val (0));
    storage_len   : aliased size_t      := storage'Length - (sizint + sizint + sizuint16);
    storage_addr  : aliased constant Address  := get_address_and_family_address (storage);
    stor_len      : aliased constant size_t   :=  storage_len;

    receive_data         : Stream_Element_Array := (1 .. ((2**16) + 5) * 3 => 0);
    receive_data_address : constant Address     := receive_data (1)'Address;
    receive_data_length  : constant size_t      := receive_data'Length;

    poll : aliased poll_of_events;
  begin

    if proto = udp then
      storage (1 .. sizint + sizint) :=
        sock.storage.stor.all (sock.storage.stor.all'First .. (sock.storage.stor.all'First + sizint + sizint) - 1);
    end if;

    receive_count    := 0;
    received_address := socket_address'(stor => null);

    if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
      if not set_receive (poll, sock.all) then
        return False;
      end if;

      if miliseconds_start_timeout > 0 then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_receive (poll, sock.all)) then
          close (poll);

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

          set_address_length (storage, Unsigned_16 (storage_len));

          storage_len :=  stor_len;

      end case;

      exit loop1 when received_length < 1 or else received_length = socket_error;

      total_received := total_received + received_length;

      --  bme :
      --  begin
      Stream_Element_Array'Write
      (data_to_receive'Access,
        receive_data (1 .. Stream_Element_Offset (received_length)));
      --  exception
      --    when Constraint_Error => exit loop1;
      --  end bme;


      if miliseconds_next_timeouts > 0 then
        reset_results (poll);

        if not (poll_wait (poll, int (miliseconds_next_timeouts)) and then is_receive (poll, sock.all)) then
          exit loop1;
        end if;
      end if;

    end loop loop1;

    if proto = udp then
      received_address := socket_address'(stor => new char_array'(storage (1 .. sizint + sizint + sizuint16 +
        size_t (get_address_length (storage)))));
    end if;

    receive_count := total_received;

    if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
      close (poll);
    end if;

    return True;
  end receive_buffer;

  function receive_buffer
    (sock  : not null socket_access;
     data_to_receive   : not null socket_buffer_access;
     received_address  : out socket_address;
     receive_count     : out int;
     miliseconds_start_timeout : Unsigned_32 :=  0;
     miliseconds_next_timeouts : Unsigned_32 :=  0
    ) return Boolean
  is
    use adare_net.base.waits;

    received_length : int := 0;
    total_received  : int := 0;

    proto : constant Address_type_label := get_address_type (sock);

    storage       : aliased char_array  := (1 .. 270 => char'Val (0));
    storage_len   : aliased size_t      := storage'Length - (sizint + sizint + sizuint16);
    storage_addr  : aliased constant Address  := get_address_and_family_address (storage);
    stor_len      : aliased constant size_t   :=  storage_len;

    receive_data         : Stream_Element_Array := (1 .. (2**16 + 5) * 3 => 0);
    receive_data_address : constant Address     := receive_data (1)'Address;
    receive_data_length  : constant size_t      := receive_data'Length;

    poll : aliased poll_of_events;
  begin

    if proto = udp then
      storage (1 .. sizint + sizint) :=
        sock.storage.stor.all (sock.storage.stor.all'First .. (sock.storage.stor.all'First + sizint + sizint) - 1);
    end if;

    receive_count    := 0;
    received_address := socket_address'(stor => null);

    if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
      if not set_receive (poll, sock.all) then
        return False;
      end if;

      if miliseconds_start_timeout > 0 then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_receive (poll, sock.all)) then
          close (poll);

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

          set_address_length (storage, Unsigned_16 (storage_len));

          storage_len :=  stor_len;

      end case;

      exit loop1 when received_length < 1 or else received_length = socket_error;

      total_received := total_received + received_length;

      --  bme :
      --  begin
      Stream_Element_Array'Write
      (data_to_receive,
        receive_data (1 .. Stream_Element_Offset (received_length)));
      --  exception
      --    when Constraint_Error => exit loop1;
      --  end bme;


      if miliseconds_next_timeouts > 0 then
        reset_results (poll);

        if not (poll_wait (poll, int (miliseconds_next_timeouts)) and then is_receive (poll, sock.all)) then
          exit loop1;
        end if;
      end if;

    end loop loop1;

    if proto = udp then
      received_address := socket_address'(stor => new char_array'(storage (1 .. sizint + sizint + sizuint16 +
        size_t (get_address_length (storage)))));
    end if;

    receive_count := total_received;

    if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
      close (poll);
    end if;

    return True;
  end receive_buffer;

  function receive_stream
    (sock : not null socket_access;
     data_to_receive  : out stream_element_array_access;
     received_address : out socket_address;
     receive_count    : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;

    received_length : int := 0;
    total_received  : int := 0;

    proto     : constant Address_type_label := get_address_type (sock);

    storage       : aliased char_array  := (1 .. 270 => char'Val (0));
    storage_len   : aliased size_t      := storage'Length - (sizint + sizint + sizuint16);
    storage_addr  : aliased constant Address  := get_address_and_family_address (storage);
    stor_len      : aliased constant size_t   :=  storage_len;

    poll : aliased poll_of_events;

    receive_data    : stream_element_array_access :=  new Stream_Element_Array'(1 .. ((2**16) + 5) * 3 => 0);
    pos             : Stream_Element_Offset       := receive_data.all'First;

  begin

    receive_count     :=  0;
    received_address  :=  socket_address'(stor => null);
    data_to_receive   :=  null;

    if proto = udp then
      storage (1 .. sizint + sizint) :=
        sock.storage.stor.all (sock.storage.stor.all'First .. (sock.storage.stor.all'First + sizint + sizint) - 1);
    end if;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      if not set_receive (poll, sock.all) then
        return False;
      end if;

      if miliseconds_start_timeout > 0  then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_receive (poll, sock.all)) then
          close (poll);

          return False;
        end if;
      end if;
    end if;

    loop1 :
    loop

      case proto is
        when tcp =>

          received_length := inner_recv (sock.sock, receive_data.all (pos)'Address,
            receive_data.all'Length - size_t (pos), 0);

        when udp =>

          received_length := inner_recvfrom (sock.sock, receive_data.all (pos)'Address,
            receive_data.all'Length - size_t (pos), 0, storage_addr, storage_len'Address);

          set_address_length (storage, Unsigned_16 (storage_len));

          storage_len :=  stor_len;

      end case;

      exit loop1 when received_length < 1 or else received_length = socket_error;

      total_received  :=  total_received + received_length;

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

        if not (poll_wait (poll, int (miliseconds_next_timeouts)) and then is_receive (poll, sock.all)) then
          exit loop1;
        end if;
      end if;
    end loop loop1;

    if proto = udp then
      received_address := socket_address'(stor => new char_array'(storage (1 .. sizint + sizint + sizuint16 +
        size_t (get_address_length (storage)))));
    end if;

    data_to_receive := new Stream_Element_Array'(receive_data.all (1 .. Stream_Element_Offset (total_received)));
    receive_count := total_received;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      close (poll);
    end if;

    return True;
  end receive_stream;


  function receive_buffer
    (sock  : socket;
     data_to_receive   : in out socket_buffer;
     received_address  : out socket_address_access;
     receive_count     : out int;
     miliseconds_start_timeout : Unsigned_32 :=  0;
     miliseconds_next_timeouts : Unsigned_32 :=  0
    ) return Boolean
  is
    use adare_net.base.waits;

    received_length : int := 0;
    total_received  : int := 0;

    proto : constant Address_type_label := get_address_type (sock);

    storage       : aliased char_array  := (1 .. 270 => char'Val (0));
    storage_len   : aliased size_t      := storage'Length - (sizint + sizint + sizuint16);
    storage_addr  : aliased constant Address  := get_address_and_family_address (storage);
    stor_len      : aliased constant size_t   :=  storage_len;

    receive_data         : Stream_Element_Array := (1 .. (2**16 + 5) * 3 => 0);
    receive_data_address : constant Address     := receive_data (1)'Address;
    receive_data_length  : constant size_t      := receive_data'Length;

    poll : aliased poll_of_events;
  begin

    if proto = udp then
      storage (1 .. sizint + sizint) :=
        sock.storage.stor.all (sock.storage.stor.all'First .. (sock.storage.stor.all'First + sizint + sizint) - 1);
    end if;

    receive_count    := 0;
    received_address := null;

    if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
      if not set_receive (poll, sock) then
        return False;
      end if;

      if miliseconds_start_timeout > 0 then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_receive (poll, sock)) then
          close (poll);

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

          set_address_length (storage, Unsigned_16 (storage_len));

          storage_len :=  stor_len;

      end case;

      exit loop1 when received_length < 1 or else received_length = socket_error;

      total_received := total_received + received_length;

      --  bme :
      --  begin
      Stream_Element_Array'Write
      (data_to_receive'Access,
        receive_data (1 .. Stream_Element_Offset (received_length)));
      --  exception
      --    when Constraint_Error => exit loop1;
      --  end bme;


      if miliseconds_next_timeouts > 0 then
        reset_results (poll);

        if not (poll_wait (poll, int (miliseconds_next_timeouts)) and then is_receive (poll, sock)) then
          exit loop1;
        end if;
      end if;

    end loop loop1;

    if proto = udp then
      received_address := new socket_address'(stor => new char_array'(storage (1 .. sizint + sizint + sizuint16 +
        size_t (get_address_length (storage)))));
    end if;

    receive_count := total_received;

    if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
      close (poll);
    end if;

    return True;
  end receive_buffer;

  function receive_buffer
    (sock  : socket;
     data_to_receive   : not null socket_buffer_access;
     received_address  : out socket_address_access;
     receive_count     : out int;
     miliseconds_start_timeout : Unsigned_32 :=  0;
     miliseconds_next_timeouts : Unsigned_32 :=  0
    ) return Boolean
  is
    use adare_net.base.waits;

    received_length : int := 0;
    total_received  : int := 0;

    proto : constant Address_type_label := get_address_type (sock);

    storage       : aliased char_array  := (1 .. 270 => char'Val (0));
    storage_len   : aliased size_t      := storage'Length - (sizint + sizint + sizuint16);
    storage_addr  : aliased constant Address  := get_address_and_family_address (storage);
    stor_len      : aliased constant size_t   :=  storage_len;

    receive_data         : Stream_Element_Array := (1 .. (2**16 + 5) * 3 => 0);
    receive_data_address : constant Address     := receive_data (1)'Address;
    receive_data_length  : constant size_t      := receive_data'Length;

    poll : aliased poll_of_events;
  begin

    if proto = udp then
      storage (1 .. sizint + sizint) :=
        sock.storage.stor.all (sock.storage.stor.all'First .. (sock.storage.stor.all'First + sizint + sizint) - 1);
    end if;

    receive_count    := 0;
    received_address := null;

    if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
      if not set_receive (poll, sock) then
        return False;
      end if;

      if miliseconds_start_timeout > 0 then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_receive (poll, sock)) then
          close (poll);

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

          set_address_length (storage, Unsigned_16 (storage_len));

          storage_len :=  stor_len;

      end case;

      exit loop1 when received_length < 1 or else received_length = socket_error;

      total_received := total_received + received_length;

      --  bme :
      --  begin
      Stream_Element_Array'Write
      (data_to_receive,
        receive_data (1 .. Stream_Element_Offset (received_length)));
      --  exception
      --    when Constraint_Error => exit loop1;
      --  end bme;


      if miliseconds_next_timeouts > 0 then
        reset_results (poll);

        if not (poll_wait (poll, int (miliseconds_next_timeouts)) and then is_receive (poll, sock)) then
          exit loop1;
        end if;
      end if;

    end loop loop1;

    if proto = udp then
      received_address := new socket_address'(stor => new char_array'(storage (1 .. sizint + sizint + sizuint16 +
        size_t (get_address_length (storage)))));
    end if;

    receive_count := total_received;

    if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
      close (poll);
    end if;

    return True;
  end receive_buffer;

  function receive_stream
    (sock : socket;
     data_to_receive  : out stream_element_array_access;
     received_address : out socket_address_access;
     receive_count    : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;

    received_length : int := 0;
    total_received  : int := 0;

    proto     : constant Address_type_label := get_address_type (sock);

    storage       : aliased char_array  := (1 .. 270 => char'Val (0));
    storage_len   : aliased size_t      := storage'Length - (sizint + sizint + sizuint16);
    storage_addr  : aliased constant Address  := get_address_and_family_address (storage);
    stor_len      : aliased constant size_t   :=  storage_len;

    poll : aliased poll_of_events;

    receive_data    : stream_element_array_access :=  new Stream_Element_Array'(1 .. ((2**16) + 5) * 3 => 0);
    pos             : Stream_Element_Offset       := receive_data.all'First;

  begin

    receive_count     :=  0;
    received_address  :=  null;
    data_to_receive   :=  null;

    if proto = udp then
      storage (1 .. sizint + sizint) :=
        sock.storage.stor.all (sock.storage.stor.all'First .. (sock.storage.stor.all'First + sizint + sizint) - 1);
    end if;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      if not set_receive (poll, sock) then
        return False;
      end if;

      if miliseconds_start_timeout > 0  then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_receive (poll, sock)) then
          close (poll);

          return False;
        end if;
      end if;
    end if;

    loop1 :
    loop

      case proto is
        when tcp =>

          received_length := inner_recv (sock.sock, receive_data.all (pos)'Address,
            receive_data.all'Length - size_t (pos), 0);

        when udp =>

          received_length := inner_recvfrom (sock.sock, receive_data.all (pos)'Address,
            receive_data.all'Length - size_t (pos), 0, storage_addr, storage_len'Address);

          set_address_length (storage, Unsigned_16 (storage_len));

          storage_len :=  stor_len;

      end case;

      exit loop1 when received_length < 1 or else received_length = socket_error;

      total_received  :=  total_received + received_length;

      pos :=  pos + Stream_Element_Offset (received_length);

      if pos + (((2**16) + 5) * 2) > receive_data.all'Length then
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
      received_address := new socket_address'(stor => new char_array'(storage (1 .. sizint + sizint + sizuint16 +
        size_t (get_address_length (storage)))));
    end if;

    data_to_receive := new Stream_Element_Array'(receive_data.all (1 .. Stream_Element_Offset (total_received)));
    receive_count := total_received;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      close (poll);
    end if;

    return True;
  end receive_stream;

  function receive_buffer
    (sock  : not null socket_access;
     data_to_receive   : in out socket_buffer;
     received_address  : out socket_address_access;
     receive_count     : out int;
     miliseconds_start_timeout : Unsigned_32 :=  0;
     miliseconds_next_timeouts : Unsigned_32 :=  0
    ) return Boolean
  is
    use adare_net.base.waits;

    received_length : int := 0;
    total_received  : int := 0;

    proto : constant Address_type_label := get_address_type (sock);

    storage       : aliased char_array  := (1 .. 270 => char'Val (0));
    storage_len   : aliased size_t      := storage'Length - (sizint + sizint + sizuint16);
    storage_addr  : aliased constant Address  := get_address_and_family_address (storage);
    stor_len      : aliased constant size_t   :=  storage_len;

    receive_data         : Stream_Element_Array := (1 .. ((2**16) + 5) * 3 => 0);
    receive_data_address : constant Address     := receive_data (1)'Address;
    receive_data_length  : constant size_t      := receive_data'Length;

    poll : aliased poll_of_events;
  begin

    if proto = udp then
      storage (1 .. sizint + sizint) :=
        sock.storage.stor.all (sock.storage.stor.all'First .. (sock.storage.stor.all'First + sizint + sizint) - 1);
    end if;

    receive_count    := 0;
    received_address := null;

    if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
      if not set_receive (poll, sock.all) then
        return False;
      end if;

      if miliseconds_start_timeout > 0 then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_receive (poll, sock.all)) then
          close (poll);

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

          set_address_length (storage, Unsigned_16 (storage_len));

          storage_len :=  stor_len;

      end case;

      exit loop1 when received_length < 1 or else received_length = socket_error;

      total_received := total_received + received_length;

      bme :
      begin
        Stream_Element_Array'Write
        (data_to_receive'Access,
          receive_data (1 .. Stream_Element_Offset (received_length)));
      exception
        when Constraint_Error => exit loop1;
      end bme;


      if miliseconds_next_timeouts > 0 then
        reset_results (poll);

        if not (poll_wait (poll, int (miliseconds_next_timeouts)) and then is_receive (poll, sock.all)) then
          exit loop1;
        end if;
      end if;

    end loop loop1;

    if proto = udp then
      received_address := new socket_address'(stor => new char_array'(storage (1 .. sizint + sizint + sizuint16 +
        size_t (get_address_length (storage)))));
    end if;

    receive_count := total_received;

    if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
      close (poll);
    end if;

    return True;
  end receive_buffer;

  function receive_buffer
    (sock  : not null socket_access;
     data_to_receive   : not null socket_buffer_access;
     received_address  : out socket_address_access;
     receive_count     : out int;
     miliseconds_start_timeout : Unsigned_32 :=  0;
     miliseconds_next_timeouts : Unsigned_32 :=  0
    ) return Boolean
  is
    use adare_net.base.waits;

    received_length : int := 0;
    total_received  : int := 0;

    proto : constant Address_type_label := get_address_type (sock);

    storage       : aliased char_array  := (1 .. 270 => char'Val (0));
    storage_len   : aliased size_t      := storage'Length - (sizint + sizint + sizuint16);
    storage_addr  : aliased constant Address  := get_address_and_family_address (storage);
    stor_len      : aliased constant size_t   :=  storage_len;

    receive_data         : Stream_Element_Array := (1 .. ((2**16) + 5) * 3 => 0);
    receive_data_address : constant Address     := receive_data'Address;
    receive_data_length  : constant size_t      := receive_data'Length;

    poll : aliased poll_of_events;
  begin

    if proto = udp then
      storage (1 .. sizint + sizint) :=
        sock.storage.stor.all (sock.storage.stor.all'First .. (sock.storage.stor.all'First + sizint + sizint) - 1);
    end if;

    receive_count    := 0;
    received_address := null;

    if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
      if not set_receive (poll, sock.all) then
        return False;
      end if;

      if miliseconds_start_timeout > 0 then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_receive (poll, sock.all)) then
          close (poll);

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

          set_address_length (storage, Unsigned_16 (storage_len));

          storage_len :=  stor_len;

      end case;

      exit loop1 when received_length < 1 or else received_length = socket_error;

      total_received := total_received + received_length;

      --  bme :
      --  begin
      Stream_Element_Array'Write
      (data_to_receive,
        receive_data (1 .. Stream_Element_Offset (received_length)));
      --  exception
      --    when Constraint_Error => exit loop1;
      --  end bme;


      if miliseconds_next_timeouts > 0 then
        reset_results (poll);

        if not (poll_wait (poll, int (miliseconds_next_timeouts)) and then is_receive (poll, sock.all)) then
          exit loop1;
        end if;
      end if;

    end loop loop1;

    if proto = udp then
      received_address := new socket_address'(stor => new char_array'(storage (1 .. sizint + sizint + sizuint16 +
        size_t (get_address_length (storage)))));
    end if;

    receive_count := total_received;

    if miliseconds_start_timeout > 0 or else miliseconds_next_timeouts > 0 then
      close (poll);
    end if;

    return True;
  end receive_buffer;

  function receive_stream
    (sock : not null socket_access;
     data_to_receive  : out stream_element_array_access;
     received_address : out socket_address_access;
     receive_count    : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
  is
    use adare_net.base.waits;

    received_length : int := 0;
    total_received  : int := 0;

    proto     : constant Address_type_label := get_address_type (sock);

    storage       : aliased char_array  := (1 .. 270 => char'Val (0));
    storage_len   : aliased size_t      := storage'Length - (sizint + sizint + sizuint16);
    storage_addr  : aliased constant Address  := get_address_and_family_address (storage);
    stor_len      : aliased constant size_t   :=  storage_len;

    poll : aliased poll_of_events;

    receive_data    : stream_element_array_access :=  new Stream_Element_Array'(1 .. ((2**16) + 5) * 3 => 0);
    pos             : Stream_Element_Offset       := receive_data.all'First;

  begin

    receive_count     :=  0;
    received_address  :=  null;
    data_to_receive   :=  null;

    if proto = udp then
      storage (1 .. sizint + sizint) :=
        sock.storage.stor.all (sock.storage.stor.all'First .. (sock.storage.stor.all'First + sizint + sizint) - 1);
    end if;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      if not set_receive (poll, sock.all) then
        return False;
      end if;

      if miliseconds_start_timeout > 0  then
        if not (poll_wait (poll, int (miliseconds_start_timeout)) and then is_receive (poll, sock.all)) then
          close (poll);

          return False;
        end if;
      end if;
    end if;

    loop1 :
    loop

      case proto is
        when tcp =>

          received_length := inner_recv (sock.sock, receive_data.all (pos)'Address,
            receive_data.all'Length - size_t (pos), 0);

        when udp =>

          received_length := inner_recvfrom (sock.sock, receive_data.all (pos)'Address,
            receive_data.all'Length - size_t (pos), 0, storage_addr, storage_len'Address);

          set_address_length (storage, Unsigned_16 (storage_len));

          storage_len :=  stor_len;

      end case;

      exit loop1 when received_length < 1 or else received_length = socket_error;

      total_received  :=  total_received + received_length;

      pos :=  pos + Stream_Element_Offset (received_length);

      if pos + (((2**16) + 5) * 2) > receive_data.all'Length then
        b1 :
        declare
          receive_data_old  : constant Stream_Element_Array := receive_data.all (1 .. pos - 1);
        begin

          receive_data :=  new Stream_Element_Array'(1 .. receive_data_old'Length + (((2**16) + 5) * 3) => 0);
          receive_data.all (receive_data_old'Range) := receive_data_old;
        end b1;
      end if;

      if miliseconds_next_timeouts > 0 then
        reset_results (poll);

        if not (poll_wait (poll, int (miliseconds_next_timeouts)) and then is_receive (poll, sock.all)) then
          exit loop1;
        end if;
      end if;
    end loop loop1;

    if proto = udp then
      received_address := new socket_address'(stor => new char_array'(storage (1 .. sizint + sizint + sizuint16 +
        size_t (get_address_length (storage)))));
    end if;

    data_to_receive := new Stream_Element_Array'(receive_data.all (1 .. Stream_Element_Offset (total_received)));
    receive_count := total_received;

    if miliseconds_start_timeout > 0  or else miliseconds_next_timeouts > 0 then
      close (poll);
    end if;

    return True;
  end receive_stream;


  procedure clear
    (sock_address : in out socket_address)
  is
  begin
    sock_address.stor  :=  null;
  end clear;

  procedure clear
    (sock_address : not null socket_address_access)
  is
  begin
    sock_address.stor  :=  null;
  end clear;

  procedure clear
    (sock_address : in out socket_addresses)
  is
  begin
    sock_address.stor :=  null;
    sock_address.next_cursor := 0;
    sock_address.initialized := False;
  end clear;

  procedure clear
    (sock_address : not null socket_addresses_access)
  is
  begin
    sock_address.stor :=  null;
    sock_address.next_cursor := 0;
    sock_address.initialized := False;
  end clear;

  procedure clear
    (buffer : in out socket_buffer)
  is
  begin
    buffer.head_first := 0;
    buffer.tail_end := 0;
    buffer.data := null;
  end clear;

  procedure clear
    (buffer : not null socket_buffer_access)
  is
  begin
    buffer.head_first := 0;
    buffer.tail_end := 0;
    buffer.data := null;
  end clear;

  function get_address
    (sock : socket) return socket_address
  is (socket_address'(stor => new char_array'(sock.storage.stor.all)));

  function get_address
    (sock : not null socket_access) return socket_address
  is (socket_address'(stor => new char_array'(sock.storage.stor.all)));

  function get_address
    (sock : socket) return socket_address_access
  is (new socket_address'(stor => new char_array'(sock.storage.stor.all)));

  function get_address
    (sock : not null socket_access) return socket_address_access
  is (new socket_address'(stor => new char_array'(sock.storage.stor.all)));

  procedure get_address
    (sock   : socket;
    result  : out socket_address)
  is
  begin
    result := socket_address'(stor => new char_array'(sock.storage.stor.all));
  end get_address;

  procedure get_address
    (sock   : socket;
    result  : out socket_address_access)
  is
  begin
    result := new socket_address'(stor => new char_array'(sock.storage.stor.all));
  end get_address;

  procedure get_address
    (sock   : socket_access;
    result  : out socket_address)
  is
  begin
    result := socket_address'(stor => new char_array'(sock.storage.stor.all));
  end get_address;

  procedure get_address
    (sock   : socket_access;
    result  : out socket_address_access)
  is
  begin
    result := new socket_address'(stor => new char_array'(sock.storage.stor.all));
  end get_address;

  function get_address
    (sock_address : in out socket_addresses;
     result : out socket_address) return Boolean
  is
    addr_len : size_t := 0;
  begin

    result := socket_address'(stor => null);

    if not sock_address.initialized then
      sock_address.next_cursor := sock_address.stor.all'First + sizint; -- start at 'protocol'
      sock_address.initialized := True;
    end if;

    if sock_address.next_cursor >= sock_address.stor.all'Last then -- less one?
      sock_address.initialized := False;

      return False;
    end if;

    addr_len  := size_t (get_address_length (sock_address));

    result := socket_address'(stor => new char_array'(1 .. ((2 * sizint) + sizuint16 + addr_len + 1)  => char'Val (0)));

    result.stor.all (1 .. sizint) := sock_address.stor.all (sock_address.stor.all'First .. sock_address.stor.all'First + sizint - 1);

    result.stor.all (1 + sizint .. sizint  + sizint + sizuint16 + addr_len)
      := sock_address.stor.all (sock_address.next_cursor ..
        (sock_address.next_cursor + sizint + sizuint16 + addr_len) - 1);

    sock_address.next_cursor  := sock_address.next_cursor + sizint + sizuint16 + addr_len + 1;

    return True;
  end get_address;

  function get_address
    (sock_address : not null socket_addresses_access;
     result : out socket_address) return Boolean
  is
    addr_len : size_t := 0;
  begin

    result := socket_address'(stor => null);

    if not sock_address.initialized then
      sock_address.next_cursor := sock_address.stor.all'First + sizint; -- start at 'protocol'
      sock_address.initialized := True;
    end if;

    if sock_address.next_cursor >= sock_address.stor.all'Last then -- less one?
      sock_address.initialized := False;

      return False;
    end if;

    addr_len  := size_t (get_address_length (sock_address));

    result := socket_address'(stor => new char_array'(1 .. ((2 * sizint) + sizuint16 + addr_len + 1)  => char'Val (0)));

    result.stor.all (1 .. sizint) := sock_address.stor.all (sock_address.stor.all'First .. sock_address.stor.all'First + sizint - 1);

    result.stor.all (1 + sizint .. sizint  + sizint + sizuint16 + addr_len)
      := sock_address.stor.all (sock_address.next_cursor ..
        (sock_address.next_cursor + sizint + sizuint16 + addr_len) - 1);

    sock_address.next_cursor  := sock_address.next_cursor + sizint + sizuint16 + addr_len + 1;

    return True;
  end get_address;

  function get_address
    (sock_address : in out socket_addresses;
     result : out socket_address_access) return Boolean
  is
    addr_len : size_t := 0;
  begin

    result := null;

    if not sock_address.initialized then
      sock_address.next_cursor := sock_address.stor.all'First + sizint; -- start at 'protocol'
      sock_address.initialized := True;
    end if;

    if sock_address.next_cursor >= sock_address.stor.all'Last then -- less one?
      sock_address.initialized := False;

      return False;
    end if;

    addr_len  := size_t (get_address_length (sock_address));

    result := new socket_address'(stor => new char_array'(1 .. ((2 * sizint) + sizuint16 + addr_len + 1)  => char'Val (0)));

    result.stor.all (1 .. sizint) := sock_address.stor.all (sock_address.stor.all'First .. sock_address.stor.all'First + sizint - 1);

    result.stor.all (1 + sizint .. sizint  + sizint + sizuint16 + addr_len)
      := sock_address.stor.all (sock_address.next_cursor ..
        (sock_address.next_cursor + sizint + sizuint16 + addr_len) - 1);

    sock_address.next_cursor  := sock_address.next_cursor + sizint + sizuint16 + addr_len + 1;

    return True;
  end get_address;

  function get_address
    (sock_address : not null socket_addresses_access;
     result : out socket_address_access) return Boolean
  is
    addr_len : size_t := 0;
  begin

    result := null;

    if not sock_address.initialized then
      sock_address.next_cursor := sock_address.stor.all'First + sizint; -- start at 'protocol'
      sock_address.initialized := True;
    end if;

    if sock_address.next_cursor >= sock_address.stor.all'Last then -- less one?
      sock_address.initialized := False;

      return False;
    end if;

    addr_len  := size_t (get_address_length (sock_address));

    result := new socket_address'(stor => new char_array'(1 .. ((2 * sizint) + sizuint16 + addr_len + 1)  => char'Val (0)));

    result.stor.all (1 .. sizint) := sock_address.stor.all (sock_address.stor.all'First .. sock_address.stor.all'First + sizint - 1);

    result.stor.all (1 + sizint .. sizint  + sizint + sizuint16 + addr_len)
      := sock_address.stor.all (sock_address.next_cursor ..
        (sock_address.next_cursor + sizint + sizuint16 + addr_len) - 1);

    sock_address.next_cursor  := sock_address.next_cursor + sizint + sizuint16 + addr_len + 1;

    return True;
  end get_address;


  procedure rewind  -- rewind to the first socket_address in socket_addresses
    (sock_address : in out socket_addresses)
  is
  begin
    sock_address.next_cursor := sock_address.stor.all'First + sizint;
    sock_address.initialized := False;
  end rewind;

  procedure rewind  -- rewind to the first socket_address in socket_addresses
    (sock_address : not null socket_addresses_access)
  is
  begin
    sock_address.next_cursor := sock_address.stor.all'First + sizint;
    sock_address.initialized := False;
  end rewind;

  function get_address_port
    (sock_address : socket_address) return ports
  is
    mi_family_label : constant Address_family_label := get_family_label (sock_address.stor);
  begin

    if mi_family_label = ipv4 then

      return ports (inner_ntohs (a_sockaddr_in4.To_Pointer (get_address_and_family_address (sock_address.stor)).sin_port));
    end if;

    if mi_family_label = ipv6 then

      return ports (inner_ntohs (a_sockaddr_in6.To_Pointer (get_address_and_family_address (sock_address.stor)).sin6_port));
    end if;

    return 0;
  end get_address_port;

  function get_address_port
    (sock_address : not null socket_address_access) return ports
  is
    mi_family_label : constant Address_family_label := get_family_label (sock_address.stor);
  begin

    if mi_family_label = ipv4 then

      return ports (inner_ntohs (a_sockaddr_in4.To_Pointer (get_address_and_family_address (sock_address.stor)).sin_port));
    end if;

    if mi_family_label = ipv6 then

      return ports (inner_ntohs (a_sockaddr_in6.To_Pointer (get_address_and_family_address (sock_address.stor)).sin6_port));
    end if;

    return 0;
  end get_address_port;

  function get_address_port
    (sock_address : socket_address) return String
  is
    string_port : constant String :=  ports'(get_address_port (sock_address))'Image;
  begin
    return string_port (string_port'First + 1 .. string_port'Last);
  end get_address_port;

  function get_address_port
    (sock_address : not null socket_address_access) return String
  is
    string_port : constant String :=  ports'(get_address_port (sock_address))'Image;
  begin
    return string_port (string_port'First + 1 .. string_port'Last);
  end get_address_port;

  function get_address
    (sock_address : socket_address) return String
  is
    mi_family_label : constant Address_family_label := get_family_label (sock_address.stor);

    dest : char_array :=
      (1 .. (if mi_family_label = ipv4 then ipv4_length
        elsif mi_family_label = ipv6  then ipv6_length
        else 0) => char'Val (0));

    dest_length : size_t := dest'Length;

    acc : Address := Null_Address
      with Unreferenced;

  begin
    if dest_length < 1 then
      return "Unknown";
    end if;

    if mi_family_label = ipv4 then
      b4 :
      declare
        tmp_addr  : constant sockaddr_in  := a_sockaddr_in4.To_Pointer (get_address_and_family_address (sock_address.stor)).all;
      begin
        acc := inner_inet_ntop (int (tmp_addr.sin_family), tmp_addr.sin_addr'Address, dest'Address,
          dest'Length);
      end b4;
    end if;

    if mi_family_label = ipv6 then
      b6 :
      declare
        tmp_addr  : constant sockaddr_in6  := a_sockaddr_in6.To_Pointer (get_address_and_family_address (sock_address.stor)).all;
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

  function get_address
    (sock_address : not null socket_address_access) return String
  is
    mi_family_label : constant Address_family_label := get_family_label (sock_address.stor);

    dest : char_array :=
      (1 .. (if mi_family_label = ipv4 then ipv4_length
        elsif mi_family_label = ipv6  then ipv6_length
        else 0) => char'Val (0));

    dest_length : size_t := dest'Length;

    acc : Address := Null_Address
      with Unreferenced;

  begin
    if dest_length < 1 then
      return "Unknown";
    end if;

    if mi_family_label = ipv4 then
      b4 :
      declare
        tmp_addr  : constant sockaddr_in  := a_sockaddr_in4.To_Pointer (get_address_and_family_address (sock_address.stor)).all;
      begin
        acc := inner_inet_ntop (int (tmp_addr.sin_family), tmp_addr.sin_addr'Address, dest'Address,
          dest'Length);
      end b4;
    end if;

    if mi_family_label = ipv6 then
      b6 :
      declare
        tmp_addr  : constant sockaddr_in6  := a_sockaddr_in6.To_Pointer (get_address_and_family_address (sock_address.stor)).all;
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
    (sock  : socket) is
  begin
    inner_reuse_address (sock.sock);
  end reuse_address;

  procedure reuse_address
    (sock  : socket_type) is
  begin
    inner_reuse_address (sock);
  end reuse_address;

  procedure reuse_address
    (sock  : not null socket_access) is
  begin
    inner_reuse_address (sock.sock);
  end reuse_address;



  procedure close (sock : in out socket) is
    socket_fd : constant int := inner_close (sock.sock)
      with unreferenced;
  begin
    sock.sock         := 0;
    sock.storage.stor := null;
    sock.binded       := False;
    sock.connected    := False;
    sock.listened     := False;
  end close;

  procedure close (sock : not null socket_access) is
    socket_fd : constant int := inner_close (sock.sock)
      with unreferenced;
  begin
    sock.sock         := 0;
    sock.storage.stor := null;
    sock.binded       := False;
    sock.connected    := False;
    sock.listened     := False;
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


  function get_write_pos (from : socket_buffer) return write_pos
  is (write_pos (from.tail_end));

  function get_write_pos (from : not null socket_buffer_access) return write_pos
  is (write_pos (from.tail_end));

  function get_read_pos (from : socket_buffer) return read_pos
  is (read_pos (from.head_first));

  function get_read_pos (from : not null socket_buffer_access) return read_pos
  is (read_pos (from.head_first));


  function set_write_pos (to : in out socket_buffer;  val : write_pos) return Boolean
  is
  begin
    if to.data = null or else Ada.Streams.Stream_Element_Offset (val) > to.tail_end then
      return False;
    end if;

    if Ada.Streams.Stream_Element_Offset (val) = to.tail_end then
      return True;
    end if;

    to.tail_end :=  Ada.Streams.Stream_Element_Offset (val);

    return True;
  end set_write_pos;

  function set_write_pos (to : not null socket_buffer_access;  val : write_pos) return Boolean
  is
  begin
    if to.data = null or else Ada.Streams.Stream_Element_Offset (val) > to.tail_end then
      return False;
    end if;

    if Ada.Streams.Stream_Element_Offset (val) = to.tail_end then
      return True;
    end if;

    to.tail_end :=  Ada.Streams.Stream_Element_Offset (val);

    return True;
  end set_write_pos;

  function set_read_pos (to : in out socket_buffer;  val : read_pos) return Boolean
  is
  begin
    if to.data = null or else Ada.Streams.Stream_Element_Offset (val) > to.head_first then
      return False;
    end if;

    if Ada.Streams.Stream_Element_Offset (val) = to.head_first then
      return True;
    end if;

    to.head_first :=  Ada.Streams.Stream_Element_Offset (val);

    return True;
  end set_read_pos;

  function set_read_pos (to : not null socket_buffer_access;  val : read_pos) return Boolean
  is
  begin
    if to.data = null or else Ada.Streams.Stream_Element_Offset (val) > to.head_first then
      return False;
    end if;

    if Ada.Streams.Stream_Element_Offset (val) = to.head_first then
      return True;
    end if;

    to.head_first :=  Ada.Streams.Stream_Element_Offset (val);

    return True;
  end set_read_pos;


  function string_error return String is
    message_a : aliased char_array (1 .. 260) := (others => char'Val (0));
    length_a  : aliased int :=  int (message_a'Last) - 1;
  begin
    inner_show_error (message_a, length_a);

    return To_Ada (message_a (message_a'First .. message_a'First + size_t (length_a)));
  end string_error;

end adare_net.base;
