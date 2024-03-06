
with Interfaces.C;

with Ada.Streams;

with socket_types;

with System; use System;
with System.Address_To_Access_Conversions;

package adare_net.base
  with Preelaborate
is
  pragma  Assertion_Policy (Check);

  use Ada;
  use Ada.Streams;
  use Interfaces;
  use Interfaces.C;
  use socket_types;

  pragma Assert (Interfaces.C.char'Size = Ada.Streams.Stream_Element'Size,
    "Actually C.char'Size need be equal to Stream_Element'Size but ask maintainers about this. :-)");


  type ports  is new  Unsigned_16;

  type Address_family is new Unsigned_16;

  type Address_family_label is (any, ipv4, ipv6)
    with Convention => C;

  function family (family_label : Address_family_label) return Address_family
    with Inline;

  function family_label (a_family : Address_family) return Address_family_label
    with Inline;


  type Address_type is new int;

  type Address_type_label is (tcp, udp)
    with Convention => C;

  function a_type (type_label : Address_type_label) return Address_type
    with Inline;

  function a_type_label (a_type : Address_type) return Address_type_label
    with Inline;


  type socket_address is limited private;
  type socket_address_access is access all socket_address;

  type socket_addresses is limited private;
  type socket_addresses_access is access all socket_addresses;

  type stream_element_array_access is access all Stream_Element_Array;
  type char_array_access is access all char_array;

  type socket is private;
  type socket_access is access all socket;

  type socket_buffer  is new Root_Stream_Type with private;
  type socket_buffer_access is access all socket_buffer;


  overriding
  procedure Read
    (Stream : in out socket_buffer;
     Item   : out Stream_Element_Array;
     Last   : out Stream_Element_Offset);

  overriding
  procedure Write
    (Stream : in out socket_buffer;
     Item   : in Stream_Element_Array);

  function actual_data_size
    (buffer : aliased socket_buffer) return Integer_64;

  function create_addresses
    (host_or_ip   : String;
     network_port_or_service  : String;
     Addr_family  : Address_family_label;
     Addr_type    : Address_type_label;
     response     : aliased out socket_addresses;
     quantity     : Unsigned_16 := 9) return Boolean;

  function create_socket
    (sock_address : aliased in socket_address;
     response     : aliased out socket;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean;

  function create_socket
    (sock_address : aliased in out socket_addresses;
     response     : out socket;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean;


  function connect
    (sock : aliased in out socket) return Boolean
     with Pre => is_initialized (sock);

  --  function wait_connection
  --    (sock           : aliased in out socket;
  --     response       : out socket;
  --     data_received  : aliased out stream_element_array_access;
  --     miliseconds_start_timeout  : Unsigned_32 := 0 -- default is wait forever.
  --    ) return Boolean
  --      with Pre => is_initialized (sock) and then is_binded (sock) and then is_listened (sock);

  --  function send_buffer
  --    (sock : aliased socket;
  --     data_to_send : aliased in out socket_buffer;
  --     send_count   : aliased out ssize_t;
  --     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
  --     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
  --    ) return Boolean
  --    with Pre => is_initialized (sock);

  --  function send_stream
  --    (sock : aliased socket;
  --     data_to_send : aliased Stream_Element_Array;
  --     send_count   : aliased out ssize_t;
  --     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
  --     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
  --    ) return Boolean
  --    with Pre => is_initialized (sock);

  --  function receive_buffer
  --    (sock : aliased socket;
  --     data_to_receive  : aliased in out socket_buffer;
  --     received_address : aliased out socket_address;
  --     receive_count    : aliased out ssize_t;
  --     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
  --     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
  --    ) return Boolean
  --    with Pre => is_initialized (sock);

  --  function receive_stream
  --    (sock : aliased socket;
  --     data_to_receive  : aliased out stream_element_array_access;
  --     received_address : aliased out socket_address;
  --     receive_count    : aliased out ssize_t;
  --     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
  --     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
  --    ) return Boolean
  --    with Pre => is_initialized (sock);

  --  procedure clear
  --    (sock_address : aliased in out socket_address);

  --  procedure clear -- remove all stored socket_address's
  --    (sock_address : aliased in out socket_addresses);

  --  procedure clear
  --    (buffer : aliased in out socket_buffer);

  procedure rewind -- rewind to the first socket_address in socket_addresses
    (sock_address : aliased in out socket_addresses);

  --  function get_address
  --    (sock : aliased in socket) return socket_address;

  function get_address
    (sock_address : aliased in out socket_addresses;
     result : aliased out socket_address) return Boolean;

  --  function get_address_port
  --    (sock_address : aliased in socket_address) return ports;

  --  function get_address_port
  --    (sock_address : aliased in socket_address) return String;

  --  function get_address
  --    (sock_address : aliased in socket_address) return String;

  function is_empty
    (sock_address : aliased in socket_address) return Boolean;

  function is_empty
    (sock_address : aliased in socket_addresses) return Boolean;

  procedure close (sock : in out socket);

  function is_initialized
    (sock : aliased in socket) return Boolean;

  function is_connected
    (sock : aliased in socket) return Boolean;

  function is_binded
    (sock : aliased in socket) return Boolean;

  function is_listened
    (sock : aliased in socket) return Boolean;

  function string_error return String;

  function get_socket
    (sock : aliased in socket) return socket_type;

private

  procedure reuse_address
    (sock  : aliased in out socket);

  function max_data_length
    (buffer : aliased socket_buffer) return Stream_Element_Offset;

  function data_tail_length
    (buffer : aliased socket_buffer) return Stream_Element_Offset;

  type in_addr is
    record
      s_addr  : Unsigned_32 := 0;
    end record
      with Convention => C;

  type in6_addr is
    record
      s6_addr : char_array (1 .. 16) := (others => char'Val (0));
    end record
      with Convention => C;

  type sockaddr_in is
    record
      sin_family  : Unsigned_16 := 0;
      sin_port    : Unsigned_16 := 0;
      sin_addr    : in_addr := (others => <>);
      sin_zero    : char_array (1 .. 8) := (others => char'Val (0));
    end record
      with Convention => C;

  type sockaddr_in6 is
    record
      sin6_family   : Unsigned_16 := 0;
      sin6_port     : Unsigned_16 := 0;
      sin6_flowinfo : Unsigned_32 := 0;
      sin6_addr     : in6_addr    := (others => <>);
      sin6_scope_id : Unsigned_32 := 0;
    end record
      with Convention => C;

  type socket_address  is
    record
      stor  : char_array_access := null;
    end record
      with Convention => C;

  type socket_addresses is record
    stor  : char_array_access := null;
    next_cursor : size_t  := 0;
    initialized : Boolean := False;
  end record
      with Convention => C;

  type socket is
    record
      storage   : socket_address;
      sock      : socket_type  := 0; -- /= 0 => socket() initialized
      connected : Boolean :=  False;
      binded    : Boolean :=  False;
      listened  : Boolean :=  False;
  end record
    with Convention => C;

  type socket_buffer is new Root_Stream_Type with
    record
      data  : aliased stream_element_array_access := null;
      head_first, tail_end  : Stream_Element_Count := 0;
    end record
      with Preelaborable_initialization;

  package a_int is new System.Address_To_Access_Conversions (int);
  package a_uint16 is new System.Address_To_Access_Conversions (Unsigned_16);

  package a_sockaddr_in6 is new System.Address_To_Access_Conversions (sockaddr_in6);
  package a_sockaddr_in4 is new System.Address_To_Access_Conversions (sockaddr_in);


  sizint : constant Integer_8 with Import => True, Convention => C, External_Name => "c_size_int";
  sizuint16 : constant Integer_8 with Import => True, Convention => C, External_Name => "c_size_uint16";

  function pos_protocol (pos_actual : size_t) return size_t
  is (pos_actual) with Inline;

  function pos_addrlen (pos_actual : size_t) return size_t
  is (pos_actual + size_t (sizint)) with Inline;

  function pos_family (pos_actual : size_t) return size_t
  is (pos_actual + size_t (sizint) + size_t (sizuint16)) with Inline;

  function pos_address (pos_actual : size_t) return size_t renames pos_family;

end adare_net.base;
