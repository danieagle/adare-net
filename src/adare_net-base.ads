
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

  function a_family (a_family_label : Address_family_label) return Address_family
    with Inline;

  function a_family_label (a_family : Address_family) return Address_family_label
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

  type socket is limited private;
  type socket_access is access all socket;

  type socket_buffer  is new Root_Stream_Type with private;
  type socket_buffer_access is access all socket_buffer;


  sizint : constant Integer_8 with Import => True, Convention => C, External_Name => "c_size_int";
  sizuint16 : constant Integer_8 with Import => True, Convention => C, External_Name => "c_size_uint16";

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
     backlog        : Unsigned_16 := 10) return Boolean
     with Pre => not is_empty (sock_address);

  function create_socket
    (sock_address : aliased in out socket_addresses;
     response     : aliased out socket;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
     with Pre => not is_empty (sock_address);

  function connect
    (sock : aliased in out socket) return Boolean
     with Pre => is_initialized (sock) and then
      not (is_binded (sock) or else is_listened (sock) or else is_connected (sock));

  function wait_connection
    (sock           : aliased in out socket;
     response       : aliased out socket;
     data_received  : aliased out stream_element_array_access;
     miliseconds_start_timeout  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
      with Pre => is_initialized (sock) and then is_binded (sock) and then is_listened (sock);

  function send_buffer
    (sock : aliased socket;
     data_to_send : aliased in out socket_buffer;
     send_count   : aliased out ssize_t;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  function send_stream
    (sock : aliased socket;
     data_to_send : aliased Stream_Element_Array;
     send_count   : aliased out ssize_t;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  function receive_buffer
    (sock : aliased socket;
     data_to_receive  : aliased in out socket_buffer;
     received_address : aliased out socket_address;
     receive_count    : aliased out ssize_t;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  function receive_stream
    (sock : aliased socket;
     data_to_receive  : aliased out stream_element_array_access;
     received_address : aliased out socket_address;
     receive_count    : aliased out ssize_t;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  procedure clear
    (sock_address : aliased in out socket_address);

  procedure clear -- remove all stored socket_address's
    (sock_address : aliased in out socket_addresses);

  procedure clear
    (buffer : aliased in out socket_buffer);

  procedure rewind -- rewind to the first socket_address in socket_addresses
    (sock_address : aliased in out socket_addresses)
    with Pre => not is_empty (sock_address);

  function get_address
    (sock : aliased in socket) return socket_address
      with Pre => not is_initialized (sock);

  function get_address
    (sock : not null socket_access) return socket_address
      with Pre => not is_initialized (sock.all);

  procedure get_address
    (sock   : aliased in socket;
    result  : aliased out socket_address);

  function get_address
    (sock_address : aliased in out socket_addresses;
     result : aliased out socket_address) return Boolean
      with Pre => not is_empty (sock_address);

  function get_address_port
    (sock_address : aliased in socket_address) return ports
      with Pre => not is_empty (sock_address);

  function get_address_port
    (sock_address : aliased in socket_address) return String
      with Pre => not is_empty (sock_address);

  function get_address
    (sock_address : aliased in socket_address) return String
      with Pre => not is_empty (sock_address);

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

  function get_socket
    (sock : aliased in socket) return socket;

  function get_socket
    (sock : socket_access) return socket;

  procedure get_socket
    (sock : socket_access;
     result : aliased out socket_access);

  procedure get_socket
    (sock : socket_access;
     result : aliased out socket);

  procedure get_socket
    (sock : aliased in socket;
     result : aliased out socket_access);

  procedure get_socket
    (sock : aliased in socket;
     result : aliased out socket);



  function get_raw_length
    (from : aliased in socket_addresses) return size_t
    with Pre => not is_empty (from);

  function get_address_type
    (from : not null char_array_access) return Address_type_label
    with Pre => from.all'Length >= size_t (sizint);

  function get_address_type
    (from : aliased in char_array) return Address_type_label
    with Pre => from'Length >= size_t (sizint);

  function get_address_type
    (from : aliased in socket) return Address_type_label
    with Pre => is_initialized (from);

  function get_address_type
    (from : aliased in socket_address) return Address_type_label
    with Pre => not is_empty (from);

  function get_address_type
    (from : aliased in socket_addresses) return Address_type_label
    with Pre => not is_empty (from) and then get_raw_length (from) >= size_t (sizint);


  function get_address_type
    (from : not null char_array_access) return String
    with Pre => from.all'Length >= size_t (sizint);

  function get_address_type
    (from : aliased in char_array) return String
    with Pre => from'Length >= size_t (sizint);

  function get_address_type
    (from : aliased in socket) return String
    with Pre => is_initialized (from);

  function get_address_type
    (from : aliased in socket_address) return String
    with Pre => not is_empty (from);

  function get_address_type
    (from : aliased in socket_addresses) return String
    with Pre => not is_empty (from) and then get_raw_length (from) >= size_t (sizint);


  function get_address_protocol
    (from : not null char_array_access) return int
    with Pre => from.all'Length >= 2 * size_t (sizint);

  function get_address_protocol
    (from : aliased in char_array) return int
    with Pre => from'Length >= 2 * size_t (sizint);

  function get_address_protocol
    (from : aliased in socket) return int
    with Pre => is_initialized (from);

  function get_address_protocol
    (from : aliased in socket_address) return int
    with Pre => not is_empty (from);

  function get_address_protocol
    (from : aliased in socket_addresses;
     address_start_at : size_t) return int
    with Pre => not is_empty (from) and then get_raw_length (from) >= address_start_at + (2 * size_t (sizint));


  function get_address_length
    (from : not null char_array_access) return Unsigned_16
    with Pre => from.all'Length >= (2 * size_t (sizint)) + size_t (sizuint16);

  function get_address_length
    (from : aliased in char_array) return Unsigned_16
    with Pre => from'Length >= (2 * size_t (sizint)) + size_t (sizuint16);

  function get_address_length
    (from : aliased in socket) return Unsigned_16
    with Pre => is_initialized (from);

  function get_address_length
    (from : aliased in socket_address) return Unsigned_16
    with Pre => not is_empty (from);

  function get_address_length
    (from : aliased in socket_addresses;
     address_start_at : size_t) return Unsigned_16
    with Pre => not is_empty (from) and then get_raw_length (from) >= address_start_at + size_t (sizuint16) + (2 * size_t (sizint));


  function get_address_and_family
    (from : not null char_array_access) return Address
    with Pre => from.all'Length >= (2 * size_t (sizint)) + size_t (sizuint16);

  function get_address_and_family
    (from : aliased in char_array) return Address
    with Pre => from'Length >= (2 * size_t (sizint)) + size_t (sizuint16);

  function get_address_and_family
    (from : aliased in socket) return Address
    with Pre => is_initialized (from);

  function get_address_and_family
    (from : aliased in socket_address) return Address
    with Pre => not is_empty (from);

  function get_address_and_family
    (from : aliased in socket_addresses;
     address_start_at : size_t) return Address
    with Pre => not is_empty (from) and then get_raw_length (from) >= address_start_at + size_t (sizuint16) + (2 * size_t (sizint));


  procedure set_address_length
    (from : not null char_array_access;
     length : Unsigned_16)
    with Pre => from.all'Length >= (2 * size_t (sizint)) + size_t (sizuint16);

  procedure set_address_length
    (from : aliased in char_array;
     length : Unsigned_16)
    with Pre => from'Length >= (2 * size_t (sizint)) + size_t (sizuint16);

  procedure set_address_length
    (from : aliased in socket;
     length : Unsigned_16)
    with Pre => is_initialized (from);

  procedure set_address_length
    (from : aliased in socket_address;
     length : Unsigned_16)
    with Pre => not is_empty (from);

  procedure set_address_length
    (from : aliased in socket_addresses;
     address_start_at : size_t;
     length : Unsigned_16)
    with Pre => not is_empty (from) and then get_raw_length (from) >= address_start_at + size_t (sizuint16) + (2 * size_t (sizint));


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

end adare_net.base;
