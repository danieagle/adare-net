
with Interfaces.C;

with Ada.Streams;

with socket_types;

with System; use System;
with System.Address_To_Access_Conversions;

package adare_net.base
  with Preelaborate
is
  pragma  Assertion_Policy (Check);
  pragma  Unsuppress (All_Checks);

  use Ada;
  use Ada.Streams;
  use Interfaces;
  use Interfaces.C;
  use socket_types;

  pragma Assert (Interfaces.C.char'Size = Ada.Streams.Stream_Element'Size,
    "Actually C.char'Size need be equal to Stream_Element'Size but ask maintainers about this. :-)");

  type ports  is new  Unsigned_16;

  type Address_family is new Unsigned_16;

  type char_array_access is access all char_array;

  type Address_family_label is (any, ipv4, ipv6)
    with Convention => C;

  function get_family (a_family_label : Address_family_label) return Address_family
    with Inline;

  function get_family_label (a_family : Address_family) return Address_family_label
    with Inline;

  function get_family_label (a_family : aliased char_array) return Address_family_label
    with Pre => a_family'Length >= (2 * sizint) + (2 * sizuint16), Inline;

  function get_family_label (a_family : not null char_array_access) return Address_family_label
    with Pre => a_family.all'Length >= (2 * sizint) + (2 * sizuint16), Inline;

  type Address_type is new int;

  type Address_type_label is (tcp, udp)
    with Convention => C;

  function get_address_type (type_label : Address_type_label) return Address_type
    with Inline;

  function get_address_type_label (a_type : Address_type) return Address_type_label
    with Inline;

  type socket_address is limited private;
  type socket_address_access is access all socket_address;

  type socket_addresses is limited private;
  type socket_addresses_access is access all socket_addresses;

  type stream_element_array_access is access all Stream_Element_Array;

  type socket is limited private;
  type socket_access is access all socket;

  type socket_buffer  is new Root_Stream_Type with private; -- a "limited" type
  type socket_buffer_access is access all socket_buffer;

  function get_buffer (from : socket_buffer) return socket_buffer
    with Inline;

  function get_buffer (from : not null socket_buffer_access) return socket_buffer
    with Inline;

  function get_buffer (from : socket_buffer) return socket_buffer_access
    with Inline;

  function get_buffer (from : not null socket_buffer_access) return socket_buffer_access
    with Inline;

  sizint    : constant size_t with Import => True, Convention => C, External_Name => "c_size_int";
  sizuint16 : constant size_t with Import => True, Convention => C, External_Name => "c_size_uint16";

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
    (buffer : socket_buffer) return Integer_64
    with Inline;

  function actual_data_size
    (buffer : not null socket_buffer_access) return Integer_64
    with Inline;

  function create_addresses
    (host_or_ip   : String;
     network_port_or_service  : String;
     Addr_family  : Address_family_label;
     Addr_type    : Address_type_label;
     response     : out socket_addresses;
     quantity     : Unsigned_16 := 9) return Boolean;

  function create_addresses
    (host_or_ip   : String;
     network_port_or_service  : String;
     Addr_family  : Address_family_label;
     Addr_type    : Address_type_label;
     response     : out socket_addresses_access;
     quantity     : Unsigned_16 := 9) return Boolean;

  function create_socket
    (sock_address : socket_address;
     response     : out socket;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
     with Pre => not is_empty (sock_address);

  function create_socket
    (sock_address : socket_address;
     response     : out socket_access;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
     with Pre => not is_empty (sock_address);

  function create_socket
    (sock_address : not null socket_address_access;
     response     : out socket;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
     with Pre => not is_empty (sock_address);

  function create_socket
    (sock_address : not null socket_address_access;
     response     : out socket_access;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
     with Pre => not is_empty (sock_address);

  function create_socket
    (sock_address : in out socket_addresses;
     response     : out socket;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
     with Pre => not is_empty (sock_address);

  function create_socket
    (sock_address : in out socket_addresses;
     response     : out socket_access;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
     with Pre => not is_empty (sock_address);

  function create_socket
    (sock_address : not null socket_addresses_access;
     response     : out socket;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
     with Pre => not is_empty (sock_address);

  function create_socket
    (sock_address : not null socket_addresses_access;
     response     : out socket_access;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
     with Pre => not is_empty (sock_address);

  function connect
    (sock : in out socket) return Boolean
     with Pre => is_initialized (sock) and then
      not (is_binded (sock) or else is_listened (sock) or else is_connected (sock));

  function connect
    (sock : not null socket_access) return Boolean
     with Pre => is_initialized (sock) and then
      not (is_binded (sock) or else is_listened (sock) or else is_connected (sock));

  function wait_connection
    (sock           : socket;
     response       : out socket;
     data_received  : out stream_element_array_access;
     miliseconds_start_timeout  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
      with Pre => is_initialized (sock) and then is_binded (sock) and then is_listened (sock);

  function wait_connection
    (sock           : not null socket_access;
     response       : out socket;
     data_received  : out stream_element_array_access;
     miliseconds_start_timeout  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
      with Pre => is_initialized (sock) and then is_binded (sock) and then is_listened (sock);

  function wait_connection
    (sock           : socket;
     response       : out socket_access;
     data_received  : out stream_element_array_access;
     miliseconds_start_timeout  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
      with Pre => is_initialized (sock) and then is_binded (sock) and then is_listened (sock);

  function wait_connection
    (sock           : not null socket_access;
     response       : out socket_access;
     data_received  : out stream_element_array_access;
     miliseconds_start_timeout  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
      with Pre => is_initialized (sock) and then is_binded (sock) and then is_listened (sock);

  function send_buffer
    (sock : socket;
     data_to_send : aliased in out socket_buffer;
     send_count   : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  function send_buffer
    (sock : socket;
     data_to_send : not null socket_buffer_access;
     send_count   : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  function send_stream
    (sock : socket;
     data_to_send : aliased Stream_Element_Array;
     send_count   : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  function send_stream
    (sock : socket;
     data_to_send : not null stream_element_array_access;
     send_count   : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);


  function send_buffer
    (sock : not null socket_access;
     data_to_send : aliased in out socket_buffer;
     send_count   : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  function send_buffer
    (sock : not null socket_access;
     data_to_send : not null socket_buffer_access;
     send_count   : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  function send_stream
    (sock : not null socket_access;
     data_to_send : aliased Stream_Element_Array;
     send_count   : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  function send_stream
    (sock : not null socket_access;
     data_to_send : not null stream_element_array_access;
     send_count   : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);


  function receive_buffer
    (sock : socket;
     data_to_receive  : in out socket_buffer;
     received_address : out socket_address;
     receive_count    : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  function receive_buffer
    (sock : socket;
     data_to_receive  : not null socket_buffer_access;
     received_address : out socket_address;
     receive_count    : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  function receive_stream
    (sock : socket;
     data_to_receive  : out stream_element_array_access;
     received_address : out socket_address;
     receive_count    : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);


  function receive_buffer
    (sock : not null socket_access;
     data_to_receive  : in out socket_buffer;
     received_address : out socket_address;
     receive_count    : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  function receive_buffer
    (sock : not null socket_access;
     data_to_receive  : not null socket_buffer_access;
     received_address : out socket_address;
     receive_count    : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  function receive_stream
    (sock : not null socket_access;
     data_to_receive  : out stream_element_array_access;
     received_address : out socket_address;
     receive_count    : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);


  function receive_buffer
    (sock : socket;
     data_to_receive  : in out socket_buffer;
     received_address : out socket_address_access;
     receive_count    : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  function receive_buffer
    (sock : socket;
     data_to_receive  : not null socket_buffer_access;
     received_address : out socket_address_access;
     receive_count    : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  function receive_stream
    (sock : socket;
     data_to_receive  : out stream_element_array_access;
     received_address : out socket_address_access;
     receive_count    : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);


  function receive_buffer
    (sock : not null socket_access;
     data_to_receive  : in out socket_buffer;
     received_address : out socket_address_access;
     receive_count    : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  function receive_buffer
    (sock : not null socket_access;
     data_to_receive  : not null socket_buffer_access;
     received_address : out socket_address_access;
     receive_count    : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  function receive_stream
    (sock : not null socket_access;
     data_to_receive  : out stream_element_array_access;
     received_address : out socket_address_access;
     receive_count    : out int;
     miliseconds_start_timeout  : Unsigned_32 := 0; -- default is wait forever.
     miliseconds_next_timeouts  : Unsigned_32 := 0 -- default is wait forever.
    ) return Boolean
    with Pre => is_initialized (sock);

  procedure clear
    (sock_address : in out socket_address)
    with Inline;

  procedure clear
    (sock_address : not null socket_address_access)
    with Inline;

  procedure clear
    (sock_address : in out socket_addresses)
    with Inline;

  procedure clear
    (sock_address : not null socket_addresses_access)
    with Inline;

  procedure clear
    (buffer : in out socket_buffer)
    with Inline;

  procedure clear
    (buffer : not null socket_buffer_access)
    with Inline;

  procedure rewind -- rewind to the first socket_address in socket_addresses
    (sock_address : in out socket_addresses)
    with Pre => not is_empty (sock_address);

  procedure rewind -- rewind to the first socket_address in socket_addresses
    (sock_address : not null socket_addresses_access)
    with Pre => not is_empty (sock_address);

  -- mostly helpers make a deep-copy of data.

  function get_address
    (sock : socket) return socket_address
      with Pre => is_initialized (sock);

  function get_address
    (sock : not null socket_access) return socket_address
      with Pre => is_initialized (sock);

  function get_address
    (sock : socket) return socket_address_access
      with Pre => is_initialized (sock);

  function get_address
    (sock : not null socket_access) return socket_address_access
      with Pre => is_initialized (sock);

  procedure get_address
    (sock   : socket;
     result  : out socket_address)
      with Pre => is_initialized (sock);

  procedure get_address
    (sock   : socket;
     result  : out socket_address_access)
      with Pre => is_initialized (sock);

  procedure get_address
    (sock   : socket_access;
     result  : out socket_address)
      with Pre => is_initialized (sock);

  procedure get_address
    (sock   : socket_access;
     result  : out socket_address_access)
      with Pre => is_initialized (sock);

  function get_address
    (sock_address : in out socket_addresses;
     result       : out socket_address) return Boolean
      with Pre => not is_empty (sock_address);

  function get_address
    (sock_address : not null socket_addresses_access;
     result       : out socket_address) return Boolean
      with Pre => not is_empty (sock_address);

  function get_address
    (sock_address : in out socket_addresses;
     result       : out socket_address_access) return Boolean
      with Pre => not is_empty (sock_address);

  function get_address
    (sock_address : not null socket_addresses_access;
     result       : out socket_address_access) return Boolean
      with Pre => not is_empty (sock_address);

  function get_address_port
    (sock_address : socket_address) return ports
      with Pre => not is_empty (sock_address);

  function get_address_port
    (sock_address : not null socket_address_access) return ports
      with Pre => not is_empty (sock_address);

  function get_address_port
    (sock_address : socket_address) return String
      with Pre => not is_empty (sock_address);

  function get_address_port
    (sock_address : not null socket_address_access) return String
      with Pre => not is_empty (sock_address);

  function get_address
    (sock_address : socket_address) return String
      with Pre => not is_empty (sock_address);

  function get_address
    (sock_address : not null socket_address_access) return String
      with Pre => not is_empty (sock_address);

  function is_empty
    (sock_address : socket_address) return Boolean
  with Inline;

  function is_empty
    (sock_address : not null socket_address_access) return Boolean
  with Inline;

  function is_empty
    (sock_address : socket_addresses) return Boolean
  with Inline;

  function is_empty
    (sock_address : not null socket_addresses_access) return Boolean
  with Inline;

  procedure close (sock : in out socket)
  with Pre => is_initialized (sock);

  procedure close (sock : not null socket_access)
  with Pre => is_initialized (sock);

  function is_initialized
    (sock : socket) return Boolean
  with Inline;

  function is_initialized
    (sock : not null socket_access) return Boolean
  with Inline;

  function is_connected
    (sock : socket) return Boolean
  with Inline;

  function is_connected
    (sock : not null socket_access) return Boolean
  with Inline;

  function is_binded
    (sock : socket) return Boolean
  with Inline;

  function is_binded
    (sock : not null socket_access) return Boolean
  with Inline;

  function is_listened
    (sock : socket) return Boolean
  with Inline;

  function is_listened
    (sock : not null socket_access) return Boolean
  with Inline;

  function string_error return String;


  function get_socket
    (sock : socket) return socket_type
  with Inline;

  function get_socket
    (sock : not null socket_access) return socket_type
  with Inline;


  function get_socket
    (sock : socket) return socket
  with Inline;

  function get_socket
    (sock : socket) return socket_access
  with Inline;

  function get_socket
    (sock : not null socket_access) return socket
  with Inline;

  function get_socket
    (sock : not null socket_access) return socket_access
  with Inline;

  function get_raw_length
    (from : socket_addresses) return size_t
    with Pre => not is_empty (from), Inline;

  function get_raw_length
    (from : not null socket_addresses_access) return size_t
    with Pre => not is_empty (from), Inline;

  function get_raw_left_length
    (from : socket_addresses) return size_t
    with Pre => not is_empty (from), Inline;

  function get_raw_left_length
    (from : not null socket_addresses_access) return size_t
    with Pre => not is_empty (from), Inline;


  function get_address_type
    (from : not null char_array_access) return Address_type_label
    with Pre => from.all'Length >= sizint;

  function get_address_type
    (from : aliased in char_array) return Address_type_label
    with Pre => from'Length >= sizint;

  function get_address_type
    (from : socket) return Address_type_label
    with Pre => is_initialized (from);

  function get_address_type
    (from : not null socket_access) return Address_type_label
    with Pre => is_initialized (from);

  function get_address_type
    (from : socket_address) return Address_type_label
    with Pre => not is_empty (from);

  function get_address_type
    (from : not null socket_address_access) return Address_type_label
    with Pre => not is_empty (from);

  function get_address_type
    (from : socket_addresses) return Address_type_label
    with Pre => not is_empty (from) and then get_raw_length (from) >= sizint;

  function get_address_type
    (from : not null socket_addresses_access) return Address_type_label
    with Pre => not is_empty (from) and then get_raw_length (from) >= sizint;


  function get_address_type
    (from : not null char_array_access) return String
    with Pre => from.all'Length >= sizint;

  function get_address_type
    (from : aliased in char_array) return String
    with Pre => from'Length >= sizint;

  function get_address_type
    (from : socket) return String
    with Pre => is_initialized (from);

  function get_address_type
    (from : not null socket_access) return String
    with Pre => is_initialized (from);

  function get_address_type
    (from : socket_address) return String
    with Pre => not is_empty (from);

  function get_address_type
    (from : not null socket_address_access) return String
    with Pre => not is_empty (from);

  function get_address_type
    (from : socket_addresses) return String
    with Pre => not is_empty (from) and then get_raw_length (from) >= sizint;

  function get_address_type
    (from : not null socket_addresses_access) return String
    with Pre => not is_empty (from) and then get_raw_length (from) >= sizint;


  function get_address_protocol
    (from : not null char_array_access) return int
    with Pre => from.all'Length >= 2 * sizint;

  function get_address_protocol
    (from : aliased in char_array) return int
    with Pre => from'Length >= 2 * sizint;

  function get_address_protocol
    (from : socket) return int
    with Pre => is_initialized (from);

  function get_address_protocol
    (from : not null socket_access) return int
    with Pre => is_initialized (from);

  function get_address_protocol
    (from : socket_address) return int
    with Pre => not is_empty (from);

  function get_address_protocol
    (from : not null socket_address_access) return int
    with Pre => not is_empty (from);

  function get_address_protocol
    (from : socket_addresses) return int
    with Pre => not is_empty (from) and then get_raw_length (from) >= 2 * sizint;

  function get_address_protocol
    (from : not null socket_addresses_access) return int
    with Pre => not is_empty (from) and then get_raw_length (from) >= 2 * sizint;


  function get_address_length
    (from : not null char_array_access) return Unsigned_16
    with Pre => from.all'Length >= (2 * sizint) + sizuint16;

  function get_address_length
    (from : aliased in char_array) return Unsigned_16
    with Pre => from'Length >= (2 * sizint) + sizuint16;

  function get_address_length
    (from : socket) return Unsigned_16
    with Pre => is_initialized (from);

  function get_address_length
    (from : not null socket_access) return Unsigned_16
    with Pre => is_initialized (from);

  function get_address_length
    (from : socket_address) return Unsigned_16
    with Pre => not is_empty (from);

  function get_address_length
    (from : not null socket_address_access) return Unsigned_16
    with Pre => not is_empty (from);

  function get_address_length
    (from : socket_addresses) return Unsigned_16
    with Pre => not is_empty (from) and then get_raw_length (from) >= sizuint16 + (2 * sizint);

  function get_address_length
    (from : not null socket_addresses_access) return Unsigned_16
    with Pre => not is_empty (from) and then get_raw_length (from) >= sizuint16 + (2 * sizint);


  function get_address_and_family_address
    (from : not null char_array_access) return Address
    with Pre => from.all'Length >= (2 * sizint) + (2 * sizuint16);

  function get_address_and_family_address
    (from : aliased in char_array) return Address
    with Pre => from'Length >= (2 * sizint) + (2 * sizuint16);

  function get_address_and_family_address
    (from : socket) return Address
    with Pre => is_initialized (from);

  function get_address_and_family_address
    (from : not null socket_access) return Address
    with Pre => is_initialized (from);

  function get_address_and_family_address
    (from : socket_address) return Address
    with Pre => not is_empty (from);

  function get_address_and_family_address
    (from : not null socket_address_access) return Address
    with Pre => not is_empty (from);

  function get_address_and_family_address
    (from : socket_addresses) return Address
    with Pre => not is_empty (from) and then get_raw_length (from) >= (2 * sizuint16) + (2 * sizint);

  function get_address_and_family_address
    (from : not null socket_addresses_access) return Address
    with Pre => not is_empty (from) and then get_raw_length (from) >= (2 * sizuint16) + (2 * sizint);


  procedure set_address_length
    (from : not null char_array_access;
     length : Unsigned_16)
    with Pre => from.all'Length >= (2 * sizint) + sizuint16;

  procedure set_address_length
    (from : aliased in out char_array;
     length : Unsigned_16)
    with Pre => from'Length >= (2 * sizint) + sizuint16;

  procedure set_address_length
    (from : in out socket;
     length : Unsigned_16)
    with Pre => is_initialized (from);

  procedure set_address_length
    (from : not null socket_access;
     length : Unsigned_16)
    with Pre => is_initialized (from);

  procedure set_address_length
    (from : in out socket_address;
     length : Unsigned_16)
    with Pre => not is_empty (from);

  procedure set_address_length
    (from : not null socket_address_access;
     length : Unsigned_16)
    with Pre => not is_empty (from);

  procedure set_address_length
    (from : in out socket_addresses;
     length : Unsigned_16)
    with Pre => not is_empty (from) and then get_raw_length (from) >= sizuint16 + (2 * sizint);

  procedure set_address_length
    (from : not null socket_addresses_access;
     length : Unsigned_16)
    with Pre => not is_empty (from) and then get_raw_length (from) >= sizuint16 + (2 * sizint);


  function get_family_label (from : socket) return Address_family_label
    with Pre  => is_initialized (from);

  function get_family_label (from : not null socket_access) return Address_family_label
    with Pre  => is_initialized (from);

  function get_family_label (from : socket_address) return Address_family_label
    with Pre  => not is_empty (from);

  function get_family_label (from : not null socket_address_access) return Address_family_label
    with Pre  => not is_empty (from);

  function get_family_label (from : socket_addresses) return Address_family_label
    with Pre  => not is_empty (from) and then get_raw_length (from) >= (2 * sizint) + (2 * sizuint16);

  function get_family_label (from : not null socket_addresses_access) return Address_family_label
    with Pre  => not is_empty (from) and then get_raw_length (from) >= (2 * sizint) + (2 * sizuint16);


  function get_family_label (from : socket) return String
    with Pre  => is_initialized (from);

  function get_family_label (from : not null socket_access) return String
    with Pre  => is_initialized (from);

  function get_family_label (from : socket_address) return String
    with Pre  => not is_empty (from);

  function get_family_label (from : not null socket_address_access) return String
    with Pre  => not is_empty (from);

  function get_family_label (from : socket_addresses) return String
    with Pre  => not is_empty (from) and then get_raw_length (from) >= (2 * sizint) + (2 * sizuint16);

  function get_family_label (from : not null socket_addresses_access) return String
    with Pre  => not is_empty (from) and then get_raw_length (from) >= (2 * sizint) + (2 * sizuint16);


  type write_pos is private;

  type read_pos is private;


  function get_write_pos (from : socket_buffer) return write_pos
    with Pre => actual_data_size (from) > 0, Inline;

  function get_write_pos (from : not null socket_buffer_access) return write_pos
    with Pre => actual_data_size (from) > 0, Inline;

  function get_read_pos (from : socket_buffer) return read_pos
    with Pre => actual_data_size (from) > 0, Inline;

  function get_read_pos (from : not null socket_buffer_access) return read_pos
    with Pre => actual_data_size (from) > 0, Inline;


  function set_write_pos (to : in out socket_buffer;  val : write_pos) return Boolean;
  function set_write_pos (to : not null socket_buffer_access;  val : write_pos) return Boolean;

  function set_read_pos (to : in out socket_buffer;  val : read_pos) return Boolean;
  function set_read_pos (to : not null socket_buffer_access;  val : read_pos) return Boolean;

private

  type write_pos is new Ada.Streams.Stream_Element_Offset;

  type read_pos is new Ada.Streams.Stream_Element_Offset;


  procedure reuse_address
    (sock  : socket);

  procedure reuse_address
    (sock  : socket_type);

  procedure reuse_address
    (sock  : not null socket_access);

  function max_data_length
    (buffer : socket_buffer) return Stream_Element_Offset
    with Inline;

  function max_data_length
    (buffer : not null socket_buffer_access) return Stream_Element_Offset
    with Inline;

  function data_tail_length
    (buffer : socket_buffer) return Stream_Element_Offset
    with Inline;

  function data_tail_length
    (buffer : not null socket_buffer_access) return Stream_Element_Offset
    with Inline;

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
      sin_family  : Unsigned_16 := 0; -- bsd unsigned_8 ?
      sin_port    : Unsigned_16 := 0;
      sin_addr    : in_addr := (others => <>);
      sin_zero    : char_array (1 .. 8) := (others => char'Val (0));
    end record
      with Convention => C;

  type sockaddr_in6 is
    record
      sin6_family   : Unsigned_16 := 0; -- bsd unsigned_8 ?
      sin6_port     : Unsigned_16 := 0;
      sin6_flowinfo : Unsigned_32 := 0;
      sin6_addr     : in6_addr    := (others => <>);
      sin6_scope_id : Unsigned_32 := 0;
    end record
      with Convention => C;

  type sockaddr_in_bsd is
    record
      sin_len     : Unsigned_8  := 0;
      sin_family  : Unsigned_8  := 0;
      sin_port    : Unsigned_16 := 0;
      sin_addr    : in_addr := (others => <>);
      sin_zero    : char_array (1 .. 8) := (others => char'Val (0));
    end record
      with Convention => C;

  type sockaddr_in6_bsd is
    record
      sin6_len      : Unsigned_8  := 0;
      sin6_family   : Unsigned_8  := 0;
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
  end record;

  type socket is
    record
      storage   : socket_address;
      sock      : socket_type  := 0; -- /= 0 => socket() initialized
      connected : Boolean :=  False;
      binded    : Boolean :=  False;
      listened  : Boolean :=  False;
  end record;

  type socket_buffer is new Root_Stream_Type with
    record
      data  : aliased stream_element_array_access := null;
      head_first, tail_end  : Stream_Element_Count := 0;
    end record
      with Preelaborable_initialization;

  package a_int is new System.Address_To_Access_Conversions (int);
  package a_uint16 is new System.Address_To_Access_Conversions (Unsigned_16);
  package a_uint8 is new System.Address_To_Access_Conversions (Unsigned_8);

  package a_sockaddr_in6 is new System.Address_To_Access_Conversions (sockaddr_in6);
  package a_sockaddr_in4 is new System.Address_To_Access_Conversions (sockaddr_in);
  package a_sockaddr_in6_bsd is new System.Address_To_Access_Conversions (sockaddr_in6_bsd);
  package a_sockaddr_in4_bsd is new System.Address_To_Access_Conversions (sockaddr_in_bsd);

end adare_net.base;
