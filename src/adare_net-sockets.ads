
with Interfaces.C;

with Ada.Streams;

with socket_types;

package adare_net.sockets
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

  any : constant Address_family with Import => True, Convention => C, external_name => "c_af_unspec";
  v4  : constant Address_family with Import => True, Convention => C, external_name => "c_af_inet";
  v6  : constant Address_family with Import => True, Convention => C, external_name => "c_af_inet6";

  type Address_type is new int;

  tcp : constant Address_type with Import => True, Convention => C, external_name => "c_sock_stream";
  udp : constant Address_type with Import => True, Convention => C, external_name => "c_sock_dgram";

  v6_str_length : constant int with Import => True, Convention => C, External_Name => "c_v6_str_length";
  v4_str_length : constant int with Import => True, Convention => C, External_Name => "c_v4_addrstrlen";

  type addresses  is  private
    with Preelaborable_initialization;

  null_addresses  : constant addresses;

  type addresses_access is access all addresses;

  type addresses_list is array (Positive range <>) of aliased addresses
     with Preelaborable_initialization;

  type addresses_list_access is access all addresses_list;

  type stream_element_array_access is access all Stream_Element_Array;

  procedure init_addresses
    (ip_or_host   : String;
     port         : String;
     ai_socktype  : Address_type;
     ai_family    : Address_family;
     addr         : out addresses_list_access
    )
    with pre => (ai_socktype = tcp or else ai_socktype = udp) and then (ai_family = any or else ai_family = v4 or else ai_family = v6);

  procedure init_addresses
    (ip_or_host : String;
     port       : String;
     ai_socktype  : Address_type;
     ai_family    : Address_family;
     addr         : out addresses_access
    )
    with pre => (ai_socktype = tcp or else ai_socktype = udp) and then (ai_family = any or else ai_family = v4 or else ai_family = v6);


  function get_addresses
    (show  : not null addresses_access) return String
     with Pre =>  initialized (show);

  function get_port
    (show  : not null addresses_access) return String
     with Pre =>  initialized (show);

  function get_address_and_port
    (show  : not null addresses_access) return String
     with Pre =>  initialized (show);

  function get_address_family
    (show  : not null addresses_access) return Address_family
     with Pre =>  initialized (show);

  function is_null
    (addr  : not null addresses_access) return Boolean;

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


  type rewind_t is private;


  function get_read_rewind
    (from : not null socket_buffer_access) return rewind_t
     with pre => not is_empty (from);

  function get_current_reading_position -- better name :-)
    (from : not null socket_buffer_access) return rewind_t renames get_read_rewind;

  function read_rewind
    (who : not null socket_buffer_access;
     rewind_at  : rewind_t) return Boolean
     with pre => not is_empty (who);

  function rewind_reading_at -- better name :-)
    (who : not null socket_buffer_access;
    rewind_at : rewind_t) return Boolean renames read_rewind;

  type socket is private
    with Preelaborable_initialization;

  type socket_access is access all socket;

  function init_socket
    (sock  : out socket_access;
     addr  : not null addresses_access) return Boolean
     with  pre => initialized (addr);

  function init_socket
    (sock  : out socket_access;
     addr  : not null addresses_list_access) return Boolean
     with  pre => initialized (addr);

  procedure reuse_address
    (sock  : not null socket_access)
     with  pre => (initialized (sock)) and then (not binded (sock));

  function bind
    (sock  : not null socket_access) return Boolean
     with  pre => (initialized (sock)) and then (not binded (sock)) and then (not connected (sock)) and then (not listened (sock));

  function listen
    (sock     : not null socket_access;
     backlog  : int) return Boolean
     with  pre => (binded (sock)) and then (not listened (sock)) and then backlog >= 0;

  function accept_socket
    (sock     : not null socket_access;
     new_sock : out socket_access) return Boolean
    with Pre  =>  initialized (sock) and then listened (sock);

  function connect
    (sock  : not null socket_access) return Boolean
     with  pre => (initialized (sock)) and then (not binded (sock)) and then (not connected (sock)) and then (not listened (sock));

  procedure close
    (sock  : not null socket_access)
     with  pre => initialized (sock);

  procedure clean
    (sock  : not null socket_access);

  function send
    (sock     : not null socket_access;
     buffer   : not null stream_element_array_access) return ssize_t
     with  pre => initialized (sock);

  function send
    (sock     : not null socket_access;
     buffer   : not null socket_buffer_access) return ssize_t
     with  pre => initialized (sock);

  function sendto
    (sock     : not null socket_access;
     send_to  : not null addresses_access;
     buffer   : not null stream_element_array_access) return ssize_t
     with  pre => initialized (sock) and then initialized (send_to);

  function sendto
    (sock     : not null socket_access;
     send_to  : not null addresses_access;
     buffer   : not null socket_buffer_access) return ssize_t
     with  pre => initialized (sock) and then initialized (send_to);

  function receive
    (sock     : not null socket_access;
     buffer   : out stream_element_array_access;
     max_len  : Stream_Element_Count := 1500) return ssize_t
     with  pre => initialized (sock);

  function receive
    (sock     : not null socket_access;
     buffer   : not null socket_buffer_access;
     max_len  : Stream_Element_Count := 1500) return ssize_t
     with  pre => initialized (sock);

  function receive_from
    (sock     : not null socket_access;
     buffer   : out stream_element_array_access;
     from     : out addresses_access;
     max_len  : Stream_Element_Count := 1500) return ssize_t
     with  pre => initialized (sock);

  function receive_from
    (sock     : not null socket_access;
     buffer   : not null socket_buffer_access;
     from     : out addresses_access;
     max_len  : Stream_Element_Count := 1500) return ssize_t
     with  pre => initialized (sock);

  function get_sock
    (sock : not null socket_access) return socket_type
     with pre => initialized (sock);

  function get_addresses
    (sock : not null socket_access) return addresses
     with pre => initialized (sock);

  function get_addresses
    (sock : not null socket_access) return addresses_access
     with pre => initialized (sock);

  function initialized
    (sock  : not null socket_access) return Boolean;

  function initialized
    (addr  : not null addresses_list_access) return Boolean;

  function initialized
    (addr  : not null addresses_access) return Boolean;

  function connected
    (sock  : not null socket_access) return Boolean;

  function binded
    (sock  : not null socket_access) return Boolean;

  function listened
    (sock  :  not null socket_access) return Boolean;

  function is_empty
    (buffer : not null socket_buffer_access) return Boolean;

  function actual_data_size
    (buffer : not null socket_buffer_access) return Integer_64;

  function actual_data_size
    (buffer : socket_buffer) return Integer_64;

  function get_buffer_init
    (buffer : not null socket_buffer_access) return socket_buffer;

  function get_buffer_init
    (buffer : not null socket_buffer_access) return socket_buffer_access;

  procedure clean
    (buffer : not null socket_buffer_access);

  function add_raw
    (buffer : not null socket_buffer_access;
     raw    : not null stream_element_array_access) return Boolean;

  function get_raw
    (buffer : not null socket_buffer_access) return Stream_Element_Array;

  function string_error return String;


private

  type rewind_t is new Stream_Element_Offset;

  function max_data_length
    (buffer : socket_buffer) return Stream_Element_Offset;

  function data_tail_length
    (buffer : socket_buffer) return Stream_Element_Offset;

  type in_addr is
    record
      s_addr  : Unsigned_32;
    end record
      with Convention => C;

  type in6_addr is
    record
      s6_addr : char_array (1 .. 16);
    end record
      with Convention => C;

  type sockaddr_in is
    record
      sin_family  : Unsigned_16;
      sin_port    : Unsigned_16;
      sin_addr    : in_addr;
      sin_zero    : char_array (1 .. 8);
    end record
      with Convention => C;

  type sockaddr_in6 is
    record
      sin6_family : Unsigned_16;
      sin6_port   : Unsigned_16;
      sin6_flowinfo : Unsigned_32;
      sin6_addr     : in6_addr;
      sin6_scope_id : Unsigned_32;
    end record
      with Convention => C;

  type sockaddr_storage is
    record
      ss_family : aliased Unsigned_16 := 0;
      padding   : aliased char_array (1 .. 132) := (others => char'Val (0));
    end record
      with Convention => C, Preelaborable_initialization;

  type addresses  is
    record
      storage   : aliased sockaddr_storage;
      socktype  : aliased int := 0;
      protocol  : aliased int := 0;
      address_length  : aliased int := 0;
    end record
    with Convention => C;

  type socket is
    record
      storage   : aliased addresses;
      sock      : aliased socket_type  := 0; -- /= 0 => socket() initialized
      connected : Boolean :=  False;
      binded    : Boolean :=  False;
      listened  : Boolean :=  False;
  end record
  with Convention => C;


  type socket_buffer
  is new Root_Stream_Type with
    record
      data  : aliased stream_element_array_access := null;
      head_first, tail_end  : Stream_Element_Count := 0;
    end record
    with Preelaborable_initialization;

  null_addresses  : constant addresses := (others => <>);

end adare_net.sockets;
