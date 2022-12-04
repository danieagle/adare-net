
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


  type ports  is new  Unsigned_16;


  type Address_family is new Unsigned_16;

  any : constant Address_family with Import => True, Convention => C,
                                     external_name => "c_af_unspec";
  v4  : constant Address_family with Import => True, Convention => C,
                                     external_name => "c_af_inet";
  v6  : constant Address_family with Import => True, Convention => C,
                                     external_name => "c_af_inet6";


  type Address_type is new int;

  tcp : constant Address_type with Import => True, Convention => C,
                                   external_name => "c_sock_stream";
  udp : constant Address_type with Import => True, Convention => C,
                                   external_name => "c_sock_dgram";


  v6_str_length : constant int with Import => True, Convention => C,
                                    External_Name => "c_v6_str_length";
  v4_str_length : constant int with Import => True, Convention => C,
                                    External_Name => "c_v4_addrstrlen";


  type addresses  is  private
    with Preelaborable_initialization;

  type addresses_list is array (Positive range <>) of aliased addresses
     with Preelaborable_initialization;

  null_addresses  : constant addresses;

  function init_addresses
    (ip_or_host : String;
     port       : String;
     ai_socktype  : Address_type;
     ai_family    : Address_family
    ) return addresses_list
    with pre => (port /= "" or else ip_or_host /= "")
      and then (ai_socktype = tcp or else ai_socktype = udp)
      and then (ai_family = any or else ai_family = v4 or else ai_family = v6);

  function init_addresses
    (ip_or_host : String;
     port       : String;
     ai_socktype  : Address_type;
     ai_family    : Address_family
    ) return addresses
    with pre => (port /= "" or else ip_or_host /= "")
      and then (ai_socktype = tcp or else ai_socktype = udp)
      and then (ai_family = any or else ai_family = v4 or else ai_family = v6);

  function get_addresses
    (show  : not null access addresses) return String;

  function get_addresses
    (show  : in addresses) return String;

  function get_port
    (show  : not null access addresses) return String;

  function get_port
    (show  : in addresses) return String;

  function get_address_and_port
    (show  : not null access addresses) return String;

  function get_address_and_port
    (show  : in addresses) return String;

  function get_address_family
    (show  : not null access addresses) return Address_family;

  function get_address_family
    (show  : in addresses) return Address_family;

  function is_null
    (addr  : not null access addresses) return Boolean;

  function is_null
    (addr  : in addresses) return Boolean;


  type socket_buffer (buffer_length : Stream_Element_Count)
  is new Root_Stream_Type with private;

  overriding
  procedure Read
    (Stream : in out socket_buffer;
     Item   : out Stream_Element_Array;
     Last   : out Stream_Element_Offset);

  overriding
  procedure Write
    (Stream : in out socket_buffer;
     Item   : in Stream_Element_Array);


  type socket is private
    with Preelaborable_initialization;

  function init_socket
    (sock  : in out socket;
     addr  : not null access addresses) return Boolean
     with  pre => not initialized (sock);

  function init_socket
    (sock   : in out socket;
     addr   : not null access addresses_list) return Boolean
     with  pre => not initialized (sock);


  procedure reuse_address
    (sock  : in out socket)
     with  pre => (initialized (sock))
      and then (not binded (sock));

  function bind
    (sock  : in out socket) return Boolean
    with  pre => (initialized (sock)) and then  (not binded (sock)) and then
                 (not connected (sock)) and then (not listened (sock));

  function listen
    (sock     : in out socket;
     backlog  : int) return Boolean
    with  pre => (binded (sock)) and then (not listened (sock))
                and then backlog >= 0;

  function accept_socket
    (sock     : not null access socket;
     new_sock : in out socket) return Boolean
  with  pre => listened (sock);

  function connect
    (sock  : in out socket) return Boolean
    with  pre => (initialized (sock)) and then  (not binded (sock)) and then
                 (not connected (sock)) and then (not listened (sock));

  procedure close
    (sock  : in out socket)
     with  pre => initialized (sock);

  procedure clean
    (sock  : in out socket);

  function send
    (sock     : socket;
     buffer   : in Stream_Element_Array
    ) return ssize_t;

  function send
    (sock     : socket;
     buffer   : in out socket_buffer
    ) return ssize_t;

  function sendto
    (sock     : socket;
     send_to  : addresses;
     buffer   : in Stream_Element_Array) return ssize_t;

  function sendto
    (sock     : socket;
     send_to  : addresses;
     buffer   : in out socket_buffer) return ssize_t;

  function receive
    (sock     : socket;
     buffer   : in out Stream_Element_Array
    ) return ssize_t;

  function receive
    (sock     : socket;
     buffer   : in out socket_buffer
    ) return ssize_t
    with pre => not is_full (buffer);

  function receive_from
    (sock     : socket;
     buffer   : in out Stream_Element_Array;
     from     : out addresses) return ssize_t;

  function receive_from
    (sock     : socket;
     buffer   : in out socket_buffer;
     from     : out addresses) return ssize_t
     with pre => not is_full (buffer);

  function get_sock
    (sock : in socket) return socket_type
     with pre => initialized (sock);

  function get_addresses
    (sock : in socket) return addresses
     with pre => initialized (sock);


  function initialized
    (sock  :  socket) return Boolean;

  function initialized
    (sock  : not null access socket) return Boolean;


  function connected
    (sock  : not null access socket) return Boolean;

  function connected
    (sock  : in socket) return Boolean;


  function binded
    (sock  : not null access socket) return Boolean;

  function binded
    (sock  : in socket) return Boolean;


  function listened
    (sock  :  not null access socket) return Boolean;

  function listened
    (sock  :  in socket) return Boolean;


  function is_empty
    (buffer : not null access socket_buffer) return Boolean;

  function is_empty
    (buffer : in socket_buffer) return Boolean;


  function is_full
    (buffer : not null access socket_buffer) return Boolean;

  function is_full
    (buffer : in socket_buffer) return Boolean;


  function actual_data_size
    (buffer : not null access socket_buffer)
      return Integer_64
  with Inline;

  function actual_data_size
    (buffer : in socket_buffer)
      return Integer_64;


  function max_data_length
    (buffer : not null access socket_buffer)
      return Integer_64;

  function max_data_length
    (buffer : in socket_buffer)
      return Integer_64;


  function max_data_length
    (buffer : not null access socket_buffer)
      return Stream_Element_Offset;

  function max_data_length
    (buffer : in socket_buffer)
      return Stream_Element_Offset;


  function get_buffer_init
    (buffer : not null access socket_buffer)
      return socket_buffer;


  procedure clean
    (buffer : in out socket_buffer);

  procedure clean
    (buffer : not null access socket_buffer);


  procedure flush_buffer
    (buffer : in out socket_buffer);


  function add_raw
    (buffer : in out socket_buffer;
     raw    : in Stream_Element_Array) return Boolean
     with pre => raw'Length > 0;

  function add_raw
    (buffer : not null access socket_buffer;
     raw    : in Stream_Element_Array) return Boolean
     with pre => raw'Length > 0;


  function get_raw
    (buffer : in socket_buffer) return Stream_Element_Array
    with pre => not is_empty (buffer);

  function get_raw
    (buffer : not null access socket_buffer) return Stream_Element_Array
    with pre => not is_empty (buffer);


  procedure reset_errno;

  function string_error return String;


private

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

  type socket_buffer (buffer_length : Stream_Element_Count)
  is new Root_Stream_Type with
    record
      data  : Stream_Element_Array (1 .. buffer_length);
      head, tail  : Stream_Element_Count := 0;
    end record
    with Preelaborable_initialization;

  null_addresses  : constant addresses := (others => <>);

end adare_net.sockets;
