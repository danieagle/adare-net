with Interfaces.C;

with Ada.Streams;

with socket_types;

private with Ada.Containers.Bounded_Doubly_Linked_Lists;

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

  type Address_family_label is (any, ipv4, ipv6);

  tmp_any   : constant Address_family with Import => True, Convention => C, external_name => "c_af_unspec";
  tmp_ipv4  : constant Address_family with Import => True, Convention => C, external_name => "c_af_inet";
  tmp_ipv6  : constant Address_family with Import => True, Convention => C, external_name => "c_af_inet6";

  function family (family_label : Address_family_label := any) return Address_family
    with Inline;

  function family_label (a_family : Address_family := tmp_any) return Address_family_label
    with Inline;


  type Address_type is new int;

  type Address_type_label is (tcp, udp);

  tmp_tcp : constant Address_type with Import => True, Convention => C, external_name => "c_sock_stream";
  tmp_udp : constant Address_type with Import => True, Convention => C, external_name => "c_sock_dgram";

  function a_type (type_label : Address_type_label := tcp) return Address_type
    with Inline;

  function a_type_label (a_type : Address_type  := tmp_tcp) return Address_type_label
    with Inline;


  ipv4_length : constant socklen_t;
  ipv6_length : constant socklen_t;


  type socket_address is  private;

  type socket_address_access is access all socket_address;

  null_socket_address : constant socket_address;

  type socket_addresses is  private;


  type stream_element_array_access is access all Stream_Element_Array;


  type socket is private;

  null_socket : constant socket;


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


  function create_address
    (host_or_ip : String;
     network_port_or_service  : String;
     Addr_family  : Address_family_label;
     Addr_type    : Address_type_label) return socket_address;

  function create_address
    (host_or_ip : String;
     network_port_or_service  : String;
     Addr_family  : Address_family_label;
     Addr_type : Address_type_label) return socket_addresses;

  function create_socket
    (sock_address : socket_address;
     bind_socket  : Boolean := False) return socket;

  function create_socket
    (sock_address : socket_addresses;
     bind_socket  : Boolean := False) return socket;

  function wait_connection
    (sock     : aliased in out socket;
     data_received  : aliased out stream_element_array_access;
     backlog  : Unsigned_16 := 10
     -- backlog is ignored after first use in sock. close and recreate socket
     --   to configure backlog again.
    ) return socket
      with Pre => is_initialized (sock) and then backlog > 0;

  function wait_connection_with_timeout
    (sock : aliased in out socket;
     miliseconds_timeout : Unsigned_32;
     data_received  : aliased out stream_element_array_access;
     backlog  : Unsigned_16 :=  10
     -- backlog is ignored after first use in sock. close and recreate socket
     --   to configure backlog again.
    ) return socket
      with Pre => is_initialized (sock) and then
        miliseconds_timeout > 0 and then backlog > 0;

  function send_buffer
    (sock : aliased socket;
     data_to_send : aliased in out socket_buffer
    ) return Interfaces.C.int
    with Pre => is_initialized (sock);

  function send_stream
    (sock : aliased socket;
     data_to_send : aliased Stream_Element_Array
    ) return Interfaces.C.int
    with Pre => is_initialized (sock);

  function send_buffer_with_timeout
    (sock : aliased socket;
     data_to_send : aliased in out socket_buffer;
     miliseconds_timeout : Unsigned_32
    ) return Interfaces.C.int
    with Pre => is_initialized (sock) and then miliseconds_timeout > 0;

  function send_stream_with_timeout
    (sock : aliased socket;
     data_to_send : aliased Stream_Element_Array;
     miliseconds_timeout : Unsigned_32
    ) return Interfaces.C.int
    with Pre => is_initialized (sock) and then miliseconds_timeout > 0;


  function receive_buffer
    (sock : aliased socket;
     data_to_receive  : aliased in out socket_buffer;
     received_address : aliased out socket_address
    ) return Interfaces.C.int
    with Pre => is_initialized (sock);

  function receive_stream
    (sock : aliased socket;
     data_to_receive : aliased out stream_element_array_access;
     received_address : aliased out socket_address
    ) return Interfaces.C.int
    with Pre => is_initialized (sock);

  function receive_buffer_with_timeout
    (sock : aliased socket;
     data_to_receive : aliased in out socket_buffer;
     received_address : aliased out socket_address;
     miliseconds_timeout : Unsigned_32
    ) return Interfaces.C.int
    with Pre => is_initialized (sock) and then miliseconds_timeout > 0;

  function receive_stream_with_timeout
    (sock : aliased socket;
     data_to_receive : aliased out stream_element_array_access;
     received_address : aliased out socket_address;
     miliseconds_timeout : Unsigned_32
    ) return Interfaces.C.int
    with Pre => is_initialized (sock) and then miliseconds_timeout > 0;


  procedure close (sock : in out socket);

  function is_initialized
    (sock : socket) return Boolean;

private

  function max_data_length
    (buffer : aliased socket_buffer) return Stream_Element_Offset;

  function data_tail_length
    (buffer : aliased socket_buffer) return Stream_Element_Offset;

  function family (family_label : Address_family_label := any) return Address_family
    is (case family_label is when any => tmp_any, when ipv4 => tmp_ipv4, when ipv6 => tmp_ipv6);

  function a_type (type_label : Address_type_label := tcp) return Address_type
    is (case type_label is when tcp => tmp_tcp, when udp => tmp_udp);

  ipv4_length : constant socklen_t with Import => True, Convention => C, External_Name => "c_v4_addrstrlen";
  ipv6_length : constant socklen_t with Import => True, Convention => C, External_Name => "c_v6_str_length";

  msg_peek_flag : constant int with Import => True, Convention => C, external_name => "c_msg_peek";

  type in_addr is
    record
      s_addr  : Unsigned_32 := 0;
    end record
      with Convention => C, Preelaborable_initialization;

  type in6_addr is
    record
      s6_addr : char_array (1 .. 16) := (others => char'Val (0));
    end record
      with Convention => C, Preelaborable_initialization;

  type sockaddr_in is
    record
      sin_family  : Unsigned_16 := 0;
      sin_port    : Unsigned_16 := 0;
      sin_addr    : in_addr;
      sin_zero    : char_array (1 .. 8) := (others => char'Val (0));
    end record
      with Convention => C, Preelaborable_initialization;

  type sockaddr_in6 is
    record
      sin6_family : Unsigned_16 := 0;
      sin6_port   : Unsigned_16 := 0;
      sin6_flowinfo : Unsigned_32 := 0;
      sin6_addr     : in6_addr;
      sin6_scope_id : Unsigned_32 := 0;
    end record
      with Convention => C, Preelaborable_initialization;

  type sockaddr_storage is
    record
      ss_family : aliased Unsigned_16 := 0;
      padding   : aliased char_array (1 .. 132) := (others => char'Val (0));
    end record
      with Convention => C, Preelaborable_initialization;

  type sockaddr_storage_access is access all sockaddr_storage;


  type storage_union_tmp is (mi_plain_storage, mi_storage, mi_ipv4, mi_ipv6);

  type storage_union (mi_discr : storage_union_tmp := mi_storage) is
    record
      case mi_discr is
        when mi_plain_storage =>
          sp  : aliased char_array (1 .. 134) := (others => char'Val (0));

        when mi_storage =>
          ss  : aliased sockaddr_storage;

        when mi_ipv6 =>
          i6  : aliased sockaddr_in6;

        when mi_ipv4 =>
          i4  : aliased sockaddr_in;
      end case;
    end record
      with Convention => C, Unchecked_Union, Preelaborable_initialization;

  type socket_address  is
    record
      storage   : aliased storage_union;
      socktype  : aliased Address_type    := 0; -- a_type (tcp);
      protocol  : aliased Address_family  := 0; -- family (any);
      addr_length : aliased socklen_t     := 0;
    end record
      with Convention => C, Preelaborable_initialization;


  package scbdll is new ada.Containers.Bounded_Doubly_Linked_Lists (socket_address);
  use scbdll;

  type socket_addresses  is
    record
      mi_list : List (300) := Empty_List;
    end record
      with Convention => C, Preelaborable_initialization;


  type socket is
    record
      storage   : aliased socket_address;
      sock      : aliased socket_type  := 0; -- /= 0 => socket() initialized
      connected : Boolean :=  False;
      binded    : Boolean :=  False;
      listened  : Boolean :=  False;
  end record
    with Convention => C, Preelaborable_initialization;

  null_socket : constant socket := (others => <>);


  type socket_buffer is new Root_Stream_Type with
    record
      data  : aliased stream_element_array_access := null;
      head_first, tail_end  : Stream_Element_Count := 0;
    end record
      with Preelaborable_initialization;

  null_socket_address   : constant socket_address := (others => <>);


  type char_array_access is access all char_array;

  type addr_info;
  type addr_info_access is access all addr_info; -- ToDo: Convention => C ?

  type addr_info is
    record
      ai_flags  : Interfaces.C.int := 0;
      ai_family : Interfaces.C.int := 0;
      ai_socktype : Interfaces.C.int := 0;
      ai_protocol : Interfaces.C.int := 0;
      ai_addrlen  : socklen_t := 0;
      ai_addr     : sockaddr_storage_access := null;
      ai_canonname  : char_array_access := null;
      ai_next : addr_info_access  :=  null;
    end record
      with Convention => C, Preelaborable_initialization;

  function storage_size return socklen_t
    with Inline;

end adare_net.base;
