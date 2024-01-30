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

  any   : constant Unsigned_16 with Import => True, Convention => C, external_name => "c_af_unspec";
  ipv4  : constant Unsigned_16 with Import => True, Convention => C, external_name => "c_af_inet";
  ipv6  : constant Unsigned_16 with Import => True, Convention => C, external_name => "c_af_inet6";

  subtype Address_family is Unsigned_16
    with Dynamic_Predicate => Address_family in any | ipv4 | ipv6;

  tcp : constant int with Import => True, Convention => C, external_name => "c_sock_stream";
  udp : constant int with Import => True, Convention => C, external_name => "c_sock_dgram";

  subtype Address_type is int
    with Dynamic_Predicate => Address_type in tcp | udp;

  ipv4_length : constant socklen_t;
  ipv6_length : constant socklen_t;

  type socket_address  is  private;
  null_socket_address  : constant socket_address;

  type socket_addresses  is  private;

  type stream_element_array_access is access all Stream_Element_Array;

  type socket is private;

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


private

  function max_data_length
    (buffer : aliased socket_buffer) return Stream_Element_Offset;

  function data_tail_length
    (buffer : aliased socket_buffer) return Stream_Element_Offset;

  ipv4_length : constant socklen_t with Import => True, Convention => C, External_Name => "c_v4_addrstrlen";
  ipv6_length : constant socklen_t with Import => True, Convention => C, External_Name => "c_v6_str_length";

  type in_addr is
    record
      s_addr  : Unsigned_32;
    end record
      with Convention => C, Preelaborable_initialization;

  type in6_addr is
    record
      s6_addr : char_array (1 .. 16);
    end record
      with Convention => C, Preelaborable_initialization;

  type sockaddr_in is
    record
      sin_family  : Unsigned_16;
      sin_port    : Unsigned_16;
      sin_addr    : in_addr;
      sin_zero    : char_array (1 .. 8);
    end record
      with Convention => C, Preelaborable_initialization;

  type sockaddr_in6 is
    record
      sin6_family : Unsigned_16;
      sin6_port   : Unsigned_16;
      sin6_flowinfo : Unsigned_32;
      sin6_addr     : in6_addr;
      sin6_scope_id : Unsigned_32;
    end record
      with Convention => C, Preelaborable_initialization;

  type sockaddr_storage is
    record
      ss_family : aliased Unsigned_16 := 0;
      padding   : aliased char_array (1 .. 132) := (others => char'Val (0));
    end record
      with Convention => C, Preelaborable_initialization;

  type sockaddr_storage_access is access all sockaddr_storage;


  type storage_union_tmp is (mi_storage, mi_ipv4, mi_ipv6);

  type storage_union (mi_discr : storage_union_tmp := mi_storage) is
    record
      case mi_discr is
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
      socktype  : aliased Address_type    := Address_type (any);
      protocol  : aliased Address_family  := Address_family (tcp);
      addr_length : aliased socklen_t     := 0;
    end record
      with Convention => C, Preelaborable_initialization;


  package scbdll is new ada.Containers.Bounded_Doubly_Linked_Lists (socket_address);
  use scbdll;

  type socket_addresses  is
    record
      mi_list : List (300);
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


  type socket_buffer is new Root_Stream_Type with
    record
      data  : aliased stream_element_array_access := null;
      head_first, tail_end  : Stream_Element_Count := 0;
    end record
      with Preelaborable_initialization;

  null_socket_address   : constant socket_address := (others => <>);


  type char_array_access is access all char_array;

  type addr_info;
  type addr_info_access is access all addr_info;

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

end adare_net.base;
