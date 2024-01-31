
with adare_net_exceptions;  use adare_net_exceptions;

with System;  use System;

with adare_net.base.inners; use adare_net.base.inners;


package body adare_net.base
  with Preelaborate
is

  function actual_data_size
    (buffer : not null socket_buffer_access) return Integer_64
  is (if buffer.data /= null then (Integer_64 ((buffer.tail_end + 1) - buffer.head_first)) else 0);

  function actual_data_size
    (buffer : aliased socket_buffer) return Integer_64
  is (if buffer.data /= null then (Integer_64 ((buffer.tail_end + 1) - buffer.head_first)) else 0);


  function max_data_length
    (buffer : aliased socket_buffer) return Stream_Element_Offset
  is (if buffer.data /= null then (buffer.head_first - buffer.data'First) + (buffer.data'Last - buffer.tail_end) else 0);

  function data_tail_length
    (buffer : aliased socket_buffer) return Stream_Element_Offset
  is (if buffer.data /= null then buffer.data'Last - buffer.tail_end else 0);


  function create_address
    (host_or_ip : String;
     network_port_or_service  : String;
     Addr_family  : Address_family;
     Addr_type : Address_type) return socket_address
  is
    ai_passive  : constant Interfaces.C.int
      with Import => True, Convention => C, External_Name => "c_ai_passive";

    hint  : aliased constant addr_info
      :=  (ai_flags => (if host_or_ip'Length /= 0 then 0 else ai_passive),
           ai_family => Interfaces.C.int (Addr_family),
           ai_socktype => Interfaces.C.int (Addr_type),
           ai_protocol => 0,
           ai_addrlen => 0,
           ai_addr => null,
           ai_canonname => null,
           ai_next => null);

    mi_response     : aliased addr_info_access;

    h_or_i    : aliased constant char_array  := To_C (host_or_ip);
    n_p_or_s  : aliased constant char_array  := To_C (network_port_or_service);

    ret_value : constant Interfaces.C.int :=  inner_getaddrinfo
      (host_or_ip_i =>  (if host_or_ip'Length /= 0 then h_or_i'Address else Null_Address),
       port_i       =>  (if network_port_or_service'Length /= 0 then n_p_or_s'Address else Null_Address),
       hints_i      =>  hint'Address,
       response_i   =>  mi_response'Address);

    mi_result     : socket_address := socket_address'(others => <>);

  begin

    if ret_value /= 0 or else mi_response = null then
      return null_socket_address;
    end if;

    mi_result.storage.ss.ss_family := mi_response.ai_addr.ss_family;

    mi_result.storage.ss.padding (1 .. Interfaces.C.size_t (mi_response.ai_addrlen) - 2)
      := mi_response.ai_addr.padding (1 .. Interfaces.C.size_t (mi_response.ai_addrlen) - 2);

    mi_result.socktype     :=  mi_response.ai_socktype;

    mi_result.protocol     :=  Address_family (mi_response.ai_protocol);

    mi_result.addr_length  :=  mi_response.ai_addrlen;

    inner_free_addrinfo (mi_response'Address);

    return mi_result;
  end create_address;

  function create_address
    (host_or_ip : String;
     network_port_or_service  : String;
     Addr_family  : Address_family;
     Addr_type : Address_type) return socket_addresses
  is
    ai_passive  : constant Interfaces.C.int
      with Import => True, Convention => C, External_Name => "c_ai_passive";

    hint  : aliased constant addr_info
      :=  (ai_flags => (if host_or_ip'Length /= 0 then 0 else ai_passive),
           ai_family => Interfaces.C.int (Addr_family),
           ai_socktype => Interfaces.C.int (Addr_type),
           ai_protocol => 0,
           ai_addrlen => 0,
           ai_addr => null,
           ai_canonname => null,
           ai_next => null);

    mi_response     : aliased addr_info_access;
    mi_tmp_response : aliased addr_info_access;

    h_or_i    : aliased constant char_array  := To_C (host_or_ip);
    n_p_or_s  : aliased constant char_array  := To_C (network_port_or_service);

    ret_value : constant Interfaces.C.int :=  inner_getaddrinfo
      (host_or_ip_i =>  (if host_or_ip'Length /= 0 then h_or_i'Address else Null_Address),
       port_i       =>  (if network_port_or_service'Length /= 0 then n_p_or_s'Address else Null_Address),
       hints_i      =>  hint'Address,
       response_i   =>  mi_response'Address);

    mi_result   : socket_addresses;
    mi_address  : socket_address := socket_address'(others => <>);

    capacity_max  : constant int := int (mi_result.mi_list.Capacity);
    mi_counter    : int := 1;

  begin
    if not Is_Empty (mi_result.mi_list) then
      Clear (mi_result.mi_list);
    end if;

    if ret_value /= 0 or else mi_response = null then
      return mi_result;
    end if;

    mi_tmp_response := mi_response;

    loop1 :
    loop

      mi_address.storage.ss.ss_family := mi_tmp_response.ai_addr.ss_family;

      mi_address.storage.ss.padding (1 .. Interfaces.C.size_t (mi_tmp_response.ai_addrlen) - 2)
        := mi_tmp_response.ai_addr.padding (1 .. Interfaces.C.size_t (mi_tmp_response.ai_addrlen) - 2);

      mi_address.socktype     :=  mi_tmp_response.ai_socktype;

      mi_address.protocol     :=  Address_family (mi_tmp_response.ai_protocol);

      mi_address.addr_length  :=  mi_tmp_response.ai_addrlen;

      Prepend (Container =>  mi_result.mi_list, New_Item => mi_address, Count => 1);

      mi_counter  :=  mi_counter + 1;

      mi_tmp_response := mi_tmp_response.ai_next;

      exit loop1 when mi_tmp_response = null or else mi_counter > capacity_max;

      mi_address.storage.sp := (others => char'Val (0));

    end loop loop1;

    inner_free_addrinfo (mi_response'Address);

    return mi_result;
  end create_address;

  function create_socket
    (sock_address : socket_address;
     bind_socket  : Boolean := False) return socket
  is
    mi_response   : socket  := null_socket;
    mi_socket_fd  : socket_type;
    acc           : Interfaces.C.int := 0
      with Unreferenced;
  begin

    mi_response.storage  := sock_address;

    mi_socket_fd :=
      inner_socket (int (mi_response.storage.storage.ss.ss_family),
        Interfaces.C.int (mi_response.storage.socktype),
        Interfaces.C.int (mi_response.storage.protocol));

    if mi_socket_fd /= invalid_socket then
      mi_response.sock := mi_socket_fd;

      if bind_socket then
        if inner_bind (mi_response.sock, mi_response.storage.storage.ss'Address,
          Interfaces.C.int (mi_response.storage.addr_length)) /= 0
        then
          mi_response.binded := True;

          return mi_response;
        end if;

        acc := inner_close (mi_response.sock);

        return null_socket;
      end if;

      return mi_response;
    end if;

    return null_socket;
  end create_socket;

  function create_socket
    (sock_address : socket_addresses;
     bind_socket  : Boolean := False) return socket
  is
    mi_response   : socket  := null_socket;
    mi_socket_fd  : socket_type;
    acc           : Interfaces.C.int := 0
      with Unreferenced;
  begin

    loop1 :
    for addr of sock_address.mi_list loop

      mi_response.storage  := addr;

      mi_socket_fd :=
        inner_socket (int (mi_response.storage.storage.ss.ss_family),
          Interfaces.C.int (mi_response.storage.socktype),
          Interfaces.C.int (mi_response.storage.protocol));

      if mi_socket_fd /= invalid_socket then
        mi_response.sock := mi_socket_fd;

        if bind_socket then
          if inner_bind (mi_response.sock, mi_response.storage.storage.ss'Address,
            Interfaces.C.int (mi_response.storage.addr_length)) /= 0
          then
            mi_response.binded := True;

            return mi_response;
          end if;

          acc := inner_close (mi_response.sock);

          goto end_loop1_label;
        end if;

        return mi_response;
      end if;

      <<end_loop1_label>> -- a missing continue :)

      mi_response :=  null_socket;
    end loop loop1;

    return null_socket;
  end create_socket;



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

end adare_net.base;
