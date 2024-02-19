separate (adare_net.base)
function create_addresses
  (host_or_ip : String;
    network_port_or_service  : String;
    Addr_family  : Address_family_label;
    Addr_type    : Address_type_label;
    response     : out socket_addresses) return Boolean
is
  ai_passive  : constant Interfaces.C.int
    with Import => True, Convention => C, External_Name => "c_ai_passive";

  acc : Address := Null_Address
    with Unreferenced;

  hint  : aliased constant addr_info
    :=  (ai_flags => (if host_or_ip'Length /= 0 then 0 else ai_passive),
          ai_family => Interfaces.C.int (family (Addr_family)),
          ai_socktype => Interfaces.C.int (a_type (Addr_type)),
          ai_protocol => 0,
          ai_addrlen => 0,
          ai_addr => Null_Address,
          ai_canonname => Null_Address,
          ai_next => Null_Address);

  tmp_getted_response : Address  := Null_Address;
  mi_system_address   : Address  := Null_Address;
  mi_socket_addresses : socket_addresses;

  tmp_addr  : aliased socket_address;

  h_or_i    : aliased constant char_array  := To_C (host_or_ip);
  n_p_or_s  : aliased constant char_array  := To_C (network_port_or_service);

  ret_value : constant Interfaces.C.int :=  inner_getaddrinfo
    (host_or_ip_i =>  (if host_or_ip'Length /= 0 then h_or_i'Address else Null_Address),
      port_i       =>  (if network_port_or_service'Length /= 0 then n_p_or_s'Address else Null_Address),
      hints_i      =>  hint'Address,
      response_i   =>  tmp_getted_response'Address);

  --  tmp_addrinfo : aliased addr_info  := To_Pointer (tmp_getted_response).all;

begin
  response := mi_socket_addresses;

  if ret_value /= 0 or else tmp_getted_response = Null_Address then
    return False;
  end if;

  mi_system_address := tmp_getted_response;

  loop1 :
  loop
    tmp_addr  :=  null_socket_address;

    acc := inner_memset (tmp_addr.storage'Address, 0, tmp_addr.storage'Size / 8);

    b0 :
    declare
      mi_addrinfo : aliased constant addr_info := To_Pointer (mi_system_address).all;
      --  tmp_sockaddr_storage_ptr  : ainfo2.Object_Pointer := To_Pointer (mi_addrinfo.ai_addr);
    begin

      if mi_addrinfo.ai_addrlen > 0 then
        acc := inner_memcpy (tmp_addr.storage'Address, mi_addrinfo.ai_addr,
          size_t (mi_addrinfo.ai_addrlen));
      end if;

      tmp_addr.socktype :=  Address_type (mi_addrinfo.ai_socktype);

      tmp_addr.protocol :=  Address_family (mi_addrinfo.ai_protocol);

      tmp_addr.addr_length  :=  socklen_t (mi_addrinfo.ai_addrlen);

      Prepend (mi_socket_addresses.mi_list, tmp_addr, 1);

      exit loop1 when mi_addrinfo.ai_next = Null_Address;

      mi_system_address := mi_addrinfo.ai_next;

    end b0;

  end loop loop1;

  inner_free_addrinfo (tmp_getted_response);

  response  :=  mi_socket_addresses;

  return True;
end create_addresses;

