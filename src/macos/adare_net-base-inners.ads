
package adare_net.base.inners
  with Preelaborate
is

  function inner_accept
    (sockfd_i     : socket_type;
    addr_i        : Address;
    addr_length_i : Address) return socket_type
    with Import => True, Convention => C, External_Name => "accept";

  function inner_bind
    (sockfd_i           : socket_type;
    addr_i              : Address;
    address_length_i    : in int) return int
    with Import => True, Convention => C, External_Name => "bind";

  function inner_close
    (sock_i : socket_type) return int
    with Import => True, Convention => C, External_Name => "close";

  function inner_connect
    (sockfd_i : socket_type;
    addr_i    : Address;
    leng_i    : int) return int
    with Import => True, Convention => C, External_Name => "connect";

  function inner_socket
    (domain_i   : in int;
    type_i      : in int;
    protocol_i  : in int) return socket_type
    with Import => True, Convention => C, External_Name => "socket";

  function inner_listen
    (sockfd_i   : socket_type;
    backlog_i   : int) return int
    with Import => True, Convention => C, External_Name => "listen";

  function inner_recvfrom
    (sock_i : socket_type;
     buf_i  : Address;
     len_i  : size_t;
     flags_i  : int;
     from_i   : Address;
     from_len_i : Address
    ) return int
    with Import => True, Convention => C, External_Name => "recvfrom";

  function inner_recv
    (sock_i : socket_type;
     buf_i  : Address;
     len_i  : size_t;
     flags_i  : int
     ) return int
     with Import => True, Convention => C, External_Name => "recv";

  procedure inner_reset_errno
    with Import => True, Convention => C, External_Name => "c_reset_errno";

  procedure inner_reuse_address
    (sock_i : socket_type)
    with Import => True, Convention => C, External_Name => "c_reuse_address";

  function inner_send
    (sock_i : socket_type;
     buf_i  : Address;
     len_i  : size_t;
     flags_i  : int
    ) return int
    with Import => True, Convention => C, External_Name => "send";

  function inner_sendto
    (sock_i : socket_type;
     buf_i  : Address;
     len_i  : size_t;
     flags_i  : int;
     to_i     : Address;
     to_len_i : int
     ) return int
     with Import => True, Convention => C, External_Name => "sendto";

  function inner_inet_ntop
    (af : int;
     src  : Address;
     dst  : Address;
     size : size_t) return Address
     with Import => True, Convention => C, External_Name => "inet_ntop";

  function inner_ntohs
    (netshort : Unsigned_16) return Unsigned_16
    with Import => True, Convention => C, External_Name => "ntohs";

  procedure inner_show_error
    (message_i : in out char_array;
    length_i   : in out int)
    with Import => True, Convention => C, External_Name => "c_show_error";

  function inner_mi_and
    (left_i, right_i  : short) return short
    with Import => True, Convention => C, External_Name => "mi_and";

  function inner_poll
    (fds_i  : Address;
     nfds_i : Unsigned_16;
     timeout_i  : int) return int
     with Import => True, Convention => C, External_Name => "poll";


  procedure inner_create_addresses
    (host_i    : Address;
     service_i : Address;
     data_i    : Address;
     data_length_i : Address;
     addr_family_i : Address_family_label;
     addr_type_i   : Address_type_label
    )
    with Import => True, Convention => C,  External_Name => "create_addresses";

end adare_net.base.inners;
