
package adare_net.base.inners
  with Preelaborate
is

  function inner_accept
    (sockfd_i     : socket_type;
    addr_i        : Address;
    addr_length_i : Address) return socket_type
    with Import => True, Convention => StdCall, External_Name => "accept";

  function inner_bind
    (sockfd_i           : in socket_type;
    addr_i              : Address;
    address_length_i    : in int) return int
    with Import => True, Convention => StdCall, External_Name => "bind";

  function inner_close
    (sock_i : socket_type) return int
    with Import => True, Convention => StdCall, External_Name => "closesocket";

  function inner_connect
    (sockfd_i : socket_type;
    addr_i    : Address;
    leng_i    : int) return int
    with Import => True, Convention => StdCall, External_Name => "connect";

  function inner_getaddrinfo
    (host_or_ip_i : Address;
     port_i   : Address;
     hints_i  : Address;
     response_i : Address) return int
    with Import => True, Convention => StdCall, External_Name => "getaddrinfo";

  procedure inner_free_addrinfo
    (res_i  : Address)
    with Import => True, Convention => StdCall, External_Name => "freeaddrinfo";

  function inner_socket
    (domain_i   : in int;
    type_i      : in int;
    protocol_i  : in int) return socket_type
    with Import => True, Convention => StdCall, External_Name => "socket";

  function inner_listen
    (sockfd_i   : socket_type;
    backlog_i   : int) return int
    with Import => True, Convention => StdCall, External_Name => "listen";

  function inner_recvfrom
    (sock_i : socket_type;
     buf_i  : Address;
     len_i  : int;
     flags_i  : int;
     from_i   : Address;
     from_len_i : Address -- *int
    ) return ssize_t
    with Import => True, Convention => StdCall, External_Name => "recvfrom";

  function inner_recv
    (sock_i : socket_type;
     buf_i  : Address;
     len_i  : int;
     flags_i  : int
     ) return ssize_t
     with Import => True, Convention => StdCall, External_Name => "recv";

  procedure inner_reset_errno
    with Import => True, Convention => StdCall, External_Name => "c_reset_errno";

  procedure inner_reuse_address
    (sock_i : socket_type)
    with Import => True, Convention => StdCall, External_Name => "c_reuse_address";

  function inner_send
    (sock_i : socket_type;
     buf_i  : Address;
     len_i  : int;
     flags_i  : int
    ) return int
    with Import => True, Convention => StdCall, External_Name => "send";

  function inner_sendto
    (sock_i : socket_type;
     buf_i  : Address;
     len_i  : int;
     flags_i  : int;
     to_i     : Address;
     to_len_i : int
     ) return int
     with Import => True, Convention => StdCall, External_Name => "sendto";

  procedure inner_inet_ntop
    (af : int;
     src  : Address;
     dst  : Address;
     size : size_t)
     with Import => True, Convention => StdCall, External_Name => "inet_ntop";

  function inner_ntohs
    (netshort : Unsigned_16) return Unsigned_16
    with Import => True, Convention => StdCall, External_Name => "ntohs";

  procedure inner_show_error
    (message_i : in out char_array;
    length_i   : in out int)
    with Import => True, Convention => StdCall, External_Name => "c_show_error";


  function inner_epoll_create1 (flags_i : int := 0) return handle_type
    with Import => True, Convention => StdCall, External_Name => "epoll_create1";

  function inner_epoll_close (ephnd_i : handle_type) return int
    with Import => True, Convention => StdCall, External_Name => "epoll_close";

  function inner_epoll_ctl
    (ephnd_i  : handle_type;
     op_i     : int;
     sock_i   : socket_type;
     event_i  : Address
    ) return int
    with Import => True, Convention => StdCall, External_Name => "epoll_ctl";

  function inner_epoll_wait
    (ephnd_i  : handle_type;
     events_i : Address;
     maxevents_i  : int;
     timeout_i    : int
    ) return int
    with Import => True, Convention => StdCall, External_Name => "epoll_wait";

  function inner_memset
    (dest_i : Address;
     c_i    : int;
     count_i  : size_t
    ) return Address
    with Import => True, Convention => StdCall, External_Name => "memset";

  function inner_memcpy
    (dest_i   : Address;
     src_i    : Address;
     count_i  : size_t
    ) return Address
    with Import => True, Convention => StdCall, External_Name => "memcpy";

end adare_net.base.inners;
