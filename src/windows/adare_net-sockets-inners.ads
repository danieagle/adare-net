
with System;

package adare_net.sockets.inners
  with Preelaborate
is
  use System;

  function inner_accept
    (sockfd_i     : socket_type;
    addr_i        : Address;
    addr_length_i : in out socklen_t) return socket_type
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
    leng_i    : size_t) return int
    with Import => True, Convention => StdCall,  External_Name => "connect";

  subtype a_list is addresses_list (1 .. 10);

  procedure inner_init_address (
    ip_or_host_i  : Address;
    port_i        : Address;
    ai_socktype_i : int;
    ai_family_i   : int;
    length_i      : in out int;
    list_i        : in out a_list
  ) with Import => True, Convention => StdCall, External_Name => "c_init_address";

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
     len_i  : size_t;
     flags_i  : int;
     from_i   : Address;
     from_len_i : in out socklen_t
    ) return int
    with Import => True, Convention => StdCall, External_Name => "recvfrom";

  function inner_recv
    (sock_i : socket_type;
     buf_i  : Address;
     len_i  : size_t;
     flags_i  : int
     ) return int
     with Import => True, Convention => StdCall, External_Name => "recv";

  procedure inner_reset_errno
    with Import => True, Convention => StdCall, External_Name => "c_reset_errno";

  procedure inner_reuse_address
    (sock_i : socket_type)
    with Import => True, Convention => StdCall, External_Name => "c_reuse_address";

  function inner_send
    (sock_i : socket_type;
     buf_i  : Address;
     len_i  : size_t;
     flags_i  : int
    ) return int
    with Import => True, Convention => StdCall, External_Name => "send";

  function inner_sendto
    (sock_i : socket_type;
     buf_i  : Address;
     len_i  : size_t;
     flags_i  : int;
     to_i     : Address;
     to_len_i : socklen_t
     ) return int
     with Import => True, Convention => StdCall, External_Name => "sendto";

  procedure inner_inet_ntop
    (af : int;
     src  : Address;
     dst  : Address;
     size : socklen_t)
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

end adare_net.sockets.inners;
