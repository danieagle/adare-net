
with System;

package adare_net.base.inners
  with Preelaborate
is
  use System;

  type who_enun is (ptr, fd, u32, u64, sock, hnd);

  type epoll_data_t (i_want_you : who_enun := ptr) is
    record
      case i_want_you is
        when ptr =>
          ptr  : Address  := Null_Address;

        when fd =>
          fd   : int  :=  -1;

        when u32 =>
          u32  : Unsigned_32  :=  0;

        when u64 =>
          u64  : Unsigned_64  :=  0;

        when sock =>
          sock : socket_type  :=  invalid_socket;

        when hnd =>
          hnd  : Address  :=  Null_Address;
      end case;
    end record
      with Convention => C, preelaborable_initialization, Unchecked_Union;

  type epoll_event is
    record
      events  : Unsigned_32 := 0;
      data    : epoll_data_t := (others => <>);
    end record
      with Convention => C, preelaborable_initialization;

    cmd_add : constant unsigned_long
      with Convention => C, import,
      External_Name   =>  "adare_epoll_cmd_add";

    cmd_del : constant unsigned_long
      with Convention => C, import,
      External_Name   =>  "adare_epoll_cmd_del";


  function inner_accept
    (sockfd_i     : socket_type;
    addr_i        : Address;
    addr_length_i : in out socklen_t) return socket_type
    with Import => True, Convention => C, External_Name => "accept";


  function inner_bind
    (sockfd_i           : in socket_type;
    addr_i              : Address;
    address_length_i    : in int) return int
    with Import => True, Convention => C, External_Name => "bind";

  function inner_close
    (sock_i : socket_type) return int
    with Import => True, Convention => C, External_Name => "close";

  function inner_connect
    (sockfd_i : socket_type;
    addr_i    : Address;
    leng_i    : size_t) return int
    with Import => True, Convention => C, External_Name => "connect";

  function inner_getaddrinfo
    (host_or_ip_i : Address;
     port_i   : Address;
     hints_i  : Address;
     response_i : Address) return int
    with Import => True, Convention => C, External_Name => "getaddrinfo";


  procedure inner_free_addrinfo
    (res_i  : Address)
    with Import => True, Convention => C, External_Name => "freeaddrinfo";


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
     from_len_i : in out socklen_t
    ) return ssize_t
    with Import => True, Convention => C, External_Name => "recvfrom";

  function inner_recv
    (sock_i : socket_type;
     buf_i  : Address;
     len_i  : size_t;
     flags_i  : int
     ) return ssize_t
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
    ) return ssize_t
    with Import => True, Convention => C, External_Name => "send";

  function inner_sendto
    (sock_i : socket_type;
     buf_i  : Address;
     len_i  : size_t;
     flags_i  : int;
     to_i     : Address;
     to_len_i : socklen_t
     ) return ssize_t
     with Import => True, Convention => C, External_Name => "sendto";

  procedure inner_inet_ntop
    (af : int;
     src  : Address;
     dst  : Address;
     size : socklen_t)
     with Import => True, Convention => C, External_Name => "inet_ntop";

  function inner_ntohs
    (netshort : Unsigned_16) return Unsigned_16
    with Import => True, Convention => C, External_Name => "ntohs";

  procedure inner_show_error
    (message_i : in out char_array;
    length_i   : in out int)
    with Import => True, Convention => C, External_Name => "c_show_error";

  function inner_epoll_create1 (flags_i : int := 0) return handle_type
    with Import => True, Convention => C, External_Name => "epoll_create1";

  function inner_epoll_close (ephnd_i : handle_type) return int
    with Import => True, Convention => C, External_Name => "close";

  function inner_epoll_ctl
    (ephnd_i  : handle_type;
     op_i     : int;
     sock_i   : socket_type;
     event_i  : Address
    ) return int
    with Import => True, Convention => C, External_Name => "epoll_ctl";

  function inner_epoll_wait
    (ephnd_i  : handle_type;
     events_i : Address;
     maxevents_i  : int;
     timeout_i    : int
    ) return int
    with Import => True, Convention => C, External_Name => "epoll_wait";

end adare_net.base.inners;
