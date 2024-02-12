
private with System;

package adare_net.base.waits
  with Preelaborate
is
  pragma  Assertion_Policy (Check);

  type epoll is limited private;

  type epoll_access is access all epoll;

  function is_initialized
    (poll : epoll_access
    ) return Boolean;

  function is_in
    (how  : not null epoll_access;
     what_sock  : not null socket_access
    ) return Boolean;


  receive_event : constant unsigned_long
    with Convention => C, import,
    External_Name   =>  "adare_epoll_epollin";

  accept_socket_event : constant unsigned_long
    with Convention => C, import,
    External_Name   =>  "adare_epoll_epollin";

  send_event : constant unsigned_long
    with Convention => C, import,
    External_Name   =>  "adare_epoll_epollout";

  receive_out_band_event : constant unsigned_long
    with Convention => C, import,
    External_Name   =>  "adare_epoll_epollpri";


  graceful_closed_socket_err : constant unsigned_long
    with Convention => C, import,
    External_Name   =>  "adare_epoll_epollrdhup";

  hang_up_socket_err : constant unsigned_long
    with Convention => C, import,
    External_Name   =>  "adare_epoll_epollhup";

  socket_err : constant unsigned_long
    with Convention => C, import,
    External_Name   =>  "adare_epoll_epollerr";


  function update
    (how  : not null epoll_access;
     with_sock  : not null socket_access;
     with_event_bitmap  : unsigned_long) return Boolean
    with Pre => is_initialized (how)
                and then initialized  (with_sock)
                and then is_in (how, with_sock);

  function remove
    (how  : not null epoll_access;
     what_sock  : not null socket_access) return Boolean
    with Pre => is_initialized (how)
                and then initialized  (what_sock)
                and then is_in (how, what_sock);

  function add
    (how  : not null epoll_access;
     with_sock  : not null socket_access;
     with_event_bitmap  : unsigned_long) return Boolean
    with Pre => is_initialized (how)
                and then initialized  (with_sock)
                and then (not is_in (how, with_sock));


  function poll_wait
    (poll  : not null epoll_access;
     timeout  : int
    ) return int
    with Pre => is_initialized (poll);

  procedure reset_poll_result
    (poll  : not null epoll_access)
    with Pre => is_initialized (poll);


  function confirm_send_event
    (where_poll : not null epoll_access;
     how        : not null socket_access) return Boolean
    with Pre => is_initialized (where_poll);

  function confirm_receive_event
    (where_poll : not null epoll_access;
     how        : not null socket_access) return Boolean
    with Pre => is_initialized (where_poll)
                and then initialized (how);

  function confirm_accept_event
    (where_poll : not null epoll_access;
     how        : not null socket_access) return Boolean
    with Pre => is_initialized (where_poll)
                and then initialized (how);

  function confirm_receive_out_band_event
    (where_poll : not null epoll_access;
     how        : not null socket_access) return Boolean
    with Pre => is_initialized (where_poll)
                and then initialized (how);


  function is_gracefull_shutdown_error
    (where_poll : not null epoll_access;
     how        : not null socket_access) return Boolean
    with Pre => is_initialized (where_poll)
                and then initialized (how);

  function is_hangup_error
    (where_poll : not null epoll_access;
     how        : not null socket_access) return Boolean
    with Pre => is_initialized (where_poll)
                and then initialized (how);

  function is_other_error
    (where_poll : not null epoll_access;
     how        : not null socket_access) return Boolean
    with Pre => is_initialized (where_poll)
                and then initialized (how);

  function init
    (poll     : out epoll_access;
     min_qtie : int := 15
    ) return Boolean
    with Pre => (not is_initialized (poll))
                 and then min_qtie > 0;

  function close
    (poll     : in out epoll_access
    ) return Boolean;

  function deinit
    (poll     : in out epoll_access
    ) return Boolean renames close;


private

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

    type epoll_event_array is array (int range <>) of epoll_event
        with Convention => C, preelaborable_initialization;

    type epoll_event_array_access is access all epoll_event_array;

    type socket_array is array (int range <>) of socket_type;

    type socket_array_access is access all socket_array;

    type epoll is limited
      record
        initialized : Boolean     := False;
        handle      : handle_type := failed_handle;
        event_poll  : epoll_event_array_access  := null;
        socket_poll : socket_array_access       := null;
        count       : int := 0;
        last_wait_returned  : int := 0;
      end record
      with preelaborable_initialization;

    epoll_add : constant int
      with  Convention => C, Import,
            External_Name => "adare_epoll_cmd_add";

    epoll_mod : constant int
      with  Convention => C, Import,
            External_Name => "adare_epoll_cmd_mod";

    epoll_del : constant int
      with  Convention => C, Import,
            External_Name => "adare_epoll_cmd_del";


end adare_net.base.waits;
