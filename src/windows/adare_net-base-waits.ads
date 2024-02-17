
private with System;

package adare_net.base.waits
  with Preelaborate
is
  pragma  Assertion_Policy (Check);

  type poll_of_events is limited private;

  function poll_wait
    (poll  : aliased in out poll_of_events;
     miliseconds_timeout  : int
    ) return Boolean
    with Pre => is_initialized (poll) and miliseconds_timeout > 0;

  function set_receive
    (poll  : aliased in out poll_of_events;
     sock  : aliased in socket) return Boolean
    with Pre => is_initialized (sock);

  function set_send
    (poll  : aliased in out poll_of_events;
     sock  : aliased in socket) return Boolean
    with Pre => is_initialized (sock);

  function remove
    (poll  : aliased in out poll_of_events;
     sock  : aliased in socket) return Boolean
    with Pre => is_initialized (poll)
                and then is_initialized  (sock)
                and then is_in (poll, sock);

  function is_receive
    (poll  : aliased in poll_of_events;
     sock  : aliased in socket) return Boolean
    with Pre => is_initialized (sock) and then is_initialized (poll);

  function is_send
    (poll  : aliased in poll_of_events;
     sock  : aliased in socket) return Boolean
    with Pre => is_initialized (sock) and then is_initialized (poll);

  procedure reset_results
    (poll   : aliased in out poll_of_events)
     with Pre => is_initialized (poll);


  procedure close
    (poll     : aliased in out poll_of_events)
    with Pre => is_initialized (poll);

  procedure deinit
    (poll     : aliased in out poll_of_events)
      renames close;


  function is_initialized
    (poll : aliased in poll_of_events
    ) return Boolean;

  function is_in
    (poll  : aliased in poll_of_events;
     sock  : aliased in socket
    ) return Boolean
    with Pre => is_initialized (poll) and then is_initialized (sock);

private

  function update
    (poll  : aliased in out poll_of_events;
     sock  : aliased in socket;
     event_bitmap  : unsigned_long) return Boolean
    with Pre => is_initialized (poll)
                and then is_initialized  (sock)
                and then is_in (poll, sock);

  function add
    (poll  : aliased in out poll_of_events;
     sock  : aliased in socket;
     event_bitmap  : unsigned_long) return Boolean
    with Pre => is_initialized (poll)
                and then is_initialized  (sock)
                and then (not is_in (poll, sock));

  function init
    (poll     : aliased in out poll_of_events;
     min_qtie : int := 2
    ) return Boolean
    with Pre => (not is_initialized (poll))
                 and then min_qtie > 0;

  use System;

  receive_event : constant unsigned_long
    with Convention => C, import,
    External_Name   =>  "adare_epoll_epollin";

  accept_socket_event : constant unsigned_long
    with Convention => C, import,
    External_Name   =>  "adare_epoll_epollin";

  send_event : constant unsigned_long
    with Convention => C, import,
    External_Name   =>  "adare_epoll_epollout";

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

    type poll_of_events is limited
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
