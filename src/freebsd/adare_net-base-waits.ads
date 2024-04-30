
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
     sock  : socket) return Boolean
    with Pre => is_initialized (sock);

  function set_send
    (poll  : aliased in out poll_of_events;
     sock  : socket) return Boolean
    with Pre => is_initialized (sock);

  function remove
    (poll  : aliased in out poll_of_events;
     sock  : socket) return Boolean
    with Pre => is_initialized (poll)
                and then is_initialized  (sock)
                and then is_in (poll, sock);

  function is_receive
    (poll  : aliased in poll_of_events;
     sock  : socket) return Boolean
    with Pre => is_initialized (sock) and then is_initialized (poll);

  function is_send
    (poll  : aliased in poll_of_events;
     sock  : socket) return Boolean
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
     sock  : socket
    ) return Boolean
    with Pre => is_initialized (poll) and then is_initialized (sock);

private

  function update
    (poll  : aliased in out poll_of_events;
     sock  : socket;
     event_bitmap  : short) return Boolean
    with Pre => is_initialized (poll)
                and then is_initialized  (sock)
                and then is_in (poll, sock);

  function add
    (poll  : aliased in out poll_of_events;
     sock  : socket;
     event_bitmap  : short) return Boolean
    with Pre => is_initialized (poll)
                and then is_initialized  (sock)
                and then (not is_in (poll, sock));

  function init
    (poll     : aliased in out poll_of_events;
     min_qtie : int := 2
    ) return Boolean
    with Pre => (not is_initialized (poll))
                 and then min_qtie > 0;

  receive_event : constant short
    with Convention => C, import,
    External_Name   =>  "adare_kpoll_filter_read";

  accept_socket_event : constant short
    with Convention => C, import,
    External_Name   =>  "adare_kpoll_filter_read";

  send_event : constant short
    with Convention => C, import,
    External_Name   =>  "adare_kpoll_filter_write";


  type ext_e  is array (unsigned_8 range <>) of Unsigned_64
    with Convention => C, default_component_value => 0,
    preelaborable_initialization;

  -- type kevent_cache_array

    type kernel_event is
      record
        ident   : Address := Null_Address;
        filter  : short   := 0;
        flags   : unsigned_short := 0;
        fflags  : unsigned    := 0;
        data    : integer_64  := 0;
        udate   : Address     := Null_Address;
        ext     : ext_e (1 .. 4)  := (others => 0);
      end record
    with Convention => C, preelaborable_initialization;

    type kernel_event_array is array (int range <>) of kernel_event
        with Convention => C, preelaborable_initialization;

    type kernel_event_array_access is access all kernel_event_array;

    type socket_kevent is
      record
        sock  : socket_type := -1;
        ev    : short := 0;
      end record
    with preelaborable_initialization;

    type socket_kevent_array is array (int range <>) of socket_kevent;

    type socket_kevent_array_access is access all socket_array;

    type poll_of_events is limited
      record
        initialized : Boolean     := False;
        handle      : handle_type := failed_handle;
        event_poll  : kernel_event_array_access   := null;
        socket_poll : socket_kevent_array_access  := null;
        count       : int := 0;
        last_wait_returned  : int := 0;
      end record
      with preelaborable_initialization;

    kpoll_flag_add : constant unsigned_short
      with  Convention => C, Import,
            External_Name => "adare_kpoll_flag_add";

    kpoll_flag_enable : constant unsigned_short
      with  Convention => C, Import,
            External_Name => "adare_kpoll_flag_enable";

    kpoll_flag_del : constant unsigned_short
      with  Convention => C, Import,
      External_Name => "adare_kpoll_flag_del";


end adare_net.base.waits;
