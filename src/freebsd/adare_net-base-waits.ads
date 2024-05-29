
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

  send_event : constant short
    with Convention => C, import,
    External_Name   =>  "adare_kpoll_filter_write";

  type mi_unsigned_int is mod 2**(int'Size);
  type mi_int64_array is array (int range <>) of Integer_64
    with default_component_value => 0;

    type kernel_event is
      record
        ident   : adr_uintptr_t   := 0;
        filter  : short           := 0;
        flags   : unsigned_short  := 0;
        fflags  : mi_unsigned_int := 0;
        data    : Integer_64  := 0;
        udata   : Address     := Null_Address;
        ext     : mi_int64_array (1 .. 4) := (others => 0);
      end record
    with Convention => C, preelaborable_initialization;

    type kernel_event_array is array (int range <>) of kernel_event
      with Convention => C, preelaborable_initialization;

    type kernel_event_array_access is access all kernel_event_array;


    type kernel_event_info is
      record
        ident   : adr_uintptr_t   := 0;
        filter  : short           := 0;
        failed  : Boolean         := False;
      end record
    with Convention => C, preelaborable_initialization;

    type kernel_event_info_array is array (int range <>) of kernel_event_info
      with Convention => C, preelaborable_initialization;

    type kernel_event_info_array_access is access all kernel_event_info_array;


    type poll_of_events is
      record
        initialized : Boolean     := False;
        handle      : handle_type := failed_handle;
        event_info_poll  : kernel_event_info_array_access  :=  null;
        result_poll : kernel_event_array_access :=  null;
        count       : int := 0;
        last_wait_returned  : int := 0;
      end record
      with Convention => C, preelaborable_initialization;

    kpoll_flag_add : constant unsigned_short
      with  Convention => C, Import,
            External_Name => "adare_kpoll_flag_add";

    kpoll_flag_delete : constant unsigned_short
      with  Convention => C, Import,
            External_Name => "adare_kpoll_flag_delete";

    kpoll_flag_clear : constant unsigned_short
      with  Convention => C, Import,
      External_Name => "adare_kpoll_flag_clear";

    kpoll_flag_error : constant unsigned_short
      with  Convention => C, Import,
      External_Name => "adare_kpoll_flag_error";


end adare_net.base.waits;
