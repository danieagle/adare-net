
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
    External_Name   =>  "adare_poll_filter_read";

  send_event : constant short
    with Convention => C, import,
    External_Name   =>  "adare_poll_filter_write";

  type poll_fd is
    record
      fd      : socket_type     := -1;
      events  : short           := 0;
      revents : short           := 0;
    end record
      with Convention => C, preelaborable_initialization;


  type poll_fd_array is array (int range <>) of poll_fd
    with Convention => C, preelaborable_initialization;

  type poll_fd_array_access is access all poll_fd_array;


  type poll_of_events is
    record
      initialized : Boolean := False;
      event_poll  : poll_fd_array_access  :=  null;
      count       : int := 0;
      last_wait_returned  : int := 0;
    end record
      with Convention => C, preelaborable_initialization;

end adare_net.base.waits;
