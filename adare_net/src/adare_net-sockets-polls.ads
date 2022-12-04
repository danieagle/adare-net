
package adare_net.sockets.polls
  with Preelaborate
is
  pragma  Assertion_Policy (Check);

  type event_type is new unsigned_short;

  receive_ev  : constant event_type
    with Import => True, Convention => C, External_Name => "c_event_pollin";

  send_ev     : constant event_type
    with Import => True, Convention => C, External_Name => "c_event_pollout";

  accept_ev   : constant event_type
    with Import => True, Convention => C, External_Name => "c_event_pollin";


  type error_type is new unsigned_short;

  poll_err    : constant error_type
    with Import => True, Convention => C, External_Name => "c_event_pollerror";

  hang_up_err : constant error_type
    with Import => True, Convention => C, External_Name => "c_event_pollhup";

  socket_descritor_err  : constant error_type
    with Import => True, Convention => C, External_Name => "c_event_pollnval";


  type poll_type (Len : Unsigned_8) is private
    with Preelaborable_Initialization;

  function is_full
    (mi_poll : not null access poll_type) return Boolean;

  function is_empty
    (mi_poll : not null access poll_type) return Boolean;

  function is_in
    (in_poll    : not null access poll_type;
     what       : not null access socket
     ) return Boolean;

  procedure add_events
    (to_poll    : not null access poll_type;
     sock       : not null access socket;
     with_events_bitmap : in event_type
    )
    with pre => not is_full (to_poll) and then initialized (sock)
      and then (not is_in (to_poll, sock));

  procedure clear_all_event_responses
    (to_poll    : not null access poll_type);

  procedure remove
    (from_poll   : not null access poll_type;
     what         : not null access socket
    )
    with pre => not is_empty (from_poll) and then is_in (from_poll, what);

  procedure update
    (from_poll    : not null access poll_type;
     for_sock     : not null access socket;
     with_events_bitmap : in event_type
    )
    with pre => initialized (for_sock) and then is_in (from_poll, for_sock);

  procedure reset_all (who  : not null access poll_type);

  subtype number_of_hits is int;

  function start_events_listen
    (from_poll : not null access poll_type;
     time_out   : int -- time_out < 0 => forever wait
                      -- time_out = 0 => no wait
                      -- time_out > 0 => miliseconds time_out wait
    ) return number_of_hits
    with pre => not is_empty (from_poll);

  function receive_event
    (from_poll    : not null access poll_type;
     for_sock     : not null access socket
    ) return Boolean;

  function accept_event
    (from_poll    : not null access poll_type;
     for_sock     : not null access socket
    ) return Boolean;

  function send_event
    (from_poll    : not null access poll_type;
     for_sock     : not null access socket
    ) return Boolean;

  function poll_error
    (from_poll    : not null access poll_type;
     for_sock     : not null access socket
    ) return Boolean;

  function hang_up_error
    (from_poll    : not null access poll_type;
     for_sock     : not null access socket
    ) return Boolean;

  function socket_descritor_error
    (from_poll    : not null access poll_type;
     for_sock     : not null access socket
    ) return Boolean;

private

  type pollfd is
    record
      fd      : aliased socket_type;
      events  : aliased unsigned_short;
      revents : aliased unsigned_short;
    end record
    with Convention => C, Preelaborable_initialization;

  null_pollfd  : constant pollfd := (fd => -1, events => 0, revents => 0);

  type pollfd_array is array (Unsigned_8 range <>) of pollfd
    with Convention => C, Preelaborable_initialization;

  type socket_type_array is array (Unsigned_8 range <>) of socket_type
    with default_component_value => invalid_socket;

  type poll_type (Len : Unsigned_8) is
    record
      count : Unsigned_8 := 0;
      pos   : socket_type_array (1 .. Len);
      poll  : pollfd_array (1 .. Len);
    end record;

end adare_net.sockets.polls;
