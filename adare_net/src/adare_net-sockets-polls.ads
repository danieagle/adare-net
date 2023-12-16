
package adare_net.sockets.polls
  with Preelaborate
is
  pragma  Assertion_Policy (Check);

  function "And" (left, right : Interfaces.C.short)
    return Interfaces.C.short;

  function "Or" (left, right : Interfaces.C.short)
    return Interfaces.C.short;

  type event_type is new Interfaces.C.short with
   Size => Interfaces.C.short'Size, Convention => C;

  receive_ev : constant event_type with
    Import => True, Convention => C, External_Name => "c_event_pollin";

  send_ev : constant event_type with
    Import => True, Convention => C, External_Name => "c_event_pollout";

  accept_ev : constant event_type with
    Import => True, Convention => C, External_Name => "c_event_pollin";


  type error_type is new Interfaces.C.short with
   Size => Interfaces.C.short'Size, Convention => C;

  poll_err : constant error_type with
    Import => True, Convention => C, External_Name => "c_event_pollerror";

  hang_up_err : constant error_type with
    Import => True, Convention => C, External_Name => "c_event_pollhup";

  socket_descritor_err : constant error_type with
    Import => True, Convention => C, External_Name => "c_event_pollnval";

  type poll_type (Len : int) is private
    with Static_Predicate => Len >= 1 and then Len < 256;

  function is_full
    (mi_poll : not null access poll_type) return Boolean;

  function is_empty
    (mi_poll : not null access poll_type) return Boolean;

  function is_in
    (in_poll    : not null access poll_type;
     what       : not null socket_access) return Boolean
     with  pre => initialized (what);

  procedure add_events
    (to_poll    : not null access poll_type;
     sock       : not null socket_access;
     with_events_bitmap : in event_type
    )
    with pre => not is_full (to_poll) and then initialized (sock) and then (not is_in (to_poll, sock));

  procedure clear_all_event_responses
    (to_poll    : not null access poll_type);

  procedure remove
    (from_poll   : not null access poll_type;
     what        : not null socket_access
    )
    with pre => not is_empty (from_poll) and then is_in (from_poll, what);

  procedure update
    (from_poll    : not null access poll_type;
     for_sock     : not null socket_access;
     with_events_bitmap : in event_type
    )
    with pre => initialized (for_sock) and then is_in (from_poll, for_sock);

  procedure reset_all (who  : not null access poll_type);

  subtype number_of_hits is int;

  function start_events_listen
    (from_poll : not null access poll_type;
     time_out   : int  := 15000
                      -- time_out < 0 => forever wait
                      -- time_out = 0 => no wait
                      -- time_out > 0 => miliseconds time_out wait
    ) return number_of_hits
    with pre => not is_empty (from_poll);

  function receive_event
    (from_poll    : not null access poll_type;
     for_sock     : not null socket_access) return Boolean
     with  pre => initialized (for_sock) and then is_in (from_poll, for_sock);

  function accept_event
    (from_poll    : not null access poll_type;
     for_sock     : not null socket_access) return Boolean
     with  pre => initialized (for_sock) and then is_in (from_poll, for_sock);

  function send_event
    (from_poll    : not null access poll_type;
     for_sock     : not null socket_access) return Boolean
     with  pre => initialized (for_sock) and then is_in (from_poll, for_sock);

  function poll_error
    (from_poll    : not null access poll_type;
     for_sock     : not null socket_access) return Boolean
     with  pre => initialized (for_sock) and then is_in (from_poll, for_sock);

  function hang_up_error
    (from_poll    : not null access poll_type;
     for_sock     : not null socket_access) return Boolean
     with  pre => initialized (for_sock) and then is_in (from_poll, for_sock);

  function socket_descritor_error
    (from_poll    : not null access poll_type;
     for_sock     : not null socket_access) return Boolean
     with  pre => initialized (for_sock) and then is_in (from_poll, for_sock);

private

  type pollfd is record
    fd      : aliased signed_socket_type := missing_file_descriptor;
    events  : aliased event_type         := 0;
    revents : aliased Interfaces.C.short := 0;
  end record with
    Convention => C, Preelaborable_Initialization;

  -- type pollfd is
  --   record
  --     fd      : aliased signed_socket_type := missing_file_descriptor;
  --     events  : aliased short := 0;
  --     revents : aliased short := 0;
  --   end record
  --   with Convention => C;
  null_pollfd : constant pollfd :=
    pollfd'(fd => missing_file_descriptor, events => 0, revents => 0);

  type pollfd_array is array (int range <>) of pollfd with
    Convention => C, Preelaborable_Initialization;

  type socket_type_array is array (int range <>) of socket_type
    with Convention => C, preelaborable_initialization;

  type poll_type (Len : int) is
    record
      count : aliased int := 0;
      pos   : aliased socket_type_array (1 .. Len);
      poll  : aliased pollfd_array (1 .. Len);
    end record;

end adare_net.sockets.polls;
