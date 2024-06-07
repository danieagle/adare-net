
with adare_net.base.inners;

use adare_net.base, adare_net.base.inners;

package body adare_net.base.waits
  with Preelaborate
is
  pragma Assertion_Policy (Check);


  function is_initialized (poll : aliased in poll_of_events) return Boolean is
   (poll.initialized = True);


  function is_in
   (poll  : aliased in poll_of_events;
    sock  : socket
   ) return Boolean
  is
  begin
    if poll.event_poll = null then
      return False;
    end if;

    b1 :
    declare
      tmp_sock : constant socket_type := get_socket (sock);
    begin
      return (for some E of poll.event_poll.all => E.fd = tmp_sock);
    end b1;
  end is_in;

  function poll_wait
    (poll  : aliased in out poll_of_events;
     miliseconds_timeout  : int
    ) return Boolean
  is
  begin
    if poll.count < 1 then
      return False;
    end if;

    poll.last_wait_returned :=
      inner_poll (poll.event_poll.all'Address, Unsigned_16 (poll.count), miliseconds_timeout);

    return poll.last_wait_returned > 0;
  end poll_wait;

  function set_receive
    (poll  : aliased in out poll_of_events;
     sock  : socket) return Boolean
  is
    ok  : Boolean := True;

  begin
    if not is_initialized (poll) then
      ok := init (poll);

      if not ok then
        return False;
      end if;
    end if;

    if not is_in (poll, sock) then
      ok := add (poll, sock, receive_event);
    else
      ok := update (poll, sock, receive_event);
    end if;

    return ok;
  end set_receive;

  function set_send
    (poll  : aliased in out poll_of_events;
     sock  : socket) return Boolean
  is
    ok  : Boolean := True;

  begin
    if not is_initialized (poll) then
      ok := init (poll);

      if not ok then
        return False;
      end if;
    end if;

    if not is_in (poll, sock) then
      ok := add (poll, sock, send_event);
    else
      ok := update (poll, sock, send_event);
    end if;

    return ok;
  end set_send;

  function update
   (poll  : aliased in out poll_of_events;
    sock  : socket;
    event_bitmap  : short) return Boolean
  is
    tmp_sock  : constant socket_type := get_socket (sock);
    tmp_indx  : int := poll.event_poll.all'First - 1;
  begin

    loop0 :
    for E of poll.event_poll.all loop
      tmp_indx := tmp_indx + 1;
      exit loop0 when E.fd = tmp_sock;
    end loop loop0;

    if poll.event_poll.all (tmp_indx).fd /= tmp_sock then
      return False;
    end if;

    poll.event_poll.all (tmp_indx).events   := event_bitmap;
    poll.event_poll.all (tmp_indx).revents  := 0;

    return True;
  end update;

  function remove
    (poll  : aliased in out poll_of_events;
     sock  : socket) return Boolean
  is
    tmp_sock  : constant socket_type := get_socket (sock);
    tmp_indx  : int := poll.event_poll.all'First - 1;
  begin
    if poll.count < 1 then
      return False;
    end if;

    loop0 :
    for E of poll.event_poll.all loop
      tmp_indx := tmp_indx + 1;
      exit loop0 when E.fd = tmp_sock;
    end loop loop0;

    if poll.event_poll.all (tmp_indx).fd /= tmp_sock then
      return False;
    end if;

    poll.event_poll.all (tmp_indx .. poll.event_poll.all'Last - 1) :=
        poll.event_poll.all (tmp_indx + 1 .. poll.event_poll.all'Last);

    poll.event_poll.all (poll.event_poll.all'Last) := (fd => -1, events => 0, revents => 0);

    poll.count := poll.count - 1;

    return True;
  end remove;

  function add
   (poll  : aliased in out poll_of_events;
    sock  : socket;
    event_bitmap  : short) return Boolean
  is
    tmp_sock    : constant socket_type := get_socket (sock);
    tmp_indx    : int     := poll.event_poll.all'First - 1;
    tmp_poll_fd : poll_fd := (fd => tmp_sock, events => event_bitmap, revents => 0);
  begin

    if poll.count >= poll.event_poll.all'Length then
      b1 :
      declare
        tmp_epa : poll_fd_array :=
          (1 .. poll.event_poll.all'Length + 15 => <>);
      begin
        tmp_epa (1 .. poll.event_poll.all'Length) := poll.event_poll.all;
        poll.event_poll  := new poll_fd_array'(tmp_epa);
      end b1;
    end if;

    if poll.count < 1 then
      poll.event_poll.all (tmp_indx + 1) := tmp_poll_fd;
      poll.count := 1;
      return True;
    end if;

    loop0 :
    for E of poll.event_poll.all (poll.event_poll.all'First .. (poll.event_poll.all'First + poll.count) - 1) loop
      tmp_indx := tmp_indx + 1;
      exit loop0 when E.fd = -1;
    end loop loop0;

    if poll.event_poll.all (tmp_indx).fd = -1 then
      poll.event_poll.all (tmp_indx) := tmp_poll_fd;
      return True;
    end if;

    poll.event_poll.all (poll.event_poll.all'First + poll.count) := tmp_poll_fd;
    poll.count  :=  poll.count + 1;

    return True;
  end add;

  procedure reset_results
    (poll   : aliased in out poll_of_events)
  is
  begin
    poll.last_wait_returned := 0;

    for E of poll.event_poll.all (poll.event_poll.all'First .. (poll.event_poll.all'First + poll.count) - 1) loop
      E.revents := 0;
    end loop;
  end reset_results;


  function is_send
    (poll  : aliased in poll_of_events;
     sock  : socket) return Boolean
  is
    mi_socket : constant socket_type := get_socket (sock);
  begin
    if poll.last_wait_returned < 1 then
      return False;
    end if;

    return (for some E of
            poll.event_poll.all (poll.event_poll.all'First .. (poll.event_poll.all'First + poll.count) - 1)
        => E.revents /= 0 and then E.fd = mi_socket and then inner_mi_and (E.revents, send_event) /= 0);

  end is_send;

  function is_receive
    (poll  : aliased in poll_of_events;
     sock  : socket) return Boolean
  is
    mi_socket : constant socket_type := get_socket (sock);
  begin
    if poll.last_wait_returned < 1 then
      return False;
    end if;

    return (for some E of
            poll.event_poll.all (poll.event_poll.all'First .. (poll.event_poll.all'First + poll.count) - 1)
        => E.revents /= 0 and then E.fd = mi_socket and then inner_mi_and (E.revents, receive_event) /= 0);

  end is_receive;

  function init
    (poll     : aliased in out poll_of_events;
     min_qtie : int := 2
    ) return Boolean
  is
  begin

    poll := (event_poll  => new poll_fd_array'(1 .. min_qtie => <>),
             initialized  => True, count  => 0, last_wait_returned => 0);

    return True;
  end init;

  procedure close
    (poll     : aliased in out poll_of_events)
  is
  begin

      poll.initialized  :=  False;
      poll.count        :=  0;
      poll.event_poll   :=  null;
      poll.last_wait_returned :=  0;

  end close;

end adare_net.base.waits;
