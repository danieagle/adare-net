
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
    if poll.socket_poll = null then
      return False;
    end if;

    b1 :
    declare
      tmp_sock : constant socket_type := get_socket (sock);
    begin
      return (for some E of poll.socket_poll.all => E = tmp_sock);
    end b1;
  end is_in;

  function poll_wait
    (poll  : aliased in out poll_of_events;
     miliseconds_timeout  : int
    ) return Boolean
  is
  begin
    poll.last_wait_returned := inner_epoll_wait (poll.handle, poll.event_poll.all (1)'Address,
      poll.count, miliseconds_timeout);

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
    event_bitmap  : unsigned_long) return Boolean
  is
    tmp_sock  : constant socket_type := get_socket (sock);
    event     : epoll_event;
  begin
    event.events := Unsigned_32 (event_bitmap);

    --  if Alire_Host_OS = "windows" then
    --    event.data.sock := sock;
    --  else
    event.data.fd := int (tmp_sock);
    --  end if;

    return (0 = inner_epoll_ctl (poll.handle, epoll_mod, tmp_sock, event'Address));
  end update;


  function remove
    (poll  : aliased in out poll_of_events;
     sock  : socket) return Boolean
  is
    tmp_sock  : constant socket_type  := get_socket (sock);
    ok        : constant Boolean      :=
     (0 = inner_epoll_ctl (poll.handle, epoll_del, tmp_sock, Null_Address));
    index : int                  := 0;
  begin
    if not ok then
      return False;
    end if;

    loop0 :
    for E of poll.socket_poll.all loop
      index := index + 1;
      exit loop0 when E = tmp_sock;
    end loop loop0;

    if poll.socket_poll.all (index) = tmp_sock then
      poll.count                   := poll.count - 1;
      poll.socket_poll.all (index) := invalid_socket;
    end if;

    return True;
  end remove;

  function add
   (poll  : aliased in out poll_of_events;
    sock  : socket;
    event_bitmap  : unsigned_long) return Boolean
  is
    index : int := 0;
    tmp_sock  : constant socket_type  := get_socket (sock);
    event : epoll_event;
    ok    : Boolean               := False;

  begin
    if poll.count >= poll.socket_poll.all'Length then
      loop0 :
      for E of poll.socket_poll.all loop
        if E = invalid_socket then
          index := index + 1;
        end if;
      end loop loop0;

      if index /= 0 then
        poll.count :=  poll.count - index;
      else
        b1 :
        declare
          tmp_epa : epoll_event_array :=
           (1 .. poll.event_poll.all'Length + 15 => <>);
          tmp_spa : socket_array      :=
           (1 .. poll.event_poll.all'Length + 15 => invalid_socket);
        begin
          tmp_epa (1 .. poll.event_poll.all'Length) := poll.event_poll.all;
          tmp_spa (1 .. poll.event_poll.all'Length) := poll.socket_poll.all;
          poll.event_poll := new epoll_event_array'(tmp_epa);
          poll.socket_poll := new socket_array'(tmp_spa);
        end b1;
      end if;
    end if;

    event.events := Unsigned_32 (event_bitmap);

    --  if Alire_Host_OS = "windows" then
    --    event.data.sock := sock;
    --  else
    event.data.fd := int (tmp_sock);
    --  end if;

    ok := (0 = inner_epoll_ctl (poll.handle, epoll_add, tmp_sock, event'Address));

    if not ok then
      return False;
    end if;

    index := 0;

    loop1 :
    for E of poll.socket_poll.all loop
      index := index + 1;
      exit loop1 when E = invalid_socket;
    end loop loop1;

    if not (poll.socket_poll.all (index) = invalid_socket) then
      return False;
    end if;

    poll.socket_poll (index) := tmp_sock;
    poll.count               := poll.count + 1;

    return True;
  end add;

  procedure reset_results
    (poll   : aliased in out poll_of_events)
  is
  begin
    poll.last_wait_returned := 0;
    poll.event_poll.all := (others => <>);
  end reset_results;


  function is_send
    (poll  : aliased in poll_of_events;
     sock  : socket) return Boolean
  is
  begin
    if poll.last_wait_returned <= 0 then
      return False;
    end if;

    b0 :
    declare
      mi_socket : constant socket_type := get_socket (sock);
      index : int := 0;
    begin

      loop0 :
      for E of poll.event_poll.all (1 .. poll.last_wait_returned) loop
        index := index + 1;
        exit loop0 when E.data.fd = int (mi_socket);
        --  exit loop0 when (if Alire_Host_OS /= "windows" then E.data.fd = int (mi_socket) else E.data.sock = mi_socket);
      end loop loop0;

      --  if Alire_Host_OS /= "windows" then
      if poll.event_poll.all (index).data.fd /= int (mi_socket) then
        return False;
      end if;

      return ((poll.event_poll.all (index).events and Unsigned_32 (send_event)) > 0);
      --  end if;

      --  if where_poll.event_poll.all (index).data.sock /= mi_socket then
      --    return False;
      --  end if;

      --  return ((where_poll.event_poll.all (index).events and Unsigned_32 (send_event)) > 0);
    end b0;
  end is_send;

  function is_receive
    (poll  : aliased in poll_of_events;
     sock  : socket) return Boolean
  is
  begin
    if poll.last_wait_returned <= 0 then
      return False;
    end if;

    b0 :
    declare
      --  use Adare_Net_Config;
      mi_socket : constant socket_type := get_socket (sock);
      index : int := 0;
    begin
      loop0 :
      for E of poll.event_poll.all (1 .. poll.last_wait_returned) loop
        index := index + 1;
        exit loop0 when E.data.fd = int (mi_socket);
        --  exit loop0 when (if Alire_Host_OS /= "windows" then E.data.fd = int (mi_socket) else E.data.sock = mi_socket);
      end loop loop0;

      --  if Alire_Host_OS /= "windows" then
      if poll.event_poll.all (index).data.fd /= int (mi_socket) then
        return False;
      end if;

      return ((poll.event_poll.all (index).events and Unsigned_32 (receive_event)) > 0);
      --  end if;

      --  if where_poll.event_poll.all (index).data.sock /= mi_socket then
      --    return False;
      --  end if;

      --  return ((where_poll.event_poll.all (index).events and Unsigned_32 (receive_event)) > 0);
    end b0;
  end is_receive;

  function init
    (poll     : aliased in out poll_of_events;
     min_qtie : int := 2
    ) return Boolean
  is
    tmp_epoll_handle : constant handle_type := inner_epoll_create1 (0);
  begin
    if tmp_epoll_handle = failed_handle then
      return False;
    end if;

    poll.handle       :=  tmp_epoll_handle;
    poll.event_poll   :=  new epoll_event_array'(1 .. min_qtie => <>);
    poll.socket_poll  :=  new socket_array'(1 .. min_qtie => invalid_socket);
    poll.initialized  :=  True;
    poll.count        :=  0;
    poll.last_wait_returned :=  0;

    return True;
  end init;

  procedure close
    (poll     : aliased in out poll_of_events)
  is
    tmp_spa : constant socket_array := poll.socket_poll.all;
    handle  : constant handle_type :=  poll.handle;
    tmp_res : int := 0
      with Unreferenced;

  begin

    poll.initialized  :=  False;
    poll.count        :=  0;
    poll.socket_poll  :=  null;
    poll.event_poll   :=  null;
    poll.handle       :=  failed_handle;
    poll.last_wait_returned :=  0;

    loop0 :
    for E of tmp_spa loop
      if E /= invalid_socket then
        tmp_res := inner_epoll_ctl (handle, epoll_del, E, Null_Address);
      end if;
    end loop loop0;

    tmp_res := inner_epoll_close (handle);
  end close;

end adare_net.base.waits;
