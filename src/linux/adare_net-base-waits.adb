
with adare_net.base.inners;

use adare_net.base, adare_net.base.inners;

package body adare_net.base.waits
  with Preelaborate
is
  pragma Assertion_Policy (Check);

  function is_initialized (poll : epoll_access) return Boolean is
   (poll /= null and then poll.all.initialized = True);


  function is_in
   (how : not null epoll_access; what_sock : not null socket_access)
    return Boolean
  is
  begin
    if how.socket_poll = null then
      return False;
    end if;

    b1 :
    declare
      sock : constant socket_type := get_socket (what_sock.all);
    begin
      return (for some E of how.socket_poll.all => E = sock);
    end b1;
  end is_in;


  function update
   (how : not null epoll_access; with_sock : not null socket_access;
    with_event_bitmap : unsigned_long) return Boolean
  is
    sock  : constant socket_type := get_socket (with_sock.all);
    event : epoll_event;
  begin
    event.events := Unsigned_32 (with_event_bitmap);

    --  if Alire_Host_OS = "windows" then
    --    event.data.sock := sock;
    --  else
    event.data.fd := int (sock);
    --  end if;

    return (0 = inner_epoll_ctl (how.handle, epoll_mod, sock, event'Address));
  end update;


  function remove
   (how : not null epoll_access; what_sock : not null socket_access) return Boolean
  is
    sock  : constant socket_type  := get_socket (what_sock.all);
    ok    : constant Boolean      :=
     (0 = inner_epoll_ctl (how.handle, epoll_del, sock, Null_Address));
    index : int                  := 0;
  begin
    if not ok then
      return False;
    end if;

    loop0 :
    for E of how.socket_poll.all loop
      index := index + 1;
      exit loop0 when E = sock;
    end loop loop0;

    if how.socket_poll.all (index) = sock then
      how.count                   := how.count - 1;
      how.socket_poll.all (index) := invalid_socket;
    end if;

    return True;
  end remove;

  function add
   (how : not null epoll_access; with_sock : not null socket_access;
    with_event_bitmap : unsigned_long) return Boolean
  is
    index : int := 0;
    sock  : constant socket_type  := get_socket (with_sock.all);
    event : epoll_event;
    ok    : Boolean               := False;

  begin
    if how.count >= how.socket_poll.all'Length then
      loop0 :
      for E of how.socket_poll.all loop
        if E = invalid_socket then
          index := index + 1;
        end if;
      end loop loop0;

      if index /= 0 then
        how.count :=  how.count - index;
      else
        b1 :
        declare
          tmp_epa : epoll_event_array :=
           (1 .. how.event_poll.all'Length + 15 => <>);
          tmp_spa : socket_array      :=
           (1 .. how.event_poll.all'Length + 15 => invalid_socket);
        begin
          tmp_epa (1 .. how.event_poll.all'Length) := how.event_poll.all;
          tmp_spa (1 .. how.event_poll.all'Length) := how.socket_poll.all;
          how.event_poll := new epoll_event_array'(tmp_epa);
          how.socket_poll := new socket_array'(tmp_spa);
        end b1;
      end if;
    end if;

    event.events := Unsigned_32 (with_event_bitmap);

    --  if Alire_Host_OS = "windows" then
    --    event.data.sock := sock;
    --  else
    event.data.fd := int (sock);
    --  end if;

    ok := (0 = inner_epoll_ctl (how.handle, epoll_add, sock, event'Address));

    if not ok then
      return False;
    end if;

    index := 0;

    loop1 :
    for E of how.socket_poll.all loop
      index := index + 1;
      exit loop1 when E = invalid_socket;
    end loop loop1;

    if not (how.socket_poll.all (index) = invalid_socket) then
      return False;
    end if;

    how.socket_poll (index) := sock;
    how.count               := how.count + 1;

    return True;

  end add;


  function poll_wait
    (poll  : not null epoll_access;
     timeout  : int
    ) return int
  is
  begin
    poll.last_wait_returned := inner_epoll_wait (poll.handle, poll.event_poll.all (1)'Address,
      poll.count, timeout);

    --  poll.last_wait_returned := inner_epoll_wait (poll.handle, poll.event_poll.all'Address,
    --    poll.event_poll.all'length, timeout); -- ToDo: poll.event_poll.all'length or poll.count ?
    return poll.last_wait_returned;
  end poll_wait;

  procedure reset_poll_result
    (poll  : not null epoll_access)
  is
  begin
    poll.last_wait_returned := 0;
    poll.event_poll.all := (others => <>);
  end reset_poll_result;


  function confirm_send_event
   (where_poll : not null epoll_access; how : not null socket_access)
    return Boolean
  is
  begin
    if where_poll.last_wait_returned <= 0 then
      return False;
    end if;

    b0 :
    declare
      mi_socket : constant socket_type := get_socket (how.all);
      index : int := 0;
    begin

      loop0 :
      for E of where_poll.event_poll.all (1 .. where_poll.last_wait_returned) loop
        index := index + 1;
        exit loop0 when E.data.fd = int (mi_socket);
        --  exit loop0 when (if Alire_Host_OS /= "windows" then E.data.fd = int (mi_socket) else E.data.sock = mi_socket);
      end loop loop0;

      --  if Alire_Host_OS /= "windows" then
      if where_poll.event_poll.all (index).data.fd /= int (mi_socket) then
        return False;
      end if;

      return ((where_poll.event_poll.all (index).events and Unsigned_32 (send_event)) > 0);
      --  end if;

      --  if where_poll.event_poll.all (index).data.sock /= mi_socket then
      --    return False;
      --  end if;

      --  return ((where_poll.event_poll.all (index).events and Unsigned_32 (send_event)) > 0);
    end b0;
  end confirm_send_event;

  function confirm_receive_event
   (where_poll : not null epoll_access; how : not null socket_access)
    return Boolean
  is
  begin
    if where_poll.last_wait_returned <= 0 then
      return False;
    end if;

    b0 :
    declare
      --  use Adare_Net_Config;
      mi_socket : constant socket_type := get_socket (how.all);
      index : int := 0;
    begin
      loop0 :
      for E of where_poll.event_poll.all (1 .. where_poll.last_wait_returned) loop
        index := index + 1;
        exit loop0 when E.data.fd = int (mi_socket);
        --  exit loop0 when (if Alire_Host_OS /= "windows" then E.data.fd = int (mi_socket) else E.data.sock = mi_socket);
      end loop loop0;

      --  if Alire_Host_OS /= "windows" then
      if where_poll.event_poll.all (index).data.fd /= int (mi_socket) then
        return False;
      end if;

      return ((where_poll.event_poll.all (index).events and Unsigned_32 (receive_event)) > 0);
      --  end if;

      --  if where_poll.event_poll.all (index).data.sock /= mi_socket then
      --    return False;
      --  end if;

      --  return ((where_poll.event_poll.all (index).events and Unsigned_32 (receive_event)) > 0);
    end b0;
  end confirm_receive_event;

  function confirm_accept_event
   (where_poll : not null epoll_access; how : not null socket_access)
    return Boolean
  is
  begin
    if where_poll.last_wait_returned <= 0 then
      return False;
    end if;

    b0 :
    declare

      mi_socket : constant socket_type := get_socket (how.all);
      index : int := 0;
    begin
      loop0 :
      for E of where_poll.event_poll.all (1 .. where_poll.last_wait_returned) loop
        index := index + 1;
        exit loop0 when E.data.fd = int (mi_socket);
        --  exit loop0 when (if Alire_Host_OS /= "windows" then E.data.fd = int (mi_socket) else E.data.sock = mi_socket);
      end loop loop0;

      --  if Alire_Host_OS /= "windows" then
      if where_poll.event_poll.all (index).data.fd /= int (mi_socket) then
        return False;
      end if;

      return ((where_poll.event_poll.all (index).events and Unsigned_32 (accept_socket_event)) > 0);
      --  end if;

      --  if where_poll.event_poll.all (index).data.sock /= mi_socket then
      --    return False;
      --  end if;

      --  return ((where_poll.event_poll.all (index).events and Unsigned_32 (accept_socket_event)) > 0);
    end b0;
  end confirm_accept_event;


  function init (poll : out epoll_access; min_qtie : int := 15) return Boolean
  is
    tmp_epoll_handle : constant handle_type := inner_epoll_create1 (0);
  begin
    if tmp_epoll_handle = failed_handle then
      return False;
    end if;

    poll :=
     new epoll'
      (handle       => tmp_epoll_handle,
       event_poll   => new epoll_event_array'(1 .. min_qtie => <>),
       socket_poll  => new socket_array'(1 .. min_qtie => invalid_socket),
       initialized  => True, count => 0, last_wait_returned => 0);

    return True;
  end init;

  function close (poll : in out epoll_access) return Boolean
  is
    ok      : Boolean := True;
    tmp_spa : constant socket_array := poll.socket_poll.all;
    handle  : constant handle_type :=  poll.handle;

  begin

    poll.initialized  :=  False;
    poll.count        :=  0;
    poll.socket_poll  :=  null;
    poll.event_poll   :=  null;
    poll.handle       :=  failed_handle;
    poll.last_wait_returned :=  0;
    poll  :=  null;

    loop0 :
    for E of tmp_spa loop
      if E /= invalid_socket then
        ok := ok and (0 = inner_epoll_ctl (handle, epoll_del, E, Null_Address));
      end if;
    end loop loop0;

    return ok and (0 = inner_epoll_close (handle));
  end close;

end adare_net.base.waits;
