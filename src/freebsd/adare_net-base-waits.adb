
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
      return (for some E of poll.socket_poll.all => E.sock = tmp_sock);
    end b1;
  end is_in;

  function poll_wait
    (poll  : aliased in out poll_of_events;
     miliseconds_timeout  : int
    ) return Boolean
  is
  begin
    poll.last_wait_returned := inner_kevent
      (poll.handle, Null_Address, 0,
        poll.event_poll.all (1)'Address, poll.count,
        inner_misec_to_timespec (miliseconds_timeout)
      );

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
    tmp_indx  : int := poll.socket_poll.all'First - 1;
  begin

    loop0 :
    for E of poll.socket_poll.all loop
      tmp_indx := tmp_indx + 1;
      exit loop0 when E.sock = tmp_sock;
    end loop loop0;

    if poll.socket_poll.all (tmp_indx).sock /= tmp_sock then
      return False;
    end if;

    if poll.socket_poll.all (tmp_indx).ev = event_bitmap then
      return True;
    end if;

    b0 :
    declare
      tmp_event : kernel_event :=
        (ident  => tmp_sock,
         filter => poll.socket_poll.all (tmp_indx).ev,
         flags  => kpoll_flag_del,
         fflags => 0,
         data   => 0,
         udata  => Null_Address);
    begin

      if inner_kevent (poll.handle, tmp_event'Address, 1,
                       Null_Address, 0, Null_Address) = -1
      then

        return False;
      end if;

      tmp_event := (ident => tmp_sock, filter => event_bitmap,
                    flags => kpoll_flag_add or kpoll_flag_clear,
                    fflags => 0, data => 0, udata => Null_Address
                   );

      -- tmp_event.flags   := kpoll_flag_add or kpoll_flag_enable;

      return not (inner_kevent (poll.handle, tmp_event'Address, 1,
        Null_Address, 0, Null_Address) = -1);

    end b0;
  end update;


  function remove
    (poll  : aliased in out poll_of_events;
     sock  : socket) return Boolean
  is
    tmp_sock  : constant socket_type := get_socket (sock);
    tmp_indx  : int := poll.socket_poll.all'First - 1;
  begin

    loop0 :
    for E of poll.socket_poll.all loop
      tmp_indx := tmp_indx + 1;
      exit loop0 when E.sock = tmp_sock;
    end loop loop0;

    if poll.socket_poll.all (tmp_indx).sock /= tmp_sock then
      return False;
    end if;

    b0 :
    declare
      tmp_event : kernel_event :=
        (ident  => tmp_sock,
         filter => poll.socket_poll.all (tmp_indx).ev,
         flags  => kpoll_flag_del,
         fflags => 0,
         data   => 0,
         udata  => Null_Address);
    begin

      if inner_kevent (poll.handle, tmp_event'Address, 1,
                       Null_Address, 0, Null_Address) = -1
      then

        return False;
      end if;

      poll.count  := poll.count - 1;
      poll.socket_poll.all (tmp_indx) := (sock => invalid_socket, ev => 0);

      return True;
    end b0;
  end remove;

  function add
   (poll  : aliased in out poll_of_events;
    sock  : socket;
    event_bitmap  : short) return Boolean
  is
    index     : int := 0;
    tmp_sock  : constant socket_type  := get_socket (sock);

  begin

    if poll.count >= poll.socket_poll.all'Length then
      loop0 :
      for E of poll.socket_poll.all loop
        if E.sock = invalid_socket then
          index := index + 1;
        end if;
      end loop loop0;

      if index /= 0 then
        poll.count :=  poll.count - index;
      else
        b1 :
        declare
          tmp_epa : kernel_event_array :=
           (1 .. poll.event_poll.all'Length + 15 => <>);
          tmp_spa : socket_kevent_array :=
           (1 .. poll.event_poll.all'Length + 15 => (sock => invalid_socket, ev => 0));
        begin
          tmp_epa (1 .. poll.event_poll.all'Length) := poll.event_poll.all;
          tmp_spa (1 .. poll.event_poll.all'Length) := poll.socket_poll.all;
          poll.event_poll := new kernel_event_array'(tmp_epa);
          poll.socket_poll := new socket_kevent_array'(tmp_spa);
        end b1;
      end if;
    end if;

    b2 :
    declare
      tmp_event : kernel_event :=
        (ident  => tmp_sock,
         filter => event_bitmap,
         flags  => kpoll_flag_add or kpoll_flag_clear,
         fflags => 0,
         data   => 0,
         udata  => Null_Address);
    begin

      if inner_kevent (poll.handle, tmp_event'Address, 1,
                       Null_Address, 0, Null_Address) = -1
      then

        return False;
      end if;

      index := poll.socket_poll.all'First - 1;

      loop1 :
      for E of poll.socket_poll.all loop
        index := index + 1;
        exit loop1 when E.sock = invalid_socket;
      end loop loop1;

      if not (poll.socket_poll.all (index).sock = invalid_socket) then
        return False;
      end if;

      poll.socket_poll.all (index) := (sock => tmp_sock, ev => event_bitmap);
      poll.count               := poll.count + 1;

      return True;
    end b2;
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
    mi_socket : constant socket_type := get_socket (sock);
  begin
    if poll.last_wait_returned <= 0 then -- poll.count ?
      return False;
    end if;

    return (for some E of poll.event_poll.all (1 .. poll.last_wait_returned)
        => E.ident = mi_socket
        and then E.filter = send_event
        and then (E.flags and kpoll_flag_error) = 0);

  end is_send;

  function is_receive
    (poll  : aliased in poll_of_events;
     sock  : socket) return Boolean
  is
    mi_socket : constant socket_type := get_socket (sock);
  begin
    if poll.last_wait_returned <= 0 then
      return False;
    end if;

    return (for some E of poll.event_poll.all (1 .. poll.last_wait_returned)
        => E.ident = mi_socket
        and then E.filter = receive_event
        and then (E.flags and kpoll_flag_error) = 0);

  end is_receive;

  function init
    (poll     : aliased in out poll_of_events;
     min_qtie : int := 2
    ) return Boolean
  is
    tmp_epoll_handle : constant handle_type := inner_kqueue;
  begin
    if tmp_epoll_handle = failed_handle then
      return False;
    end if;

    poll.handle       :=  tmp_epoll_handle;
    poll.event_poll   :=  new kernel_event_array'(1 .. min_qtie => <>);
    poll.socket_poll  :=  new socket_kevent_array'(1 .. min_qtie => (sock => invalid_socket, ev => 0));
    poll.initialized  :=  True;
    poll.count        :=  0;
    poll.last_wait_returned :=  0;

    return True;
  end init;

  procedure close
    (poll     : aliased in out poll_of_events)
  is
    tmp_res : int := 0
      with Unreferenced;

  begin

    if poll.count < 1 then
      tmp_res := inner_close (poll.handle);

      poll.initialized  :=  False;
      poll.count        :=  0;
      poll.socket_poll  :=  null;
      poll.event_poll   :=  null;
      poll.handle       :=  failed_handle;
      poll.last_wait_returned :=  0;

      return;
    end if;

    b1 :
    declare
      tmp_kevent  : kernel_event_array  := (1 .. poll.count => (ident  => invalid_socket,
         filter => 0, flags  => kpoll_flag_del, fflags => 0,  data   => 0,  udata  => Null_Address));
      tmp_indx    : int := tmp_kevent'First - 1;
      tmp_indx2   : int := 0;

    begin
      loop0 :
      for E of poll.socket_poll.all loop
        if E.sock /= invalid_socket then
          tmp_indx := tmp_indx + 1;
          tmp_kevent (tmp_indx).ident   := E.sock;
          tmp_kevent (tmp_indx).filter  := E.ev;
          tmp_indx2 := tmp_indx2 + 1;
        end if;
      end loop loop0;

      if tmp_indx2 > 0 then
        tmp_res :=  inner_kevent
          (poll.handle, tmp_kevent'Address, tmp_indx2,
          Null_Address, 0,  Null_Address);
      end if;

      tmp_res := inner_close (poll.handle);

      poll.initialized  :=  False;
      poll.count        :=  0;
      poll.socket_poll  :=  null;
      poll.event_poll   :=  null;
      poll.handle       :=  failed_handle;
      poll.last_wait_returned :=  0;

      return;
    end b1;
  end close;

end adare_net.base.waits;
