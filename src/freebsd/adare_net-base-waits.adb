
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
    if poll.event_info_poll = null then
      return False;
    end if;

    b1 :
    declare
      tmp_sock : constant adr_uintptr_t := adr_uintptr_t (socket_type'(get_socket (sock)));
    begin
      return (for some E of poll.event_info_poll.all => E.ident = tmp_sock);
    end b1;
  end is_in;

  function poll_wait
    (poll  : aliased in out poll_of_events;
     miliseconds_timeout  : int
    ) return Boolean
  is
  begin
    poll.last_wait_returned :=
      inner_mi_get_kevent (poll.handle, poll.result_poll.all(poll.result_poll.all'First)'Address, poll.count, miliseconds_timeout);

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
    tmp_sock  : constant adr_uintptr_t := adr_uintptr_t (socket_type'(get_socket (sock)));
    tmp_indx  : int := poll.event_info_poll.all'First - 1;
  begin

    loop0 :
    for E of poll.event_info_poll.all loop
      tmp_indx := tmp_indx + 1;
      exit loop0 when E.ident = tmp_sock;
    end loop loop0;

    if poll.event_info_poll.all (tmp_indx).ident /= tmp_sock then
      return False;
    end if;

    if poll.event_info_poll.all (tmp_indx).filter = event_bitmap and then
      poll.event_info_poll.all (tmp_indx).failed = False
    then
      return True;
    end if;

    b1 :
    declare
      mi_kevent : kernel_event :=
        (ident  => poll.event_info_poll.all (tmp_indx).ident,
         filter => poll.event_info_poll.all (tmp_indx).filter,
         flags  => kpoll_flag_delete,
         fflags => 0, data  => 0, udata => Null_Address, ext => (others => 0));

    begin
      if inner_mi_set_kevent (poll.handle, mi_kevent'Address, 1) = -1 then
        return False;
      end if;

      poll.event_info_poll.all (tmp_indx).filter := event_bitmap;

      mi_kevent.filter := event_bitmap;
      mi_kevent.flags  := kpoll_flag_add;

      if inner_mi_set_kevent (poll.handle, mi_kevent'Address, 1) = -1 then

        poll.event_info_poll.all (tmp_indx).failed := True;

        return False;
      end if;

      poll.event_info_poll.all (tmp_indx).failed := False;

      return True;
    end b1;
  end update;


  function remove
    (poll  : aliased in out poll_of_events;
     sock  : socket) return Boolean
  is
    tmp_sock  : constant adr_uintptr_t := adr_uintptr_t (socket_type'(get_socket (sock)));
    tmp_indx  : int := poll.event_info_poll.all'First - 1;
    tmp_res   : int := 0;
  begin

    loop0 :
    for E of poll.event_info_poll.all loop
      tmp_indx := tmp_indx + 1;
      exit loop0 when E.ident = tmp_sock;
    end loop loop0;

    if poll.event_info_poll.all (tmp_indx).ident /= tmp_sock then
      return False;
    end if;

    b1 :
    declare
      mi_kevent : kernel_event :=
        (ident  => poll.event_info_poll.all (tmp_indx).ident,
         filter => poll.event_info_poll.all (tmp_indx).filter,
         flags  => kpoll_flag_delete,
         fflags => 0, data  => 0, udata => Null_Address, ext => (others => 0));

    begin
      tmp_res :=  inner_mi_set_kevent (poll.handle, mi_kevent'Address, 1);

      poll.event_info_poll.all (tmp_indx .. poll.event_info_poll.all'Last - 1) :=
        poll.event_info_poll.all (tmp_indx + 1 .. poll.event_info_poll.all'Last);

      poll.count := poll.count - 1;

      return tmp_res /= -1;
    end b1;
  end remove;

  function add
   (poll  : aliased in out poll_of_events;
    sock  : socket;
    event_bitmap  : short) return Boolean
  is
    tmp_sock  : constant adr_uintptr_t := adr_uintptr_t (socket_type'(get_socket (sock)));

  begin

    if poll.count >= poll.event_info_poll.all'Length then
      b1 :
      declare
        tmp_epa : kernel_event_info_array :=
          (1 .. poll.event_info_poll.all'Length + 15 => <>);
        tmp_spa : kernel_event_array :=
          (1 .. poll.event_info_poll.all'Length + 15 => <>);
      begin
        tmp_epa (1 .. poll.event_info_poll.all'Length) := poll.event_info_poll.all;
        tmp_spa (1 .. poll.event_info_poll.all'Length) := poll.result_poll.all;
        poll.event_info_poll  := new kernel_event_info_array'(tmp_epa);
        poll.result_poll      := new kernel_event_array'(tmp_spa);
      end b1;
    end if;

    b2 :
    declare
      mi_kevent : kernel_event :=
        (ident  => tmp_sock,
         filter => event_bitmap,
         flags  => kpoll_flag_add,
         fflags => 0, data  => 0, udata => Null_Address, ext => (others => 0));

    begin

      if inner_mi_set_kevent (poll.handle, mi_kevent'Address, 1) /= -1 then
        poll.event_info_poll.all (poll.count + 1) :=
          (ident => tmp_sock, filter => event_bitmap, failed => False);

        poll.count := poll.count + 1;
        return True;
      end if;

      return False;
    end b2;
  end add;

  procedure reset_results
    (poll   : aliased in out poll_of_events)
  is
  begin
    poll.last_wait_returned := 0;
    poll.result_poll.all := (others => <>);
  end reset_results;


  function is_send
    (poll  : aliased in poll_of_events;
     sock  : socket) return Boolean
  is
    mi_socket : constant adr_uintptr_t := adr_uintptr_t (socket_type'(get_socket (sock)));
  begin
    if poll.last_wait_returned < 1 then
      return False;
    end if;

    return (for some E of
            poll.result_poll.all (poll.result_poll.all'First .. (poll.result_poll.all'First + poll.last_wait_returned) - 1)
        => E.ident = mi_socket and then E.filter = send_event);

  end is_send;

  function is_receive
    (poll  : aliased in poll_of_events;
     sock  : socket) return Boolean
  is
    mi_socket : constant adr_uintptr_t := adr_uintptr_t (socket_type'(get_socket (sock)));
  begin
    if poll.last_wait_returned < 1 then
      return False;
    end if;

    return (for some E of
            poll.result_poll.all (poll.result_poll.all'First .. (poll.result_poll.all'First + poll.last_wait_returned) - 1)
        => E.ident = mi_socket and then E.filter = receive_event);

  end is_receive;

  function init
    (poll     : aliased in out poll_of_events;
     min_qtie : int := 2
    ) return Boolean
  is
    tmp_kpoll_handle : constant handle_type := inner_kqueue;
  begin
    if tmp_kpoll_handle = failed_handle then
      return False;
    end if;

    poll := (handle => tmp_kpoll_handle,
             event_info_poll  => new kernel_event_info_array'(1 .. min_qtie => <>),
             result_poll  => new kernel_event_array'(1 .. min_qtie => <>),
             initialized  => True,
             count  => 0, last_wait_returned => 0);

    return True;
  end init;

  procedure close
    (poll     : aliased in out poll_of_events)
  is
    tmp_res : int := 0
      with Unreferenced;

  begin
      tmp_res := inner_close (poll.handle);

      poll.initialized  :=  False;
      poll.count        :=  0;
      poll.event_info_poll  :=  null;
      poll.result_poll  :=  null;
      poll.handle       :=  failed_handle;
      poll.last_wait_returned :=  0;

  end close;

end adare_net.base.waits;
