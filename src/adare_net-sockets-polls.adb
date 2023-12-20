with adare_net.sockets.inners;

package body adare_net.sockets.polls
  with Preelaborate
is
  use  adare_net.sockets.inners;

  function "And" (left, right : Interfaces.C.short)
    return Interfaces.C.short
  is (inner_and (left, right));

  function "Or" (left, right : Interfaces.C.short)
    return Interfaces.C.short
  is (inner_or (left, right));

  function is_full (mi_poll : not null access poll_type) return Boolean
  is (mi_poll.count >= mi_poll.Len and then
      not (for some E of mi_poll.pos (1 .. mi_poll.count) => E = 0));

  function is_empty
    (mi_poll : not null access poll_type) return Boolean
  is (mi_poll.count < 1);

  function is_in
    (in_poll    : not null access poll_type;
     what       : not null socket_access
     ) return Boolean
  is (in_poll.count > 0 and then
    (for some E of in_poll.pos (1 .. in_poll.count) => E = get_sock (what)));

  procedure add_events
    (to_poll    : not null access poll_type;
     sock       : not null socket_access;
     with_events_bitmap : in event_type
    )
  is
    pollfd_tmp  : constant pollfd
      := (fd => get_sock (sock), events => with_events_bitmap, revents => 0);

    sock_tmp : constant socket_type := get_sock (sock);

    index : int := 0;
  begin

    if to_poll.count < 1 then
      to_poll.count     := 1;
      to_poll.pos (1)   := sock_tmp;
      to_poll.poll (1)  := pollfd_tmp;
      return;
    end if;

    loop1 :
    for E of to_poll.pos (1 .. to_poll.count) loop
      index :=  index + 1;
      exit loop1 when E = 0;
    end loop loop1;

    if to_poll.pos (index) = 0 then
        to_poll.pos (index)  := sock_tmp;
        to_poll.poll (index) := pollfd_tmp;
        return;
    end if;

    to_poll.count                := to_poll.count + 1;
    to_poll.pos (to_poll.count)  := sock_tmp;
    to_poll.poll (to_poll.count) := pollfd_tmp;
  end add_events;

  procedure clear_all_event_responses
    (to_poll    : not null access poll_type)
  is
  begin
    if to_poll.count < 1 then
      return;
    end if;

    loop1 :
    for E of to_poll.poll (1 .. to_poll.count) loop
      E.revents := 0;
    end loop loop1;
  end clear_all_event_responses;

  procedure remove
    (from_poll   : not null access poll_type;
     what         : not null socket_access
    )
  is
    sock_tmp    : constant socket_type := get_sock (what);

    index : int := 0;
  begin
    loop0 :
    for E : socket_type of reverse from_poll.pos (1 .. from_poll.count) loop

      exit loop0 when E /= 0;
      from_poll.count := from_poll.count - 1;

    end loop loop0;

    index := from_poll.count + 1;

    loop1 :
    for E : socket_type of reverse from_poll.pos (1 .. from_poll.count) loop
      index :=  index - 1;
      exit loop1 when E = sock_tmp;
    end loop loop1;

    if from_poll.pos (index)  = sock_tmp then
      from_poll.pos (index)   := 0;
      from_poll.poll (index)  := null_pollfd;
    end if;

    if index = from_poll.count then
      from_poll.count := from_poll.count - 1;
    end if;

  end remove;

  procedure update
    (from_poll    : not null access poll_type;
     for_sock     : not null socket_access;
     with_events_bitmap : in event_type
    )
  is
    sock_tmp  : constant socket_type := get_sock (for_sock);
    index     : int := 0;
  begin

    loop1 :
    for E of from_poll.pos (1 .. from_poll.count) loop
      index :=  index + 1;
      exit loop1 when E = sock_tmp;
    end loop loop1;

    if from_poll.pos (index) = sock_tmp then
      from_poll.poll (index).events := with_events_bitmap;
    end if;
  end update;

  procedure reset_all (who  : not null access poll_type)
  is
  begin
    who.count := 0;
    who.poll  := (others => null_pollfd);
    who.pos   := (others => 0);
  end reset_all;

  function start_events_listen
    (from_poll : not null access poll_type;
     time_out   : int := 15000
                      -- time_out < 0 => forever wait
                      -- time_out = 0 => no wait
                      -- time_out > 0 => miliseconds time_out wait
    ) return number_of_hits
  is
      mi_len : constant unsigned_long := unsigned_long (from_poll.Len)
        with Convention => C, Size => unsigned_long'Size;
      mi_int  : constant int :=
        inner_poll (from_poll.poll'Address, mi_len'Address, time_out'Address);
  begin

    return  number_of_hits (mi_int);
  end start_events_listen;

  function receive_event
    (from_poll    : not null access poll_type;
     for_sock     : not null socket_access
    ) return Boolean
  is
    sock_tmp    : constant socket_type := get_sock (for_sock);
    index : int := 0;
  begin

    loop1 :
    for E of from_poll.pos (1 .. from_poll.count) loop
      index :=  index + 1;
      exit loop1 when E = sock_tmp;
    end loop loop1;

    return from_poll.pos (index) = sock_tmp and then
      ((from_poll.poll (index).revents and Interfaces.C.short (receive_ev)) > 0);
  end receive_event;

  function accept_event
    (from_poll    : not null access poll_type;
     for_sock     : not null socket_access
    ) return Boolean
  is
    sock_tmp    : constant socket_type := get_sock (for_sock);
    index : int := 0;
  begin

    loop1 :
    for E of from_poll.pos (1 .. from_poll.count) loop
      index :=  index + 1;
      exit loop1 when E = sock_tmp;
    end loop loop1;

    return from_poll.pos (index) = sock_tmp and then
      ((from_poll.poll (index).revents and Interfaces.C.short (accept_ev)) > 0);
  end accept_event;

  function send_event
    (from_poll    : not null access poll_type;
     for_sock     : not null socket_access
    ) return Boolean
  is
    sock_tmp    : constant socket_type := get_sock (for_sock);
    index : int := 0;
  begin

    loop1 :
    for E of from_poll.pos (1 .. from_poll.count) loop
      index :=  index + 1;
      exit loop1 when E = sock_tmp;
    end loop loop1;

    return from_poll.pos (index) = sock_tmp and then
      ((from_poll.poll (index).revents and Interfaces.C.short (send_ev)) > 0);
  end send_event;


  function poll_error
    (from_poll    : not null access poll_type;
     for_sock     : not null socket_access
    ) return Boolean
  is
    sock_tmp    : constant socket_type := get_sock (for_sock);
    index : int := 0;
  begin

    loop1 :
    for E of from_poll.pos (1 .. from_poll.count) loop
      index :=  index + 1;
      exit loop1 when E = sock_tmp;
    end loop loop1;

    return from_poll.pos (index) = sock_tmp and then
      ((from_poll.poll (index).revents and  Interfaces.C.short (poll_err)) > 0);
  end poll_error;

  function hang_up_error
    (from_poll    : not null access poll_type;
     for_sock     : not null socket_access
    ) return Boolean
  is
    sock_tmp    : constant socket_type := get_sock (for_sock);
    index : int := 0;
  begin

    loop1 :
    for E of from_poll.pos (1 .. from_poll.count) loop
      index :=  index + 1;
      exit loop1 when E = sock_tmp;
    end loop loop1;

    return from_poll.pos (index) = sock_tmp and then
      ((from_poll.poll (index).revents and Interfaces.C.short (hang_up_err)) > 0);
  end hang_up_error;

  function socket_descritor_error
    (from_poll    : not null access poll_type;
     for_sock     : not null socket_access
    ) return Boolean
  is
    sock_tmp    : constant socket_type := get_sock (for_sock);
    index : int := 0;
  begin

    loop1 :
    for E of from_poll.pos (1 .. from_poll.count) loop
      index :=  index + 1;
      exit loop1 when E = sock_tmp;
    end loop loop1;

    return from_poll.pos (index) = sock_tmp and then
      ((from_poll.poll (index).revents and Interfaces.C.short (socket_descritor_err)) > 0);
  end socket_descritor_error;


end adare_net.sockets.polls;
