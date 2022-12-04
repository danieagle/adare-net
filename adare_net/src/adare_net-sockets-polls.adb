with adare_net.sockets.inners;

package body adare_net.sockets.polls
  with Preelaborate
is

  function is_full (mi_poll : not null access poll_type) return Boolean
  is (mi_poll.count >= mi_poll.Len and then
      not (for some E of mi_poll.pos (1 .. mi_poll.count) => E = 0));

  function is_empty
    (mi_poll : not null access poll_type) return Boolean
  is (mi_poll.count <= 0);

  function is_in
    (in_poll    : not null access poll_type;
     what       : not null access socket
     ) return Boolean
  is (for some E of in_poll.pos (1 .. in_poll.count) =>
      E = get_sock (what.all));

  procedure add_events
    (to_poll    : not null access poll_type;
     sock       : not null access socket;
     with_events_bitmap : in event_type
    )
  is
    sock_tmp    : constant socket_type := get_sock (sock.all);
    pollfd_tmp  : constant pollfd :=
      (fd => sock_tmp, events => unsigned_short (with_events_bitmap),
       revents => 0);

    index : Unsigned_8 := 0;
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

    if to_poll.pos (index) /= 0 then
      to_poll.count := to_poll.count + 1;
    end if;

    to_poll.pos (to_poll.count) := sock_tmp;
    to_poll.poll (to_poll.count) := pollfd_tmp;
  end add_events;

  procedure clear_all_event_responses
    (to_poll    : not null access poll_type)
  is
  begin
    loop1 :
    for E of to_poll.poll (1 .. to_poll.count) loop
      E.revents := 0;
    end loop loop1;
  end clear_all_event_responses;

  procedure remove
    (from_poll   : not null access poll_type;
     what         : not null access socket
    )
  is
    sock_tmp    : constant socket_type := get_sock (what.all);

    index1  : constant Unsigned_8 := from_poll.count;
    index   : Unsigned_8 := index1;
  begin

    loop1 :
    for E : socket_type of reverse from_poll.pos (1 .. index1) loop
      exit loop1 when E = sock_tmp;

      index :=  index - 1;
      if E = 0 then
        from_poll.count := from_poll.count - 1;
      end if;
    end loop loop1;

    if from_poll.pos (index)  = sock_tmp then
      from_poll.pos (index)   := 0;
      from_poll.poll (index)  := null_pollfd;
    end if;
  end remove;

  procedure update
    (from_poll    : not null access poll_type;
     for_sock     : not null access socket;
     with_events_bitmap : in event_type
    )
  is
    sock_tmp    : constant socket_type := get_sock (for_sock.all);
    index : Unsigned_8 := 0;
  begin

    loop1 :
    for E of from_poll.pos (1 .. from_poll.count) loop
      index :=  index + 1;
      exit loop1 when E = sock_tmp;
    end loop loop1;

    if from_poll.pos (index) = sock_tmp then
      from_poll.poll (index).events := unsigned_short (with_events_bitmap);
    end if;
  end update;

  procedure reset_all (who  : not null access poll_type)
  is
  begin
    who.pos   := (others => 0);
    who.poll  := (others => null_pollfd);
    who.count := 0;
  end reset_all;

  function start_events_listen
    (from_poll : not null access poll_type;
     time_out   : int -- time_out < 0 => forever wait
                      -- time_out = 0 => no wait
                      -- time_out > 0 => miliseconds time_out wait
    ) return number_of_hits
  is
  begin

    return  inners.inner_poll (from_poll.poll (1)'Address,
              int (from_poll.count), time_out);
  end start_events_listen;

  function receive_event
    (from_poll    : not null access poll_type;
     for_sock     : not null access socket
    ) return Boolean
  is
    sock_tmp    : constant socket_type := get_sock (for_sock.all);
    index : Unsigned_8 := 0;
  begin

    loop1 :
    for E of from_poll.pos (1 .. from_poll.count) loop
      index :=  index + 1;
      exit loop1 when E = sock_tmp;
    end loop loop1;

    return
      (from_poll.poll (index).revents and unsigned_short (receive_ev)) > 0;
  end receive_event;

  function accept_event
    (from_poll    : not null access poll_type;
     for_sock     : not null access socket
    ) return Boolean
  is (receive_event (from_poll, for_sock));

  function send_event
    (from_poll    : not null access poll_type;
     for_sock     : not null access socket
    ) return Boolean
  is
    sock_tmp    : constant socket_type := get_sock (for_sock.all);
    index : Unsigned_8 := 0;
  begin

    loop1 :
    for E of from_poll.pos (1 .. from_poll.count) loop
      index :=  index + 1;
      exit loop1 when E = sock_tmp;
    end loop loop1;

    return (from_poll.poll (index).revents and unsigned_short (send_ev)) > 0;
  end send_event;


  function poll_error
    (from_poll    : not null access poll_type;
     for_sock     : not null access socket
    ) return Boolean
  is
    sock_tmp    : constant socket_type := get_sock (for_sock.all);
    index : Unsigned_8 := 0;
  begin

    loop1 :
    for E of from_poll.pos (1 .. from_poll.count) loop
      index :=  index + 1;
      exit loop1 when E = sock_tmp;
    end loop loop1;

    return (from_poll.poll (index).revents and unsigned_short (poll_err)) > 0;
  end poll_error;

  function hang_up_error
    (from_poll    : not null access poll_type;
     for_sock     : not null access socket
    ) return Boolean
  is
    sock_tmp    : constant socket_type := get_sock (for_sock.all);
    index : Unsigned_8 := 0;
  begin

    loop1 :
    for E of from_poll.pos (1 .. from_poll.count) loop
      index :=  index + 1;
      exit loop1 when E = sock_tmp;
    end loop loop1;

    return
      (from_poll.poll (index).revents and unsigned_short (hang_up_err)) > 0;
  end hang_up_error;

  function socket_descritor_error
    (from_poll    : not null access poll_type;
     for_sock     : not null access socket
    ) return Boolean
  is
    sock_tmp    : constant socket_type := get_sock (for_sock.all);
    index : Unsigned_8 := 0;
  begin

    loop1 :
    for E of from_poll.pos (1 .. from_poll.count) loop
      index :=  index + 1;
      exit loop1 when E = sock_tmp;
    end loop loop1;

    return (from_poll.poll (index).revents and
            unsigned_short (socket_descritor_err)) > 0;
  end socket_descritor_error;


end adare_net.sockets.polls;
