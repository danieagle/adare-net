
  separate (adare_net.base)
  function connect
    (sock : aliased in out socket) return Boolean
  is
  begin
    if sock.binded or else sock.connected or else sock.listened then
      return False;
    end if;

    if a_type_label (sock.storage.socktype) = udp then
      sock.connected := True;

      return True;
    end if;

    if inner_connect (sock.sock, sock.storage.storage'Address, sock.storage.addr_length) /= 0 then
      return False;
    end if;

    sock.connected := True;

    return True;
  end connect;
