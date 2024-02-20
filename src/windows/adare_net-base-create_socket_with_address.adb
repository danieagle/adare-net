
  separate (adare_net.base)

  function create_socket_with_address
    (sock_address : aliased socket_address;
     response     : out socket;
     bind_socket  : Boolean := False;
     listen_socket  : Boolean := False;
     backlog        : Unsigned_16 := 10) return Boolean
  is
    mi_response   : aliased socket  := null_socket;

    mi_socket_fd  : socket_type;
    proto : constant Address_type_label := a_type_label (sock_address.socktype);

    acc           : Interfaces.C.int := 0
      with Unreferenced;
  begin

    response  :=  null_socket;

    mi_response.storage  := sock_address;

    mi_socket_fd :=
      inner_socket (int (mi_response.storage.storage.ss_family),
        Interfaces.C.int (mi_response.storage.socktype),
        Interfaces.C.int (mi_response.storage.protocol));

    if mi_socket_fd = invalid_socket then
      return False;
    end if;

    mi_response.sock := mi_socket_fd;

    if bind_socket then
      reuse_address (mi_response);

      if inner_bind (mi_response.sock, mi_response.storage.storage'Address,
        Interfaces.C.int (mi_response.storage.addr_length)) /= 0
      then
        acc := inner_close (mi_response.sock);
        return False;
      end if;

      mi_response.binded := True;
    end if;

    if listen_socket and then proto = tcp then

      if inner_listen (mi_response.sock, int (backlog)) /= 0  then
        acc := inner_close (mi_response.sock);
        return False;
      end if;

      mi_response.listened := True;
    end if;

    if listen_socket and then proto = udp then
      mi_response.listened := True;
    end if;

    response := mi_response;

    return True;
  end create_socket_with_address;
