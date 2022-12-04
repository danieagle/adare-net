
with System;
use System;

with adare_net_exceptions;
use adare_net_exceptions;

with adare_net.sockets.inners;

package body adare_net.sockets
  with Preelaborate
is

  overriding
  procedure Read
    (Stream : in out socket_buffer;
     Item   : out Stream_Element_Array;
     Last   : out Stream_Element_Offset)
  is
  begin

    if Stream.data = null
      or else Stream.tail <  Stream.data'First
      or else Stream.head < Stream.data'First
    then
      raise buffer_empty_error;
    end if;

    if Item'Length = 0 then
      b1 :
      declare
        mi_empty  : constant Stream_Element_Array (1 .. 0) := (others => 0);
      begin
        Item := mi_empty;
        Last  := Item'Last;
        return;
      end b1;
    end if;

    if Item'Length > (Stream.tail - Stream.head) + 1 then
        raise buffer_insufficient_space_error with " Insufficient amount " &
          "of data left in buffer to fill the request. ";
    end if;

    Item  :=  Stream.data (Stream.head .. (Stream.head - 1) + Item'Length);
    Last  := Item'Last;
    Stream.head :=  Stream.head + Item'Length;
  end Read;

  overriding
  procedure Write
    (Stream : in out socket_buffer;
     Item   : in Stream_Element_Array)
  is
    head_idx : constant Stream_Element_Count := Stream.head;
    tail_idx : constant Stream_Element_Count := Stream.tail;
    item_len : constant Stream_Element_Count := Item'Length;
    data_last   : Stream_Element_Count;
    data_first  : Stream_Element_Count;
  begin
    if item_len = 0 then
      return;
    end if;

    if Stream.data = null then

      Stream.data := new Stream_Element_Array'(1 .. Stream_Element_Count'Max (40, item_len + 40) => 0);

    end if;

    data_first  := Stream.data.all'First;
    data_last   := Stream.data.all'Last;

    if item_len > (data_last - tail_idx) + 1 then

      if item_len <= (head_idx - data_first) + (data_last - tail_idx) then

        Stream.data (1 .. (tail_idx - head_idx) + 1) := Stream.data (head_idx .. tail_idx);
        Stream.data ((tail_idx - head_idx) + 2 .. data_last) := (others => 0);
        Stream.head := 1;
        Stream.tail := (tail_idx - head_idx) + 1;

      else

        b1 :
        declare
          data_tmp : constant Stream_Element_Array (1 .. (tail_idx - head_idx) + 1) := Stream.data (head_idx .. tail_idx);
        begin

          Stream.data := new Stream_Element_Array'(1 .. (data_tmp'Length + item_len + 40) => 0);
          Stream.data (data_tmp'Range) := data_tmp;
          Stream.head := 1;
          Stream.tail := data_tmp'Last;

        end b1;
      end if;

    end if;

    data_first  := Stream.data.all'First;

    if Stream.head < data_first then
      Stream.head := data_first;
    end if;

    Stream.data (Stream.tail + 1 .. Stream.tail + item_len)  :=  Item;
    Stream.tail :=  Stream.tail + item_len;
  end Write;

  function init_addresses
    (ip_or_host : String;
     port       : String;
     ai_socktype  : Address_type;
     ai_family    : Address_family
    ) return addresses_list
  is
      list_length : aliased int := inners.a_list'Length - 1;
      list  : aliased inners.a_list;

      i_or_o  : char_array  :=  To_C (ip_or_host);
      pc      : char_array  :=  To_C (port);
  begin

      inners.inner_init_address (i_or_o'Address, pc'Address, int (ai_socktype), int (ai_family), list_length, list);

      return list (1 .. Integer (list_length));

  end init_addresses;

  function init_addresses
    (ip_or_host : String;
     port       : String;
     ai_socktype  : Address_type;
     ai_family    : Address_family
    ) return addresses
  is
      list_length : aliased int := 2;
      list  : aliased inners.a_list;

      i_or_o  : char_array  :=  To_C (ip_or_host);
      pc      : char_array  :=  To_C (port);

  begin

      inners.inner_init_address (i_or_o'Address, pc'Address, int (ai_socktype), int (ai_family), list_length, list);

      return list (1);
  end init_addresses;

  procedure reuse_address
    (sock  : in out socket) is
  begin
    inners.inner_reuse_address (sock.sock);
  end reuse_address;

  function get_addresses
    (show  : not null access addresses) return String
  is
  begin
    return get_addresses (show.all);
  end get_addresses;

  function get_addresses
    (show  : addresses) return String
  is
    ai_family  : constant  Address_family := Address_family (show.storage.ss_family);

    dest : char_array :=
      (1 .. size_t (if ai_family = v4  then v4_str_length elsif ai_family = v6  then v6_str_length else 0) => char'Val (0));

    dest_length : size_t := dest'Length;

  begin
    if dest_length = 0 then
      return "unknown";
    end if;

    if ai_family = v6 then
      b1 :
      declare
        ip6_tmp : sockaddr_in6
          with Import, Convention => Ada;

        for ip6_tmp'Address use show.storage'Address;
      begin
        inners.inner_inet_ntop (int (ip6_tmp.sin6_family), ip6_tmp.sin6_addr.s6_addr'Address, dest'Address, socklen_t (dest'Length));
      end b1;
    else
      b2 :
      declare
        ip4_tmp : sockaddr_in
          with Import, Convention => Ada;

        for ip4_tmp'Address use show.storage'Address;
      begin
        inners.inner_inet_ntop (int (ip4_tmp.sin_family), ip4_tmp.sin_addr.s_addr'Address, dest'Address,  socklen_t (dest'Length));
      end b2;
    end if;

    loop1 :
    for E of reverse dest loop
      exit loop1 when E /= char'Val (0);
      dest_length := dest_length - 1;
    end loop loop1;

    return To_Ada (dest (1 .. dest_length), False);
  end get_addresses;

  function get_port
    (show  : not null access addresses) return String
  is
  begin
    return get_port (show.all);
  end get_port;

  function get_port
    (show  : in addresses) return String
  is
    ai_family  : constant  Address_family := Address_family (show.storage.ss_family);

  begin
    if not (ai_family = v6 or else ai_family = v4) then
      return "unknown";
    end if;

    if ai_family = v6 then
      b1 :
      declare
        ip6_tmp : sockaddr_in6
          with Import, Convention => Ada;

        for ip6_tmp'Address use show.storage'Address;
        ai_port   : constant String := inners.inner_ntohs (ip6_tmp.sin6_port)'Image;
      begin
        return ai_port (ai_port'First + 1 .. ai_port'Last);
      end b1;
    end if;

    if ai_family = v4 then
      b2 :
      declare
        ip4_tmp : sockaddr_in
          with Import, Convention => Ada;

        for ip4_tmp'Address use show.storage'Address;
        ai_port   : constant String := inners.inner_ntohs (ip4_tmp.sin_port)'Image;
      begin
        return ai_port (ai_port'First + 1 .. ai_port'Last);
      end b2;
    end if;

    return "unknown"; -- to satisfy compiler :-)
  end get_port;

  function get_address_and_port
    (show  : not null access addresses) return String
  is
  begin
    return  get_address_and_port (show.all);
  end get_address_and_port;

  function get_address_and_port
    (show  : in addresses) return String
  is
  begin
    return  " address := " & get_addresses (show) & "  port := " & get_port (show);
  end get_address_and_port;

  function get_address_family
    (show  : not null access addresses) return Address_family
  is (Address_family (show.storage.ss_family));

  function get_address_family
    (show  : in addresses) return Address_family
  is (Address_family (show.storage.ss_family));

  function is_null
    (addr  : not null access addresses) return Boolean
  is (null_addresses = addr.all);

  function is_null
    (addr  : in addresses) return Boolean
  is (null_addresses = addr);


  function init_socket
    (sock : in out socket;
     addr : not null access addresses) return Boolean
  is
    sockfd    : aliased socket_type  := 0;
    sock_tmp  : socket;

  begin
    clean (sock);

    sock_tmp.storage  := addr.all;
    sockfd  :=  inners.inner_socket (int (sock_tmp.storage.storage.ss_family), sock_tmp.storage.socktype, sock_tmp.storage.protocol);

    if sockfd = invalid_socket then
      return False;
    end if;

    sock_tmp.sock   := sockfd;
    sock  := sock_tmp;

    return True;
  end init_socket;

  function init_socket
    (sock   : in out socket;
     addr   : not null access addresses_list) return Boolean
  is
    sockfd    : aliased socket_type  := 0;
    sock_tmp  : socket;
    ok        : Boolean := False;
  begin
    clean (sock);

    loop1 :
    for addr_tmp of addr.all loop
      sock_tmp.storage  := addr_tmp;
      sockfd  :=
        inners.inner_socket (int (sock_tmp.storage.storage.ss_family), sock_tmp.storage.socktype, sock_tmp.storage.protocol);

      if sockfd /= invalid_socket then
        ok := True;
        exit loop1;
      end if;

    end loop loop1;

    if not ok then
      return False;
    end if;

    sock_tmp.sock   := sockfd;
    sock  := sock_tmp;

    return True;
  end init_socket;

  function bind
    (sock  : in out socket) return Boolean
  is
  begin
      if inners.inner_bind (sock.sock, sock.storage.storage'Address, sock.storage.address_length) /= 0 then
        return False;
      end if;

      sock.binded := True;

      return True;
  end bind;

  function listen
    (sock     : in out socket;
     backlog  : int) return Boolean
  is
  begin
      if inners.inner_listen (sock.sock, backlog) /= 0  then
        return False;
      end if;

      sock.listened := True;

      return True;
  end listen;

  function accept_socket
    (sock     : not null access socket;
     new_sock : in out socket) return Boolean
  is
    sock_tmp  : socket  := sock.all;

    len       : socklen_t := sock_tmp.storage.storage'Size / 8;
  begin

    sock_tmp.sock := inners.inner_accept (sock.sock, sock_tmp.storage.storage'Address, len);

    if sock_tmp.sock = invalid_socket  then
      return False;
    end if;

    sock_tmp.storage.address_length :=  int (len);

    sock_tmp.connected  :=  True;
    sock_tmp.binded     :=  False;
    sock_tmp.listened   :=  False;

    new_sock  :=  sock_tmp;

    return True;
  end accept_socket;

  function connect
    (sock  : in out socket) return Boolean
  is
  begin
    if inners.inner_connect (sock.sock, sock.storage.storage'Address, size_t (sock.storage.address_length)) /= 0 then
      return False;
    end if;

    sock.connected := True;

    return True;
  end connect;

  procedure close
    (sock  : in out socket)
  is
    sockfd : constant int := inners.inner_close (sock.sock)
      with unreferenced;

  begin
    clean (sock);
  end close;

  procedure clean
    (sock  : in out socket)
  is
  begin
    sock.sock    := 0;

    sock.storage.storage.ss_family  := 0;
    sock.storage.storage.padding    := (others => char'Val (0));

    sock.storage.socktype  := 0;
    sock.storage.protocol  := 0;

    sock.storage.address_length := 0;

    sock.connected :=  False;
    sock.binded    :=  False;
    sock.listened  :=  False;
  end clean;

  function send
    (sock     : socket;
     buffer   : in Stream_Element_Array
    ) return ssize_t
  is
      len         : ssize_t  := 0;
      pos         : Stream_Element_Offset  := buffer'First;
      remaining   : ssize_t  := buffer'Length;
  begin
    if remaining = 0 then
      return 0;
    end if;

    loop1 :
    loop
      len := inners.inner_send (sock.sock, buffer (pos)'Address, size_t (remaining), 0);

      exit loop1 when len < 1;

      pos := pos + Stream_Element_Offset (len);

      exit loop1 when remaining = len;

      remaining :=  remaining - len;

      exit loop1 when remaining < 1;
    end loop loop1;

    return ssize_t (abs (pos - buffer'First));
  end send;

  function send
    (sock     : socket;
     buffer   : in out socket_buffer
    ) return ssize_t
  is
    sended_size : ssize_t := 0;
  begin

    if is_empty (buffer) then
      return 0;
    end if;

    sended_size := send (sock, buffer.data (buffer.head .. buffer.tail));

    buffer.head := buffer.head + Stream_Element_Count (sended_size);

    return sended_size;
  end send;

  function sendto
    (sock     : socket;
     send_to  : addresses;
     buffer   : in Stream_Element_Array) return ssize_t
  is
    len         : ssize_t  := 0;
    pos         : Stream_Element_Offset  := buffer'First;
    remaining   : ssize_t  := buffer'Length;
  begin
    if remaining = 0 then
      return 0;
    end if;

    loop1 :
    loop
      len :=  inners.inner_sendto (sock.sock, buffer (pos)'Address, size_t (remaining), 0, send_to.storage'Address,
        socklen_t (send_to.address_length));

      exit loop1 when len < 1;

      pos := pos + Stream_Element_Offset (len);

      exit loop1 when remaining = len;

      remaining :=  remaining - len;

      exit loop1 when remaining < 1;
    end loop loop1;

    return ssize_t (abs (pos - buffer'First));

  end sendto;

  function sendto
    (sock     : socket;
     send_to  : addresses;
     buffer   : in out socket_buffer) return ssize_t
  is
    sended_size : ssize_t := 0;
  begin
    if is_empty (buffer) then
      return 0;
    end if;

    sended_size := sendto (sock, send_to, buffer.data (buffer.head .. buffer.tail));

    buffer.head := buffer.head + Stream_Element_Count (sended_size);

    return sended_size;
  end sendto;

  function receive
    (sock     : socket;
     buffer   : in out Stream_Element_Array
    ) return ssize_t
  is
  begin
    if buffer'Length = 0 then
      return 0;
    end if;

    return inners.inner_recv (sock.sock, buffer (buffer'First)'Address, size_t (buffer'Length), 0);

  end receive;

  function receive
    (sock     : socket;
     buffer   : in out socket_buffer
    ) return ssize_t
  is
    received_size : constant ssize_t := receive (sock, buffer.data (buffer.tail + 1 .. buffer.data'Last));
  begin

    if received_size > 0 then
      buffer.tail := buffer.tail + Stream_Element_Count (received_size);

      if buffer.head < buffer.data'First then
        buffer.head := buffer.data'First;
      end if;
    end if;

    return received_size;
  end receive;

  function receive_from
    (sock     : socket;
     buffer   : in out Stream_Element_Array;
     from     : out addresses) return ssize_t
  is
  begin
    if buffer'Length = 0 then
      return 0;
    end if;

    b1 :
    declare
      from_tmp      : aliased addresses := sock.storage;
      received_tmp  : ssize_t :=  0;

      len_tmp : aliased socklen_t := socklen_t (from_tmp.storage'Size / 8);

    begin
      from_tmp.storage.ss_family  := 0;
      from_tmp.storage.padding    := (others => char'Val (0));

      received_tmp  :=  inners.inner_recvfrom (sock.sock, buffer (buffer'First)'Address, size_t (buffer'Length), 0,
        from_tmp.storage'Address, len_tmp);

      from_tmp.address_length := int (len_tmp);

      from  :=  from_tmp;

      return received_tmp;

    end b1;
  end receive_from;

  function receive_from
    (sock     : socket;
     buffer   : in out socket_buffer;
     from     : out addresses) return ssize_t
  is
    received_size : constant ssize_t := receive_from (sock, buffer.data (buffer.tail + 1 .. buffer.data'Last), from);
  begin
    if received_size > 0 then
      buffer.tail := buffer.tail + Stream_Element_Count (received_size);

      if buffer.head < buffer.data'First then
        buffer.head := buffer.data'First;
      end if;
    end if;

    return received_size;
  end receive_from;


  function get_sock
    (sock : in socket) return socket_type
  is (sock.sock);

  function get_addresses
    (sock : in socket) return addresses
  is (sock.storage);


  function initialized
    (sock  : socket) return Boolean
  is (sock.sock /= 0 and then sock.sock /= invalid_socket);

  function initialized
    (sock  :  not null access socket) return Boolean
  is (sock.sock /= 0 and then sock.sock /= invalid_socket);


  function connected
    (sock  : socket) return Boolean
  is (sock.connected);

  function connected
    (sock  : not null access socket) return Boolean
  is (sock.connected);


  function binded
    (sock  : socket) return Boolean
  is (sock.binded);

  function binded
    (sock  : not null access socket) return Boolean
  is (sock.binded);


  function listened
    (sock  : socket) return Boolean
  is (sock.listened);

  function listened
    (sock  : not null access socket) return Boolean
  is (sock.listened);


  function is_empty
    (buffer : socket_buffer) return Boolean
  is (buffer.tail < buffer.data'First or else buffer.head < buffer.data'First);

  function is_empty
    (buffer : not null access socket_buffer) return Boolean
  is (buffer.tail < buffer.data'First or else buffer.head < buffer.data'First);


  function actual_data_size
    (buffer : not null access socket_buffer) return Integer_64
  is (Integer_64 ((Stream_Element_Offset (buffer.tail) - Stream_Element_Offset (buffer.head)) + 1));

  function actual_data_size
    (buffer : in socket_buffer) return Integer_64
  is (Integer_64 ((Stream_Element_Offset (buffer.tail) - Stream_Element_Offset (buffer.head)) + 1));


  function max_data_length
    (buffer : not null access socket_buffer) return Integer_64
  is (buffer.data'Length);

  function max_data_length
    (buffer : in socket_buffer) return Integer_64
  is (buffer.data'Length);


  function max_data_length
    (buffer : not null access socket_buffer) return Stream_Element_Offset
  is (buffer.data'Length);

  function max_data_length
    (buffer : in socket_buffer) return Stream_Element_Offset
  is (buffer.data'Length);


  function get_buffer_init
    (buffer : not null access socket_buffer)
    return socket_buffer
  is
  begin
    return
      socket_buffer'(
        Root_Stream_Type with
        data  => new Stream_Element_Array'(buffer.data.all),
        head  => buffer.head,
        tail  => buffer.tail
      );
  end get_buffer_init;


  procedure clean
    (buffer : in out socket_buffer)
  is
  begin
    buffer.head := 0;
    buffer.tail := 0;
    buffer.data := null;
  end clean;

  procedure clean
    (buffer : not null access socket_buffer)
  is
  begin
    buffer.head := 0;
    buffer.tail := 0;
    buffer.data := null;
  end clean;


  function add_raw
    (buffer : in out socket_buffer;
     raw    : in Stream_Element_Array) return Boolean
  is
  begin
    if raw'Length = 0 then
      return True;
    end if;

    Stream_Element_Array'Write (buffer'Access, raw);

    return True;
  end add_raw;

  function add_raw
    (buffer : not null access socket_buffer;
     raw    : in Stream_Element_Array) return Boolean
  is
  begin
    if raw'Length = 0 then
      return True;
    end if;

    Stream_Element_Array'Write (buffer.all'Access, raw);

    return True;
  end add_raw;


  function get_raw
    (buffer : in socket_buffer) return Stream_Element_Array
  is
  begin
    return buffer.data (buffer.head .. buffer.tail);
  end get_raw;


  function get_raw
    (buffer : not null access socket_buffer) return Stream_Element_Array
  is
  begin
    return buffer.data (buffer.head .. buffer.tail);
  end get_raw;

  function string_error return String is
    message_a : aliased char_array (1 .. 260) := (others => char'Val (0));
    length_a  : aliased int :=  int (message_a'Last) - 1;
  begin
    inners.inner_show_error (message_a, length_a);

    return To_Ada (
      message_a (message_a'First .. message_a'First + size_t (length_a)));
  end string_error;

end adare_net.sockets;
