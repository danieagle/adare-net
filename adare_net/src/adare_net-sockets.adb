
with adare_net.sockets.inners;
with adare_net_exceptions;

use adare_net_exceptions;

package body adare_net.sockets
  with Preelaborate
is

  function init_addresses
    (ip_or_host : String;
     port       : String;
     ai_socktype  : Address_type;
     ai_family    : Address_family
    ) return addresses_list
  is
      list_length : aliased int := inners.a_list'Length - 1;
      list  : aliased inners.a_list;

      i_or_o  : constant char_array  :=  To_C (ip_or_host);
      pc      : constant char_array  :=  To_C (port);
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
      list        : aliased inners.a_list;

      i_or_o  : constant char_array  :=  To_C (ip_or_host);
      pc      : constant char_array  :=  To_C (port);
  begin
      inners.inner_init_address (i_or_o'Address, pc'Address, int (ai_socktype), int (ai_family), list_length, list);

      return list (1);
  end init_addresses;


  function get_addresses
    (show  : not null addresses_access) return String
  is
    ai_family  : constant  Address_family := Address_family (show.storage.ss_family);

    dest : char_array :=
      [1 .. size_t (if ai_family = v4  then v4_str_length elsif ai_family = v6  then v6_str_length else 0) => char'Val (0)];

    dest_length : size_t := dest'Length;
  begin
    if dest_length = 0 then
      return "unknown";
    end if;

    if ai_family = v6 then
      b1 :
      declare
        ip6_tmp : sockaddr_in6;

        for ip6_tmp'Address use show.storage'Address;
      begin
        inners.inner_inet_ntop (int (ip6_tmp.sin6_family), ip6_tmp.sin6_addr.s6_addr'Address, dest'Address, socklen_t (dest'Length));
      end b1;
    else
      b2 :
      declare
        ip4_tmp : sockaddr_in;

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
    (show  : not null addresses_access) return String
  is
    ai_family  : constant  Address_family := Address_family (show.storage.ss_family);
  begin
    if not (ai_family = v6 or else ai_family = v4) then
      return "unknown";
    end if;

    if ai_family = v6 then
      b1 :
      declare
        ip6_tmp : sockaddr_in6;

        for ip6_tmp'Address use show.storage'Address;
        ai_port   : constant String := inners.inner_ntohs (ip6_tmp.sin6_port)'Image;
      begin
        return ai_port (ai_port'First + 1 .. ai_port'Last);
      end b1;
    end if;

    if ai_family = v4 then
      b2 :
      declare
        ip4_tmp : sockaddr_in;

        for ip4_tmp'Address use show.storage'Address;
        ai_port   : constant String := inners.inner_ntohs (ip4_tmp.sin_port)'Image;
      begin
        return ai_port (ai_port'First + 1 .. ai_port'Last);
      end b2;
    end if;

    return "unknown"; -- to satisfy compiler :-)
  end get_port;


  function get_address_and_port
    (show  : not null addresses_access) return String
  is
  begin
    return  " address := " & get_addresses (show) & "  port := " & get_port (show);
  end get_address_and_port;


  function get_address_family
    (show  : not null addresses_access) return Address_family
  is (Address_family (show.storage.ss_family));


  function is_null
    (addr  : not null addresses_access) return Boolean
  is (null_addresses = addr.all);


  overriding
  procedure Read
    (Stream : in out socket_buffer;
     Item   : out Stream_Element_Array;
     Last   : out Stream_Element_Offset)
  is
  begin
    if Stream.data = null
      or else Stream.tail_end <  Stream.data'First
      or else Stream.head_first < Stream.data'First
    then
      raise buffer_empty_error;
    end if;

    if Item'Length = 0 then
      b1 :
      declare
        mi_empty  : constant Stream_Element_Array (1 .. 0) := [others => 0];
      begin
        Item := mi_empty;
        Last  := Item'Last;
        return;
      end b1;
    end if;

    if Item'Length > (Stream.tail_end + 1) - Stream.head_first then
        raise buffer_insufficient_space_error with " Insufficient amount of data left in buffer to fill the request. ";
    end if;

    Item  :=  Stream.data (Stream.head_first .. (Stream.head_first + Item'Length) - 1);
    Last  :=  Item'Last;
    Stream.head_first :=  Stream.head_first + Item'Length;
  end Read;


  overriding
  procedure Write
    (Stream : in out socket_buffer;
     Item   : in Stream_Element_Array)
  is
    item_len : constant Stream_Element_Count := Item'Length;

    old_data : constant Stream_Element_Array :=
      (if item_len > 0 and then Stream.data /= null and then (item_len > data_tail_length (Stream) or else
        item_len > max_data_length (Stream))
        then
          (declare
            data_tmp : constant Stream_Element_Array (1 .. Stream_Element_Count (actual_data_size (Stream)))
            := Stream.data (Stream.head_first .. Stream.tail_end);
          begin
            data_tmp
          )
        else
          Stream_Element_Array'(1 .. 0 => 0)
      );
      mve : Boolean := False;
  begin
    if item_len = 0 then
      return;
    end if;

    if Stream.data = null or else item_len > max_data_length (Stream) then

      Stream.data := new Stream_Element_Array'(1 .. Stream_Element_Count'Max (40, item_len + 40) => 0);
      Stream.head_first := 0;
      Stream.tail_end := 0;

      mve := True;

    end if;

    if mve or else item_len > data_tail_length (Stream) then

      Stream.data (old_data'Range) := old_data;
      Stream.data (old_data'Last + 1 .. Stream.data'Last) := [others => 0];

      Stream.head_first := 1;
      Stream.tail_end := old_data'Last;
    end if;

    if Stream.head_first < Stream.data.all'First then
      Stream.head_first := Stream.data.all'First;
    end if;

    Stream.data (Stream.tail_end + 1 .. Stream.tail_end + item_len)  :=  Item;
    Stream.tail_end :=  Stream.tail_end + item_len;
  end Write;


  function init_socket
    (sock  : in out socket_access;
     addr  : not null addresses_access) return Boolean
  is
    sockfd    : aliased socket_type  := 0;
    sock_tmp  : socket;

  begin

    sock_tmp.storage  := addr.all;
    sockfd  :=  inners.inner_socket (int (sock_tmp.storage.storage.ss_family), sock_tmp.storage.socktype, sock_tmp.storage.protocol);

    if sockfd = invalid_socket then
      return False;
    end if;

    if sock = null then
      sock := new socket;
    end if;

    clean (sock);

    sock_tmp.sock   := sockfd;
    sock.all  := sock_tmp;

    return True;
  end init_socket;


  function init_socket
    (sock  : in out socket_access;
     addr  : not null addresses_list_access) return Boolean
  is
    sockfd    : aliased socket_type  := 0;
    sock_tmp  : socket;
    ok        : Boolean := False;
  begin

    loop1 :
    for addr_tmp of addr.all loop

      sock_tmp.storage  := addr_tmp;
      sockfd := inners.inner_socket (int (sock_tmp.storage.storage.ss_family), sock_tmp.storage.socktype, sock_tmp.storage.protocol);

      if sockfd /= invalid_socket then
        ok := True;
        exit loop1;
      end if;

    end loop loop1;

    if not ok then
      return False;
    end if;

    if sock = null then
      sock := new socket;
    end if;

    clean (sock);

    sock_tmp.sock   := sockfd;
    sock.all  := sock_tmp;

    return True;
  end init_socket;


  procedure reuse_address
    (sock  : not null socket_access) is
  begin
    inners.inner_reuse_address (sock.sock);
  end reuse_address;


  function bind
    (sock  : not null socket_access) return Boolean
  is
  begin
      if inners.inner_bind (sock.sock, sock.storage.storage'Address, sock.storage.address_length) /= 0 then
        return False;
      end if;

      sock.binded := True;

      return True;
  end bind;


  function listen
    (sock     : not null socket_access;
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
    (sock     : not null socket_access;
     new_sock : in out socket_access) return Boolean
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

    if new_sock = null then
      new_sock := new socket;
    end if;

    new_sock.all := sock_tmp;

    return True;
  end accept_socket;


  function connect
    (sock  : not null socket_access) return Boolean
  is
  begin
    if inners.inner_connect (sock.sock, sock.storage.storage'Address, size_t (sock.storage.address_length)) /= 0 then
      return False;
    end if;

    sock.connected := True;

    return True;
  end connect;


  procedure close
    (sock  : not null socket_access)
  is
    sockfd : constant int := inners.inner_close (sock.sock)
      with unreferenced;

  begin
    clean (sock);
  end close;


  procedure clean
    (sock  : not null socket_access)
  is
  begin
    sock.sock    := 0;

    sock.storage.storage.ss_family  := 0;
    sock.storage.storage.padding    := [others => char'Val (0)];

    sock.storage.socktype  := 0;
    sock.storage.protocol  := 0;

    sock.storage.address_length := 0;

    sock.connected :=  False;
    sock.binded    :=  False;
    sock.listened  :=  False;
  end clean;


  function send
    (sock     : not null socket_access;
     buffer   : not null stream_element_array_access) return ssize_t
  is
      len         : ssize_t  := 0;
      pos         : Stream_Element_Offset  := buffer.all'First;
      remaining   : ssize_t  := buffer.all'Length;
  begin
    if remaining = 0 then
      return 0;
    end if;

    loop1 :
    loop
      len := inners.inner_send (sock.sock, buffer.all (pos)'Address, size_t (remaining), 0);

      exit loop1 when len < 1 or else len = socket_error;

      pos := pos + Stream_Element_Offset (len);

      exit loop1 when remaining = len;

      remaining :=  remaining - len;

      exit loop1 when remaining < 1;
    end loop loop1;

    if len = socket_error then
      return socket_error;
    end if;

    return ssize_t (pos - buffer.all'First);
  end send;


  function send
    (sock     : not null socket_access;
     buffer   : not null socket_buffer_access) return ssize_t
  is
    len         : ssize_t  := 0;
    pos         : Stream_Element_Offset  := (if buffer.data /= null then buffer.data'First else 0);
    remaining   : ssize_t  := (if buffer.data /= null then buffer.data'Length else 0);
  begin
    if remaining = 0 then
      return 0;
    end if;

    loop1 :
    loop
      len := inners.inner_send (sock.sock, buffer.data (pos)'Address, size_t (remaining), 0);

      exit loop1 when len < 1 or else len = socket_error;

      pos := pos + Stream_Element_Offset (len);

      exit loop1 when remaining = len;

      remaining :=  remaining - len;

      exit loop1 when remaining < 1;
    end loop loop1;

    if len = socket_error then
      return socket_error;
    end if;

    buffer.head_first := buffer.head_first + Stream_Element_Count (len);

    return ssize_t (pos - buffer.data'First);
  end send;


  function sendto
    (sock     : not null socket_access;
     send_to  : not null addresses_access;
     buffer   : not null stream_element_array_access) return ssize_t
  is
    len         : ssize_t  := 0;
    pos         : Stream_Element_Offset  :=  buffer.all'First;
    remaining   : ssize_t  := buffer.all'Length;
  begin
    if remaining = 0 then
      return 0;
    end if;

    loop1 :
    loop
      len :=  inners.inner_sendto (sock.sock, buffer.all (pos)'Address, size_t (remaining), 0, send_to.storage'Address,
        socklen_t (send_to.address_length));

      exit loop1 when len < 1 or else len = socket_error;

      pos := pos + Stream_Element_Offset (len);

      exit loop1 when remaining = len;

      remaining :=  remaining - len;

      exit loop1 when remaining < 1;
    end loop loop1;

    if len = socket_error then
      return socket_error;
    end if;

    return ssize_t (pos - buffer.all'First);
  end sendto;


  function sendto
    (sock    : not null socket_access;
    send_to  : not null addresses_access;
    buffer   : not null socket_buffer_access) return ssize_t
  is
    len         : ssize_t  := 0;
    pos         : Stream_Element_Offset  := (if buffer.data /= null then buffer.data'First else 0);
    remaining   : ssize_t  := (if buffer.data /= null then buffer.data'Length else 0);
  begin
    if remaining = 0 then
      return 0;
    end if;

    loop1 :
    loop
      len :=  inners.inner_sendto (sock.sock, buffer.data (pos)'Address, size_t (remaining), 0, send_to.storage'Address,
        socklen_t (send_to.address_length));

      exit loop1 when len < 1 or else len = socket_error;

      pos := pos + Stream_Element_Offset (len);

      exit loop1 when remaining = len;

      remaining :=  remaining - len;

      exit loop1 when remaining < 1;
    end loop loop1;

    if len = socket_error then
      return socket_error;
    end if;

    buffer.head_first := buffer.head_first + Stream_Element_Count (len);

    return ssize_t (pos - buffer.data'First);
  end sendto;


  function receive
    (sock     : not null socket_access;
     buffer   : in out stream_element_array_access;
     max_len  : Stream_Element_Count := 1500) return ssize_t
  is
    data_tmp  : Stream_Element_Array := [1 .. max_len => 0];
    len       : ssize_t  := 0;
  begin
    len :=  inners.inner_recv (sock.sock, data_tmp (data_tmp'First)'Address, size_t (data_tmp'Length), 0);

    if len = socket_error then
      return socket_error;
    end if;

    buffer := new Stream_Element_Array'(data_tmp (1 .. Stream_Element_Offset (len)));

    return len;
  end receive;


  function receive
    (sock     : not null socket_access;
     buffer   : in out socket_buffer_access;
     max_len  : Stream_Element_Count := 1500) return ssize_t
  is
    data_tmp  : Stream_Element_Array := [1 .. max_len => 0];
    len       : ssize_t  := 0;
  begin
    len :=  inners.inner_recv (sock.sock, data_tmp (data_tmp'First)'Address, size_t (data_tmp'Length), 0);

    if len = socket_error then
      return socket_error;
    end if;

    if buffer = null then
      buffer := new socket_buffer;
    end if;

    Stream_Element_Array'Write (buffer.all'Access, data_tmp (1 .. Stream_Element_Offset (len)));

    return len;
  end receive;


  function receive_from
    (sock     : not null socket_access;
     buffer   : in out stream_element_array_access;
     from     : in out addresses_access;
     max_len  : Stream_Element_Count := 1500) return ssize_t
  is
    data_tmp  : Stream_Element_Array := [1 .. max_len => 0];
    len       : ssize_t  := 0;
    from_tmp  : aliased addresses := sock.storage;
    len_tmp   : aliased socklen_t := socklen_t (from_tmp.storage'Size / 8);

  begin

    from_tmp.storage.ss_family  := 0;
    from_tmp.storage.padding    := [others => char'Val (0)];

    len :=  inners.inner_recvfrom (sock.sock, data_tmp (data_tmp'First)'Address, size_t (data_tmp'Length), 0,
      from_tmp.storage'Address, len_tmp);

    if len = socket_error then
      return socket_error;
    end if;

    from_tmp.address_length := int (len_tmp);

    buffer := new Stream_Element_Array'(data_tmp (1 .. Stream_Element_Offset (len)));

    if from = null then
      from  :=  new addresses;
    end if;

    from.all  :=  from_tmp;

    return len;
  end receive_from;


  function receive_from
    (sock     : not null socket_access;
     buffer   : in out socket_buffer_access;
     from     : in out addresses_access;
     max_len  : Stream_Element_Count := 1500) return ssize_t
  is
    data_tmp  : Stream_Element_Array := [1 .. max_len => 0];
    len       : ssize_t  := 0;
    from_tmp  : aliased addresses := sock.storage;
    len_tmp   : aliased socklen_t := socklen_t (from_tmp.storage'Size / 8);

  begin

    from_tmp.storage.ss_family  := 0;
    from_tmp.storage.padding    := [others => char'Val (0)];

    len :=  inners.inner_recvfrom (sock.sock, data_tmp (data_tmp'First)'Address, size_t (data_tmp'Length), 0,
      from_tmp.storage'Address, len_tmp);

    if len = socket_error then
      return socket_error;
    end if;

    from_tmp.address_length := int (len_tmp);

    if from = null then
      from  := new addresses;
    end if;

    if buffer = null then
      buffer  := new socket_buffer;
    end if;

    Stream_Element_Array'Write (buffer.all'Access, data_tmp (1 .. Stream_Element_Offset (len)));

    from.all  :=  from_tmp;

    return len;
  end receive_from;


  function get_sock
    (sock : not null socket_access) return socket_type
  is (sock.sock);


  function get_addresses
    (sock : not null socket_access) return addresses
  is (sock.storage);


  function initialized
    (sock  : not null socket_access) return Boolean
  is (sock.sock /= 0 and then sock.sock /= invalid_socket);

  function initialized
    (addr  : not null addresses_list_access) return Boolean
  is (addr.all'Length >= 1 and then (not (for some A of addr.all => A = null_addresses)));

  function initialized
    (addr  : not null addresses_access) return Boolean
  is (addr.all /= null_addresses);


  function connected
    (sock  : not null socket_access) return Boolean
  is (sock.connected);


  function binded
    (sock  : not null socket_access) return Boolean
  is (sock.binded);


  function listened
    (sock  :  not null socket_access) return Boolean
  is (sock.listened);


  function is_empty
    (buffer : not null socket_buffer_access) return Boolean
  is (buffer.data = null or else buffer.tail_end < buffer.data'First or else buffer.head_first < buffer.data'First);



  function actual_data_size
    (buffer : not null socket_buffer_access) return Integer_64
  is (if buffer.data /= null then (Integer_64 ((buffer.tail_end + 1) - buffer.head_first)) else 0);

  function actual_data_size
    (buffer : socket_buffer) return Integer_64
  is (if buffer.data /= null then (Integer_64 ((buffer.tail_end + 1) - buffer.head_first)) else 0);


  function max_data_length
    (buffer : socket_buffer) return Stream_Element_Offset
  is (if buffer.data /= null then (buffer.head_first - buffer.data'First) + (buffer.data'Last - buffer.tail_end) else 0);


  function data_tail_length
    (buffer : socket_buffer) return Stream_Element_Offset
  is (if buffer.data /= null then buffer.data'Last - buffer.tail_end else 0);


  function get_buffer_init
    (buffer : not null socket_buffer_access) return socket_buffer
  is
  begin
    if buffer.data /= null then
      return
        socket_buffer'(
          Root_Stream_Type with
          data  => new Stream_Element_Array'(buffer.data.all),
          head_first  => buffer.head_first,
          tail_end  => buffer.tail_end
        );
    end if;

    return
      socket_buffer'(
        Root_Stream_Type with
        data  => null,
        head_first  => buffer.head_first,
        tail_end  => buffer.tail_end
      );
  end get_buffer_init;


  procedure clean
    (buffer : not null socket_buffer_access)
  is
  begin
    buffer.head_first := 0;
    buffer.tail_end := 0;
    buffer.data := null;
  end clean;


  function add_raw
    (buffer : not null socket_buffer_access;
     raw    : not null stream_element_array_access) return Boolean
  is
  begin
    if raw.all'Length = 0 then
      return True;
    end if;
    Stream_Element_Array'Write (buffer.all'Access, raw.all);

    return True;
  end add_raw;


  function get_raw
    (buffer : not null socket_buffer_access) return Stream_Element_Array
  is
  begin
    if buffer.data = null then
      b1 :
      declare
        mi_data : Stream_Element_Array :=  [1 .. 0 => 0];
      begin
        return mi_data;
      end b1;
    end if;

    return buffer.data (buffer.head_first .. buffer.tail_end);
  end get_raw;


  function string_error return String is
    message_a : aliased char_array (1 .. 260) := [others => char'Val (0)];
    length_a  : aliased int :=  int (message_a'Last) - 1;
  begin
    inners.inner_show_error (message_a, length_a);

    return To_Ada (message_a (message_a'First .. message_a'First + size_t (length_a)));
  end string_error;



end adare_net.sockets;
