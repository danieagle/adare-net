with Ada.Text_IO;
use Ada;

package body adare_net.sockets.utils
is

  procedure show_addresses
    (show  : not null addresses_list_access)
  is
  begin
    for a_tmp of show.all loop
      Text_IO.Put_Line (get_addresses (a_tmp'Access));
    end loop;
  end show_addresses;

  procedure show_port
    (show  : not null addresses_list_access)
  is
  begin
    for a_tmp of show.all loop
      Text_IO.Put_Line (get_port (a_tmp'Access));
    end loop;
  end show_port;

  procedure show_address_and_port
    (show  : not null addresses_list_access)
  is
  begin
    for a_tmp of show.all loop
      Text_IO.Put_Line (get_address_and_port (a_tmp'Access));
    end loop;
  end show_address_and_port;


  procedure show_addresses
    (show  : not null addresses_access)
  is
  begin
    Text_IO.Put_Line (get_addresses (show));
  end show_addresses;

  procedure show_port
    (show  : not null addresses_access)
  is
  begin
    Text_IO.Put_Line (get_port (show));
  end show_port;

  procedure show_address_and_port
    (show  : not null addresses_access)
  is
  begin
    Text_IO.Put_Line (get_address_and_port (show));
  end show_address_and_port;

end adare_net.sockets.utils;
