
package adare_net.sockets.utils
is

  procedure show_addresses
    (show  : not null access addresses_list);

  procedure show_addresses
    (show  : in addresses_list);

  procedure show_port
    (show  : not null access addresses_list);

  procedure show_port
    (show  : in addresses_list);

  procedure show_address_and_port
    (show  : not null access addresses_list);

  procedure show_address_and_port
    (show  : in addresses_list);


  procedure show_addresses
    (show  : not null access addresses);

  procedure show_addresses
    (show  : in addresses);

  procedure show_port
    (show  : not null access addresses);

  procedure show_port
    (show  : in addresses);

  procedure show_address_and_port
    (show  : not null access addresses);

  procedure show_address_and_port
    (show  : in addresses);


end adare_net.sockets.utils;
