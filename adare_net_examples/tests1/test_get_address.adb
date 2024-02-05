
with adare_net_init;  use adare_net_init;
with adare_net.base;  use adare_net.base;

with Ada.Text_IO; use Ada;

procedure test_get_address is
begin

  start_adare_net;

  b0 :
  declare
    mi_address    : aliased socket_address;
  begin

    mi_address :=
    create_address
      (host_or_ip => "google.com",
      network_port_or_service => "0",
      Addr_family => any,
      Addr_type => tcp);

    Text_IO.Put_Line (" addresses from google ");
    Text_IO.Put_Line (" address => " & get_address (mi_address) & " and port => " & get_address_port (mi_address));
      Text_IO.New_Line;


    Text_IO.Put_Line ("Completed.");

  end b0;
  stop_adare_net;

end test_get_address;