
package body adare_net_init
  with Preelaborate
is
  procedure start_adare_net
  is
    procedure inner1
      with Import => True, Convention => stdcall,
        External_Name => "c_start_adare_net";

    procedure inner2
      with Import => True, Convention => Ada,
      External_Name => "adare_netinit";
  begin
    inner2;
    inner1;
  end start_adare_net;

  procedure stop_adare_net
  is
    procedure inner1
      with Import => True, Convention => stdcall,
        External_Name => "c_stop_adare_net";

    procedure inner2
      with Import => True, Convention => Ada,
      External_Name => "adare_netfinal";
  begin
    inner2;
    inner1;
  end stop_adare_net;

end adare_net_init;
