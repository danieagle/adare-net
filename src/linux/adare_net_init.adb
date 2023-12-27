
package body adare_net_init
  with Preelaborate
is
  procedure start_adare_net is

    procedure inner2 with
    Import => True, Convention => Ada, External_Name => "adare_netinit";

    function inner1 return Integer with
    Import        => True, Convention => C,
    External_Name => "c_start_adare_net";

    ok : Boolean := False;
  begin

    ok := (inner1 = 0);

    if not ok then
      raise Program_Error;
    end if;

    inner2;
  end start_adare_net;

  procedure stop_adare_net
  is
    function inner1 return Integer
      with Import => True, Convention => stdcall,
        External_Name => "c_stop_adare_net";

    procedure inner2
      with Import => True, Convention => Ada,
      External_Name => "adare_netfinal";

      ok : Integer := 0
        with unreferenced;
  begin
    inner2;
    ok := inner1;

  end stop_adare_net;

end adare_net_init;
