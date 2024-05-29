
with Interfaces.C;
use Interfaces, Interfaces.C;


package socket_types
  with  Pure
is

  type socket_type is new int;

  invalid_socket  : constant socket_type := -1;

  socket_error    : constant int  := -1;

end socket_types;
