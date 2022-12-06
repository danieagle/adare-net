
with Interfaces.C;
use Interfaces, Interfaces.C;

package socket_types
  with  Pure
is

  type socket_type is new int;

  invalid_socket  : constant socket_type := -1;

  type ssize_t is new Integer_64;

  socket_error  : constant ssize_t  := -1;

  type socklen_t is new Integer_32;

end socket_types;
