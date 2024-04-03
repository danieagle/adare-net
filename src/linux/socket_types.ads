
with Interfaces.C;
use Interfaces, Interfaces.C;

package socket_types
  with  Pure
is

  type socket_type is new int;

  invalid_socket  : constant socket_type := -1;

  type ssize_t is range -(2**(size_t'Size - 1)) .. +(2**(size_t'Size - 1) - 1);

  socket_error  : constant ssize_t  := -1;

  subtype handle_type is socket_type;

  failed_handle : constant handle_type  := -1;

end socket_types;
