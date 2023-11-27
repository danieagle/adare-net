
with Interfaces;
use Interfaces;

package socket_types
  with  Pure
is

  type socket_type is mod 2**(int'Size)
    with Size => int'Size;

  invalid_socket  : constant socket_type := -1;

  type ssize_t is range -2**(size_t'Size - 1) .. 2**(size_t'Size - 1) - 1
    with Size => size_t'Size;

  socket_error  : constant ssize_t  := -1;

  type socklen_t is new Integer_32
    with Size => Integer_32'Size;

end socket_types;
