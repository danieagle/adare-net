
with Interfaces.C;
use Interfaces, Interfaces.C;

package socket_types with
 Pure
is

  type socket_type is mod 2**(int'Size) with
    Size => int'Size, Convention => C;

  invalid_socket : constant socket_type := -1 with
    Size => int'Size, Convention => C;

  type signed_socket_type is range -(2**(int'Size - 1)) .. 2**(int'Size - 1) - 1 with
    Size => int'Size, Convention => C;

  missing_file_descriptor : constant signed_socket_type := -1 with
   Convention => C;

  type ssize_t is
    range -(2**(size_t'Size - 1)) .. 2**(size_t'Size - 1) - 1 with
    Size => size_t'Size, Convention => C;

  socket_error : constant ssize_t := -1 with
    Size => ssize_t'Size, Convention => C;

  type socklen_t is new Integer_32 with
    Size => Integer_32'Size, Convention => C;

end socket_types;
