
with Interfaces.C;
use Interfaces, Interfaces.C;

package socket_types
  with  Pure
is

  type socket_type is new int;

  invalid_socket  : constant socket_type := -1;

  socket_error    : constant int  := -1;

  subtype handle_type is socket_type;

  failed_handle : constant handle_type  := -1;

  type adr_uintptr_t is mod 2**(Address'Size);

end socket_types;
