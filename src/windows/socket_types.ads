
with Interfaces.C;
use Interfaces, Interfaces.C;

with System;  use System;

package socket_types with
 Pure
is
  type socket_type is mod 2**(int'Size);

  invalid_socket : constant socket_type := -1;

  socket_error : constant int := -1;

  subtype handle_type is Address;

  function failed_handle return handle_type
  is (handle_type (Null_Address))
  with Inline;

end socket_types;
