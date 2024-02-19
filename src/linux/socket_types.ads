
with Interfaces.C;
use Interfaces, Interfaces.C;

with System;
use System;

package socket_types
  with  Pure
is

  type socket_type is new int;

  invalid_socket  : constant socket_type := -1;

  type ssize_t is range -(2**(size_t'Size - 1)) .. +(2**(size_t'Size - 1) - 1);

  socket_error  : constant ssize_t  := -1;

  type socklen_t is new Integer_32;

  subtype handle_type is socket_type;

  failed_handle : constant handle_type  := -1;


  type addr_info is
    record
      ai_flags    : Interfaces.C.int := 0;
      ai_family   : Interfaces.C.int := 0;
      ai_socktype : Interfaces.C.int := 0;
      ai_protocol : Interfaces.C.int := 0;
      ai_addrlen  : socklen_t := 0;
      ai_canonname  : Address :=  Null_Address;
      ai_addr       : Address := Null_Address;
      ai_next       : Address :=  Null_Address;
    end record
      with Convention => C;

end socket_types;
