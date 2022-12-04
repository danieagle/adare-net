
package adare_net_exceptions
  with Pure
is
  buffer_empty_error : exception;
  buffer_insufficient_space_error : exception;

  receive_error : exception;
  send_error  : exception;

end adare_net_exceptions;
