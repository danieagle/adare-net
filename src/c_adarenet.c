
#ifdef __cplusplus
extern "C" {
#endif

#include "c_adarenet.h"

  const uint16_t c_af_unspec = AF_UNSPEC;
  const uint16_t c_af_inet = AF_INET;
  const uint16_t c_af_inet6 = AF_INET6;

  const int c_v4_addrstrlen = INET_ADDRSTRLEN;
  const int c_v6_str_length = INET6_ADDRSTRLEN;

  const int c_sock_dgram = SOCK_DGRAM;
  const int c_sock_stream = SOCK_STREAM;

  const int c_ai_passive = AI_PASSIVE;

void c_show_error (
  char message [],
  int * len
){
#ifdef _WIN32

  int err = WSAGetLastError ();

  char  msg_tmp [256];   // for a message up to 255 bytes.
  msg_tmp [0] = '\0';    // Microsoft doesn't guarantee this on man page.

  FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM |
    FORMAT_MESSAGE_IGNORE_INSERTS,   // flags
    NULL,                // lpsource
    err,                 // message id
    MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),    // languageid
    msg_tmp,              // output buffer
    sizeof (msg_tmp),     // size of msgbuf, bytes
    NULL);               // va_list of arguments

  if (! *msg_tmp)
    sprintf (msg_tmp, "%d", err);  // provide error # if no string available

#else
  const char * msg_tmp = strerror (errno);

#endif

  const int msg_tmp_length = strlen (msg_tmp);

  *len = (msg_tmp_length < *len ? msg_tmp_length : *len);

  memcpy (message, msg_tmp, (size_t)*len);
}

void c_reuse_address (c_socket_type fd){
#ifdef _WIN32
  char optval = '1';
#else
  int optval = 1;
#endif

  setsockopt (fd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof optval);
}

#ifdef __cplusplus
}
#endif
