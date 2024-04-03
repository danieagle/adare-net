
#ifdef __cplusplus
extern "C" {
#endif

#include "c_adarenet.h"

  const uint16_t c_af_unspec = AF_UNSPEC;
  const uint16_t c_af_inet = AF_INET;
  const uint16_t c_af_inet6 = AF_INET6;

  const size_t c_v4_addrstrlen = INET_ADDRSTRLEN;
  const size_t c_v6_str_length = INET6_ADDRSTRLEN;

  const int c_sock_dgram = SOCK_DGRAM;
  const int c_sock_stream = SOCK_STREAM;

  const size_t c_size_int = sizeof (int);
  const size_t c_size_uint16 = sizeof (uint16_t);

void create_addresses (
  const void* host,
  const void* service,
  void* data,
  size_t* data_length,
  enum address_family addr_family,
  enum address_type addr_type
){

  if (data == NULL)
  {
    *data_length = 0;
    return;
  }

  if (*data_length <  sizeof (int))
  {
    return;
  }

  struct addrinfo hints, *servinfo, *p;

  memset(&hints, 0, sizeof hints);
  hints.ai_family = (addr_family == any ? AF_UNSPEC : (addr_family == ipv6 ? AF_INET6 : AF_INET));

  hints.ai_socktype = (addr_type == tcp ? SOCK_STREAM : SOCK_DGRAM);

  hints.ai_flags = (host == NULL ? AI_PASSIVE : 0);

  if (getaddrinfo(
    (host == NULL ? NULL : (char *)host),
    (service == NULL ? NULL : (char *)service),
    &hints, &servinfo)
    != 0
  )
  {
      *data_length = 0;
      return;
  }

  int i = 0;
  int d = sizeof (int) + sizeof (uint16_t); // protocol + address length
  int e = *data_length;
  char* dt = data;

  uint16_t  a_u16 = 0;
  int       a_int = (int)(addr_type == tcp ? SOCK_STREAM : SOCK_DGRAM);

  memcpy (&dt[i], &a_int, sizeof (int));

  i += sizeof (int);

  for (p = servinfo; p != NULL && i + d + (int)p->ai_addrlen < e; p = p->ai_next, ++i)
  {
    a_int = p->ai_protocol;

    memcpy(&dt[i], &a_int, sizeof(int));

    i += sizeof(int);

    a_u16 = (uint16_t) p->ai_addrlen;

    memcpy(&dt[i], &a_u16, sizeof (uint16_t));

    i += sizeof (uint16_t);

    memcpy(&dt[i], p->ai_addr, p->ai_addrlen);

    i += p->ai_addrlen;

  }

  freeaddrinfo(servinfo);
  *data_length = i;
}


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
