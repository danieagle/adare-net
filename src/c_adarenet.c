
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

   const signed short c_event_pollin = POLLIN;
   const signed short c_event_pollout = POLLOUT;

   const signed short c_event_pollerror = POLLERR;
   const signed short c_event_pollhup = POLLHUP;
   const signed short c_event_pollnval = POLLNVAL;

   void c_init_address(
       const char *ip_or_host,
       const char *port,
       int ai_socktype,
       int ai_family,
       int *length,
       struct addresses list[])
   {
      struct addrinfo hints, *servinfo, *p;

      memset(&hints, 0, sizeof hints);
      hints.ai_family = ai_family;

      hints.ai_socktype = ai_socktype;

      hints.ai_flags = (ip_or_host[0] == 0 ? AI_PASSIVE : 0);

      if (getaddrinfo((ip_or_host[0] != 0 ? ip_or_host : NULL), port, &hints, &servinfo) != 0)
      {
         *length = 0;
         return;
      }

      int i = 1;
      int e = *length;

      for (p = servinfo; p != NULL && i < e; p = p->ai_next, ++i)
      {

         memset(&list[i - 1].storage, 0, sizeof(struct sockaddr_storage_ada));

         memcpy(&list[i - 1].storage, (struct sockaddr_storage_ada *)p->ai_addr, p->ai_addrlen);

         list[i - 1].socktype = p->ai_socktype;
         list[i - 1].protocol = p->ai_protocol;
         list[i - 1].address_length = p->ai_addrlen;
      }

      freeaddrinfo(servinfo);
      i--;
      *length = i;
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
