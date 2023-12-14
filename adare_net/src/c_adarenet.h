
#if !defined C_ADARENET_H
#define C_ADARENET_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>

#ifdef _WIN32

#include <winsock2.h>
#include <ws2tcpip.h>

#ifdef _WIN64
  typedef uint64_t c_socket_type;
#else
  typedef uint32_t c_socket_type;
#endif
   
#else

#include <netdb.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <poll.h>
#include <errno.h>

    typedef int c_socket_type;
#endif

   struct sockaddr_storage_ada
   {
      uint16_t ss_family;
      uint8_t padding[132];
};

struct addresses  {
  struct sockaddr_storage_ada storage;
  int socktype;
  int protocol;
  int address_length;
};

void c_init_address  (
  const char * ip_or_host,
  const char * port,
  int ai_socktype,
  int ai_family,
  int *length,
  struct addresses list[]
);

void c_show_error (
  char message[],
  int * len
);

void c_reuse_address (c_socket_type fd);

#ifdef __cplusplus
}
#endif

#endif
