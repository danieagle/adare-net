
#include <stdint.h>

#ifdef _WIN32

#ifndef _WIN64
#error OS Need be of 64bit
#endif

#include <winsock2.h>
#include <ws2tcpip.h>
#include <stdio.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#include <errno.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _WIN32

  typedef uint64_t c_socket_type;

#else

  typedef int c_socket_type;
#endif

struct sockaddr_storage_ada {
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
  struct addresses list[] // Todo: return list ?
);

void c_show_error (
  char message[],
  int * len
);

void c_reuse_address (c_socket_type fd);

#ifdef __cplusplus
}
#endif
