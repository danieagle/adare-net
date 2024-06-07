
#ifndef C_ADARENET_H
#define C_ADARENET_H

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

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>

typedef int c_socket_type;

#endif

#ifdef __cplusplus
extern "C" {
#endif

enum address_family {any, ipv4, ipv6};

enum address_type {tcp, udp};

void create_addresses (
  const void* host,
  const void* service,
  void* data,
  size_t* data_length,
  enum address_family addr_family,
  enum address_type addr_type
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
