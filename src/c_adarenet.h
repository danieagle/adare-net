
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

#endif

#ifndef _WIN32

#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>

typedef int c_socket_type;

#endif


#ifdef __cplusplus
extern "C" {
#endif

void c_show_error (
  char message[],
  int * len
);

void c_reuse_address (c_socket_type fd);

#ifdef __cplusplus
}
#endif

#endif
