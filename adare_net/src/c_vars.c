
#include <stdint.h>

#ifdef _WIN32

#ifndef _WIN64
#error  OS Need be of 64bit
#endif

#include <winsock2.h>
#include <ws2tcpip.h>

#else

#include <sys/socket.h>
#include <arpa/inet.h>
#include <poll.h>

#endif

#ifdef __cplusplus
extern "C" {
#endif

const uint16_t c_af_unspec = AF_UNSPEC;
const uint16_t c_af_inet = AF_INET;
const uint16_t c_af_inet6 = AF_INET6;

const int c_v4_addrstrlen = INET_ADDRSTRLEN;
const int c_v6_str_length = INET6_ADDRSTRLEN;

const int c_sock_dgram = SOCK_DGRAM;
const int c_sock_stream = SOCK_STREAM;

const unsigned short c_event_pollin  = POLLIN;
const unsigned short c_event_pollout = POLLOUT;
const unsigned short c_event_pollerror = POLLERR;
const unsigned short c_event_pollhup   = POLLHUP;
const unsigned short c_event_pollnval  = POLLNVAL;



#ifdef __cplusplus
}
#endif
