
#ifndef C_ADARE_NET_INIT
#define C_ADARE_NET_INIT

#ifdef _WIN32

// #ifndef WIN32_LEAN_AND_MEAN
// #define WIN32_LEAN_AND_MEAN
// #endif

// #undef WINVER
// #undef _WIN32_WINNT

// #define WINVER 0x0A00
// #define _WIN32_WINNT 0x0A00

#include <winsock2.h>
#include <ws2tcpip.h>
#include <stdint.h>

#endif


#ifdef __cplusplus
extern "C" {
#endif

  void c_start_adare_net (void);

  void c_stop_adare_net (void);

#ifdef __cplusplus
}
#endif

#endif
