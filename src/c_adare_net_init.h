
#ifndef C_ADARE_NET_INIT
#define C_ADARE_NET_INIT

#ifdef _WIN32

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
