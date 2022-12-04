
#include "c_adare_net_init.h"

#ifdef _WIN32

#define WINVER 0x0601
#define _WIN32_WINNT 0x0601

#include <stdint.h>
#include <winsock2.h>
#include <ws2tcpip.h>

#endif

#ifdef __cplusplus
extern "C" {
#endif

  void c_start_adare_net (void){

    #ifdef _WIN32
      WSADATA wsaData;
      WSAStartup(MAKEWORD(2,2), &wsaData);
    #endif

  }

  void c_stop_adare_net (void){

    #ifdef _WIN32
      WSACleanup();
    #endif
  }

#ifdef __cplusplus
}
#endif
