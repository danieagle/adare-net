
#include "c_adare_net_init.h"

#ifdef __cplusplus
extern "C" {
#endif


  void c_start_adare_net (void){

#ifdef _WIN32
  WSADATA mi_data;
  WSAStartup(MAKEWORD(2, 2), &mi_data);

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
