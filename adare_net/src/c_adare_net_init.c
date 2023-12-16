
#include "c_adare_net_init.h"

#ifdef __cplusplus
extern "C" {
#endif


  int c_start_adare_net (void){

#ifdef _WIN32
  WORD mi_ver;
  WSADATA mi_data;
  int mi_err;

  mi_ver = MAKEWORD(2, 2);
  mi_err = WSAStartup(mi_ver, &mi_data);

  return mi_err;
#endif

    return 0;
  }

  int c_stop_adare_net (void){

#ifdef _WIN32

  int mi_err;
  mi_err = WSACleanup();
  return mi_err;

#endif
    return 0;
  }

#ifdef __cplusplus
}
#endif
