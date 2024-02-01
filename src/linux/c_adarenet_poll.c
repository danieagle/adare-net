
#ifndef ADARE_NET_POLL
#define ADARE_NET_POLL

#ifdef __cplusplus
extern "C" {
#endif


#include <sys/epoll.h>

  unsigned long adare_epoll_epollin = EPOLLIN;
  unsigned long adare_epoll_epollout = EPOLLOUT;
  // unsigned long adare_epoll_epollrdhup = EPOLLRDHUP;
  // unsigned long adare_epoll_epollpri = EPOLLPRI;

  // unsigned long adare_epoll_epollerr = EPOLLERR;
  // unsigned long adare_epoll_epollhup = EPOLLHUP;

  int adare_epoll_cmd_add = EPOLL_CTL_ADD;
  int adare_epoll_cmd_mod = EPOLL_CTL_MOD;
  int adare_epoll_cmd_del = EPOLL_CTL_DEL;

#ifdef __cplusplus
}
#endif

#endif