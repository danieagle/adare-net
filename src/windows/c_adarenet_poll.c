
#ifndef ADARE_NET_EPOLL_TYPES
#define ADARE_NET_EPOLL_TYPES

#include "wepoll.h"

const unsigned long adare_epoll_epollin = EPOLLIN;
const unsigned long adare_epoll_epollout = EPOLLOUT;

const int adare_epoll_cmd_add = EPOLL_CTL_ADD;
const int adare_epoll_cmd_mod = EPOLL_CTL_MOD;
const int adare_epoll_cmd_del = EPOLL_CTL_DEL;

#endif
