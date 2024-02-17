
#ifndef ADARE_NET_EPOLL_TYPES
#define ADARE_NET_EPOLL_TYPES

#include "wepoll.h"

unsigned long adare_epoll_epollin = EPOLLIN;
unsigned long adare_epoll_epollout = EPOLLOUT;

int adare_epoll_cmd_add = EPOLL_CTL_ADD;
int adare_epoll_cmd_mod = EPOLL_CTL_MOD;
int adare_epoll_cmd_del = EPOLL_CTL_DEL;

#endif