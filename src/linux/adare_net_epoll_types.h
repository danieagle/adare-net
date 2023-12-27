
#ifndef ADARE_NET_EPOLL_TYPES_H
#define ADARE_NET_EPOLL_TYPES_H

#include <sys/epoll.h>

unsigned long adare_epoll_epollin = EPOLLIN;
unsigned long adare_epoll_epollout = EPOLLOUT;
unsigned long adare_epoll_epollrdhup = EPOLLRDHUP;
unsigned long adare_epoll_epollpri = EPOLLPRI;

unsigned long adare_epoll_epollerr = EPOLLERR;
unsigned long adare_epoll_epollhup = EPOLLHUP;



#endif