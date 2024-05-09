
#ifndef ADARE_NET_EPOLL_TYPES
#define ADARE_NET_EPOLL_TYPES

#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>

short adare_kpoll_filter_read   = EVFILT_READ;
short adare_kpoll_filter_write  = EVFILT_WRITE;

unsigned short adare_kpoll_flag_add     = EV_ADD;
unsigned short adare_kpoll_flag_enable  = EV_ENABLE;
unsigned short adare_kpoll_flag_clear   = EV_CLEAR;
unsigned short adare_kpoll_flag_del     = EV_DELETE;
unsigned short adare_kpoll_flag_error   = EV_ERROR;

void * misec_to_timespec(const int misec)
{

    struct timespec * timeit = NULL;

    if (misec > 1000){
        timeit->tv_sec = misec / 1000;
        timeit->tv_nsec = (misec - (timeit->tv_sec * 1000)) * 1000000;
    } else {
        timeit->tv_sec = 0;
        timeit->tv_nsec = misec * 1000000;
    }

    return (void *)(timeit);
}

#endif
