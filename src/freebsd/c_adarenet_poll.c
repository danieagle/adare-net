
#ifndef ADARE_NET_EPOLL_TYPES
#define ADARE_NET_EPOLL_TYPES

#include <sys/types.h>
#include <sys/event.h>
#include <time.h>

short adare_kpoll_filter_read   = EVFILT_READ;
short adare_kpoll_filter_write  = EVFILT_WRITE;

unsigned short adare_kpoll_flag_add     = EV_ADD;
unsigned short adare_kpoll_flag_enable  = EV_ENABLE;
unsigned short adare_kpoll_flag_del     = EV_DELETE;


void* msec_to_timespec(int msec)
{
    struct timespec ts;

    if (msec < 1000){
        ts->tv_sec = 0;
        ts->tv_nsec = msec * 1000000;
    }
    else {
        ts->tv_sec = msec / 1000;
        ts->tv_nsec = (msec - ts->tv_sec * 1000) * 1000000;
    }

    return (void*)ts;
}

// void msec_to_timespec(unsigned long msec, struct timespec *ts)
// {
//     if (msec < 1000){
//         ts->tv_sec = 0;
//         ts->tv_nsec = msec * 1000000;
//     }
//     else {
//         ts->tv_sec = msec / 1000;
//         ts->tv_nsec = (msec - ts->tv_sec * 1000) * 1000000;
//     }
// }

#endif
