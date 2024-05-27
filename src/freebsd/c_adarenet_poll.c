
#ifndef ADARE_NET_EPOLL_TYPES
#define ADARE_NET_EPOLL_TYPES

#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>

short adare_kpoll_filter_read   = EVFILT_READ;
short adare_kpoll_filter_write  = EVFILT_WRITE;

unsigned short adare_kpoll_flag_add     = EV_ADD;
unsigned short adare_kpoll_flag_delete  = EV_DELETE;

unsigned short adare_kpoll_flag_enable  = EV_ENABLE;
unsigned short adare_kpoll_flag_disable = EV_DISABLE;


unsigned short adare_kpoll_flag_error   = EV_ERROR;


int mi_get_kevent (int kq, struct kevent *eventl, int nev, int msec){

    if (msec > 0){
        struct timespec * timeit = NULL;

        if (misec > 1000){
            timeit->tv_sec = misec / 1000;
            timeit->tv_nsec = (misec - (timeit->tv_sec * 1000)) * 1000000;
        } else {
            timeit->tv_sec = 0;
            timeit->tv_nsec = misec * 1000000;
        }
        return kevent (kq, NULL, 0, eventl, nev, timeit);
    }

    return kevent (kq, NULL, 0, eventl, nev, NULL);
}

int mi_set_kevent (int kq, const struct kevent *chglist, int nchg){
    return kevent (kq, chglist, nchg, NULL, 0, NULL);
}

#endif
