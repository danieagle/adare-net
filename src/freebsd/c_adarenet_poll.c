
#ifndef ADARE_NET_POLL_TYPES
#define ADARE_NET_POLL_TYPES

#include <poll.h>

short adare_poll_filter_read   = POLLIN;
short adare_poll_filter_write  = POLLOUT;

short adare_poll_filter_error  = POLLERR;

short mi_and (short left, short right){
    return (left & right);
}

#endif
