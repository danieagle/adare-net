
#ifndef ADARE_NET_POLL_TYPES
#define ADARE_NET_POLL_TYPES

#include <poll.h>

const short adare_poll_filter_read   = POLLIN;
const short adare_poll_filter_write  = POLLOUT;

const short adare_poll_filter_error  = POLLERR;

short mi_and (short left, short right){
    return (left & right);
}

#endif
