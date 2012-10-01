#ifndef _TIMER_H_
#define _TIMER_H_

#include "u/utime.h"

class Timer
{
    ex_time_t t_0;
    ex_time_t t_last;
    ex_time_t t_diff;
public:
    void start()
    {
        t_last = uGetTime();
        t_0 = t_last;
    };

    ex_time_t split()
    {
        ex_time_t tx = uGetTime();
        t_diff = tx - t_last;
        t_last = uGetTime();
    };
};

#endif /* _TIMER_H_ */
