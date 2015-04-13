/*
 * File:  utime.c
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _UTIME_H
#define _UTIME_H

#include "u/utime.h"

#ifndef _WIN32
#include <time.h> 
#endif

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#endif

ex_time_t uGetTime()
{
#ifdef _WIN32
    U_ASSERT(false);
#else
    struct timespec ts;
#ifdef __MACH__ // OS X does not have clock_gettime, so using clock_get_time
    clock_serv_t cclock;
    mach_timespec_t mts;
    host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
    clock_get_time(cclock, &mts);
    mach_port_deallocate(mach_task_self(), cclock);
    ts.tv_sec = mts.tv_sec;
    ts.tv_nsec = mts.tv_nsec;
#else
    clock_gettime(CLOCK_REALTIME, &ts);
#endif
    return (ts.tv_sec + ts.tv_nsec / 1000000) / (60*60*24);
#endif
}

utm getLocalTime()
{
    utm retval;

//FIXME: Need to deal with daylight savings times
#ifdef _WIN32
    SYSTEMTIME systime;
    TIME_ZONE_INFORMATION tz_info;

    GetLocalTime(&systime);
    retval.utm_year = systime.wYear;
    retval.utm_mon = systime.wMonth;
    retval.utm_wday = systime.wDayOfWeek;
    retval.utm_mday = systime.wDay;
    retval.utm_hour = systime.wHour;
    retval.utm_min = systime.wMinute;
    retval.utm_sec = systime.wSecond;
    retval.utm_millis = systime.wMilliseconds;

    GetTimeZoneInformation(&tz_info);
    retval.utm_gmtoff = -(tz_info.Bias + tz_info.DaylightBias)*60;
#else
    // !!! Calling localtime is not thread safe (use localtime_r 
    //     instead if you want thread safe code)
    time_t t;
    struct tm* tm_ptr;
    time(&t);
    tm_ptr = localtime(&t); 
    retval.utm_millis = 0;
    retval.utm_sec = tm_ptr->tm_sec;
    retval.utm_min = tm_ptr->tm_min;
    retval.utm_hour = tm_ptr->tm_hour;
    retval.utm_mday = tm_ptr->tm_mday;
    retval.utm_mon = tm_ptr->tm_mon+1;
    retval.utm_year = tm_ptr->tm_year+1900;
    retval.utm_wday = tm_ptr->tm_wday;
    retval.utm_yday = tm_ptr->tm_yday;
    retval.utm_isdst = tm_ptr->tm_isdst;

    // TODO: make it via config.h
#if defined(__cygwin__) || defined(SunOS) 
    retval.utm_zone = 0;
    retval.utm_gmtoff = 0;
#else
    retval.utm_zone = tm_ptr->tm_zone;
    retval.utm_gmtoff = tm_ptr->tm_gmtoff;
#endif

#endif

    return retval;
}

#endif /* _UTIME_H */