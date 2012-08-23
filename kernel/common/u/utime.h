/*
 * File:  utime.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef UTIME_H
#define UTIME_H

#include "common/u/u.h"


// Define the time structure class in the spirit of extended POSIX tm
// structure
typedef struct {
   /* ANSI standard fields */
   int utm_millis;   /* 0 to 1000 */
   int utm_sec;   /* 0 to 60 */
   int utm_min;   /* 0 to 59 */
   int utm_hour;  /* 0 to 23 */
   int utm_mday;  /* 1 to 31 */
   int utm_mon;   /* 0 to 11 */
   int utm_year;  /* year - 1900 */
   int utm_wday;  /* Sunday = 0 */
   int utm_yday;  /* 0 to 365 */
   int utm_isdst;
       /* >0 if Daylight Savings Time,
        *  0 if Standard,
        * <0 if unknown */
   /* extensions to ANSI standard */
   const char *utm_zone;  /* time zone name    */
   long utm_gmtoff; /* offset from GMT   */
} utm;


SE_EXTERN_C
utm getLocalTime();

typedef double ex_time_t;

SE_EXTERN_C
ex_time_t uGetTime();

#endif /* UTIME_H */

