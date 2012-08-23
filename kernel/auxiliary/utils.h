/*
 * File:  utils.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __UTILS_H
#define __UTILS_H

#include <string>
#include <vector>

#include "common/sedna.h"

/* datatypes and functions to with time (time in sec + time in millisec)*/
#ifndef _WIN32
struct _timeb
{
    long time;
    long millitm;
    int timezone;
    int dstflag;
};
#endif

typedef _timeb u_timeb;

#ifdef _WIN32
inline void u_ftime(u_timeb *t) {_ftime((t)); }
#else
inline void u_ftime(u_timeb *t)
{
    struct timeval tv;
    struct timezone tz;
    /* ftime() is obsolete in FreeBSD 6.2 and higher */
    gettimeofday(&tv, &tz);    
    t->time     = tv.tv_sec;
    t->millitm  = tv.tv_usec/1000;
    t->dstflag  = tz.tz_dsttime;
    t->timezone = tz.tz_minuteswest; 
}
#endif /* _WIN32 */


inline void u_timeb_init(u_timeb *t)
{
    t->time = 0;
    t->millitm = 0;
    t->timezone = 0;
    t->dstflag = 0;
}

inline u_timeb operator- (const u_timeb& t1, const u_timeb& t2)
{
    u_timeb t;
    if (t2.millitm > t1.millitm) {
        t.time = t1.time - t2.time - 1;
        t.millitm = 1000 - t2.millitm + t1.millitm;
    }
    else  {
        t.time = t1.time - t2.time;
        t.millitm = t1.millitm - t2.millitm;
    }
    t.dstflag = t1.dstflag;
    t.timezone = t1.timezone;
    return t;
}

inline u_timeb operator+ (const u_timeb& t1, const u_timeb& t2)
{
    u_timeb t;
    t.millitm  = t1.millitm + t2.millitm;
    t.time     = t1.time + t2.time;
    if(t.millitm > 1000) { t.time++; t.millitm-=1000; }
    t.dstflag = t1.dstflag;
    t.timezone = t1.timezone;
    return t;
}

std::string to_string(u_timeb t);

double power(double a, double b);

#ifndef _WIN32
int _isnan(double x);
#endif

/*
template<class T>
std::vector<T> vector_concat(const std::vector<T>& v1, const std::vector<T> &v2)
{
    std::vector<T> res;

    res.reserve(v1.size() + v2.size());

    typename std::vector<T>::const_iterator it;
    for (it = v1.begin(); it != v1.end(); it++) res.push_back(*it);
    for (it = v2.begin(); it != v2.end(); it++) res.push_back(*it);

    return res;
}
*/

inline static bool implies(bool a, bool b) { return !a || b; }

/* function is used for eliminating disturbance from sorted array*/
/* parameters are like parameters for qsort*/
#ifdef _WIN32
typedef int (__cdecl *compare_fun)(const void* elem1, const void* elem2);
#else
typedef int (*compare_fun)(const void* elem1, const void* elem2);
#endif
void elim_disturb(void *base, size_t num, size_t width, compare_fun compare);
void elim_disturb2(void *base, size_t num, size_t width, compare_fun compare);

std::string trim(const std::string& str);

/// Intented to be used with strcmp and memcmp
///     res = memcmp(ptr1, ptr2)
///     return sign(res)
/// In such a way we will guarantee -1 and 1 values.
inline int sign(int i)
{
    if (i > 0) return 1;
    if (i < 0) return -1;
    return i;
}

#endif /* __UTILS_H */
