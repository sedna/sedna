/*
 * File:  uutils.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef UUTILS_H
#define UUTILS_H


#include "common/u/u.h"

#ifndef _WIN32
#include <netinet/in.h>
#include <netinet/tcp.h>
#endif


#ifdef __cplusplus
extern "C"
{
#endif

#ifdef _WIN32
#define u_itoa _itoa
#define u_ltoa _ltoa
#define u_i64toa _i64toa
#define u_ui64toa _ui64toa
#define u_ultoa _ultoa
#define u_gcvt _gcvt
#else
    char *u_itoa(int value, char *str, int radix);
    char *u_ltoa(long value, char *str, int radix);
    char *u_ultoa(unsigned long value, char *str, int radix);
    char *u_i64toa(__int64 value, char *str, int radix);
	char *u_ui64toa(__uint64 value, char *str, int radix);
    char *u_gcvt(double value, int digits, char *buf);
    int _stricmp(const char *str1, const char *str2);
    int _strnicmp(const char *str1, const char *str2, size_t n);
    int _vsnprintf(char *str, size_t size, const char *format, va_list ap);
#define _snprintf snprintf
#endif

    void int2net_int(__int32 i, char *buf);
    void net_int2int(__int32 *i, const char *buf);

    /// buf length must not be less than 20 
    char *int2c_str(int value, char *buf);

    __int64  strto__int64(const char *nptr, char **endptr, int base);
    __uint64 strto__uint64(const char *nptr, char **endptr, int base);

    /// (__int64)INF, (__int64)NaN is undefined behaviour and give different
    /// results on Win/Linux/Mac OS
    /// returns 0 if NaN
    ///         __I64_MAX if +INF, or v > _I64_MAX
    ///         __I64_MIN if -INF, or v < _I64_MIN
    __int64 u_double2int64(double v);

#ifdef __cplusplus
}
#endif


#endif
