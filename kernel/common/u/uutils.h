/*
 * File:  uutils.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef UUTILS_H
#define UUTILS_H


#include "u.h"

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
#define u_ultoa _ultoa
#define u_gcvt _gcvt
#else
    char *u_itoa(int value, char *str, int radix);
    char *u_ltoa(long value, char *str, int radix);
    char *u_ultoa(unsigned long value, char *str, int radix);
    char *u_i64toa(__int64 value, char *str, int radix);
    char *u_gcvt(double value, int digits, char *buf);
    int _stricmp(const char *str1, const char *str2);
    int _strnicmp(const char *str1, const char *str2, size_t n);
    int _vsnprintf(char *str, size_t size, const char *format, va_list ap);
#define _snprintf snprintf
#endif

    void int2net_int(__int32 i, char *buf);
    void net_int2int(__int32 *i, char *buf);

    /* buf length must not be less than 20 */
    char *int2c_str(int value, char *buf);


#ifdef __cplusplus
}
#endif


#endif
