/*
 * File:  uutils.c
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <stdlib.h>
#include "u/uutils.h"
#include "common/errdbg/d_printf.h"


#ifndef _WIN32

char *u_itoa(int value, char *str, int radix)
{
    U_ASSERT(radix == 10);
    sprintf(str, "%d", value);
    return str;
}

char *u_ltoa(long value, char *str, int radix)
{
    U_ASSERT(radix == 10);
    sprintf(str, "%ld", value);
    return str;
}

char *u_ultoa(unsigned long value, char *str, int radix)
{
    U_ASSERT(radix == 10);
    sprintf(str, "%lu", value);
    return str;
}

char *u_i64toa(int64_t value, char *str, int radix)
{
    U_ASSERT(radix == 10);
    sprintf(str, "%"PRId64, value);
    return str;
}

char *u_ui64toa(uint64_t value, char *str, int radix)
{
    U_ASSERT(radix == 10);
    sprintf(str, "%"PRIu64, value);
    return str;
}

char *u_gcvt(double value, int digits, char *buf)
{
#ifdef HAVE_GCVT
    return gcvt(value, digits, buf);
#else
    sprintf(buf, "%.*E", digits, value);
    return buf;
#endif
}

int _stricmp(const char *str1, const char *str2)
{
    return strcasecmp(str1, str2);
}

int _strnicmp(const char *str1, const char *str2, size_t n)
{
    return strncasecmp(str1, str2, n);
}

int _vsnprintf(char *str, size_t size, const char *format, va_list ap)
{
    int res = vsnprintf(str, size, format, ap);
    if (res >= size) return -1;
    return res;
}

#endif /* _WIN32 */

void int2net_int(uint32_t i, char *buf)
{
    i = htonl(i);
    memcpy(buf, (void*)&i, sizeof(uint32_t));
}

void net_int2int(void * i, const char *buf)
{
    memcpy((void*)i, buf, sizeof(uint32_t));
    * (uint32_t*) i = ntohl(* (uint32_t*) i);
}

char *int2c_str(int value, char *buf)
{
    u_itoa(value, buf, 10);
    return buf;
}

int64_t  strto__int64(const char *nptr, char **endptr, int base)
{
#ifdef _WIN32
    return _strtoi64(nptr, endptr, base);
#else
    return strtoll(nptr, endptr, base);
#endif
}

uint64_t strto__uint64(const char *nptr, char **endptr, int base)
{
#ifdef _WIN32
    return _strtoui64(nptr, endptr,base);
#else
    return strtoull(nptr, endptr,base);
#endif
}

int64_t u_double2int64(double v)
{
    int64_t res;
    if(u_is_nan(v)) return 0;
    if(u_is_neg_inf(v) || v < INT64_MIN) return INT64_MIN;
    if(u_is_pos_inf(v) || v > INT64_MAX) return INT64_MAX;
    res = (int64_t)v;
    return res;
}

