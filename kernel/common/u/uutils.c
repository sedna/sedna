/*
 * File:  uutils.c
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <stdlib.h>
#include "common/u/uutils.h"
#include "common/errdbg/d_printf.h"


#ifndef _WIN32

char *u_itoa(int value, char *str, int radix)
{
    if (radix != 10)
    {
        d_printf1("radix in call to _ultoa has unsupported value\n");
        return NULL;
    }

    sprintf(str, "%d", value);
    return str;
}

char *u_ltoa(long value, char *str, int radix)
{
    if (radix != 10)
    {
        d_printf1("radix in call to _ltoa has unsupported value\n");
        return NULL;
    }

    sprintf(str, "%ld", value);
    return str;
}

char *u_ultoa(unsigned long value, char *str, int radix)
{
    if (radix != 10)
    {
        d_printf1("radix in call to _ultoa has unsupported value\n");
        return NULL;
    }

    sprintf(str, "%lu", value);
    return str;
}

char *u_i64toa(__int64 value, char *str, int radix)
{
#ifdef _WIN32
    _i64toa(value, str, radix);
#else
    if (radix != 10)
    {
        d_printf1("radix in call to _ultoa has unsupported value\n");
        return NULL;
    }

    sprintf(str, "%lld", value);
#endif
	return str;
}

char *u_ui64toa(__uint64 value, char *str, int radix)
{
#ifdef _WIN32
    _ui64toa(value, str, radix);
#else
    if (radix != 10)
    {
        d_printf1("radix in call to _ui64toa has unsupported value\n");
        return NULL;
    }

    sprintf(str, "%llu", value);
    return str;
#endif
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


#endif

void int2net_int(__int32 i, char *buf)
{
    i = htonl(i);
    memcpy(buf, (void*)&i, sizeof(__int32));
}

void net_int2int(__int32* i, const char *buf)
{
    memcpy((void*)i, buf, sizeof(__int32));
    *i = ntohl(*i);
}

char *int2c_str(int value, char *buf)
{
    u_itoa(value, buf, 10);
    return buf;
}

__int64  strto__int64(const char *nptr, char **endptr, int base)
{
#ifdef _WIN32
    return _strtoi64(nptr, endptr, base);
#else
    return strtoll(nptr, endptr, base);
#endif
}

__uint64 strto__uint64(const char *nptr, char **endptr, int base)
{
#ifdef _WIN32
    return _strtoui64(nptr, endptr,base);
#else
    return strtoull(nptr, endptr,base);
#endif
}

__int64 u_double2int64(double v)
{
    __int64 res;
    if(u_is_nan(v)) return 0;
    if(u_is_neg_inf(v) || v < _I64_MIN) return _I64_MIN;
    if(u_is_pos_inf(v) || v > _I64_MAX) return _I64_MAX;
    res = (__int64)v;
    return res;
}
