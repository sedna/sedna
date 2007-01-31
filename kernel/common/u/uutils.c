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
        return NULL;
    int is_neg = 0;
    if (value < 0)
    {
        value = -value;
        is_neg = 1;
    }
    int i = 1, j = 0;
    int digit = 0, num = 0;
    char buf[20];
    int k = 0;
    do
    {
        j = i;
        i *= 10;
        digit = (value % i - num) / j;
        num = value % i;
        buf[k++] = (char) (digit + '0');
    }
    while (value / i != 0);

    j = 0;
    if (is_neg)
        str[j++] = '-';
    for (i = k - 1; i >= 0; i--, j++)
        str[j] = buf[i];
    str[j] = '\0';
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
    return str;
#endif
}

char *u_gcvt(double value, int digits, char *buf)
{
#ifdef HAVE_GCVT
    return gcvt(value, digits, buf);
#else
    sprintf(buff, "%.*E", digits, value);
    return buff;
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

void net_int2int(__int32 * i, char *buf)
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
    return _strtoi64(nptr, endptr,base);
#else
    return strtoll(nptr, endptr,base);
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

