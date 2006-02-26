/*
 * File:  uutils.c
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <stdlib.h>
#include "uutils.h"
#include "d_printf.h"


#ifndef _WIN32

char *itoa(int value, char *str, int radix)
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

char *_ltoa(long value, char *str, int radix)
{
    if (radix != 10)
    {
        d_printf1("radix in call to _ltoa has unsupported value\n");
        return NULL;
    }

    sprintf(str, "%ld", value);
    return str;
}

char *_ultoa(unsigned long value, char *str, int radix)
{
    if (radix != 10)
    {
        d_printf1("radix in call to _ultoa has unsupported value\n");
        return NULL;
    }

    sprintf(str, "%lu", value);
    return str;
}

char *_gcvt(double value, int digits, char *buf)
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
    itoa(value, buf, 10);
    return buf;
}
