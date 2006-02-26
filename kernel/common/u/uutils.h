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

#ifndef _WIN32
    char *itoa(int value, char *str, int radix);
    char *_ltoa(long value, char *str, int radix);
    char *_ultoa(unsigned long value, char *str, int radix);
    char *_gcvt(double value, int digits, char *buf);
    int _stricmp(const char *str1, const char *str2);
    int _strnicmp(const char *str1, const char *str2, size_t n);
#endif

    void int2net_int(__int32 i, char *buf);
    void net_int2int(__int32 * i, char *buf);

/*buf length must not less than 20*/
    char *int2c_str(int value, char *buf);


#ifdef __cplusplus
}
#endif


#endif
