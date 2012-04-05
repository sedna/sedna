/*
 * File:  usystem.h
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _USYSTEM_H
#define _USYSTEM_H

#include "u/u.h"


#ifdef _WIN32

typedef struct {
    char sysname[128];
    char release[128];
    char version[128];
    char machine[128];
} U_UTSNAME;

#else

#include <sys/utsname.h>
typedef struct utsname U_UTSNAME;

#endif /* _WIN32 */


#ifdef __cplusplus
extern "C"
{
#endif

/* returns 0 if succeeded
   returns 1 if failed */
    int uUname(U_UTSNAME* s, sys_call_error_fun fun);


#ifdef __cplusplus
}
#endif


#endif /* _USYSTEM_H */
