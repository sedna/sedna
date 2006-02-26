/*
 * File:  usecurity.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __USECURITY_H
#define __USECURITY_H

#include <stdlib.h>

#ifdef _WIN32
#include <windows.h>
#include <Accctrl.h>
#include <aclapi.h>
#endif


#include "u.h"

#define REQUIRE_ROOT


#ifdef _WIN32
typedef SECURITY_ATTRIBUTES USECURITY_ATTRIBUTES;
/*union USECURITY_ATTRIBUTES {NULL; SECURITY_ATTRIBUTES};*/
typedef ACL UACL;
typedef PSID UPSID;
#else
typedef int USECURITY_ATTRIBUTES;
typedef int UACL;
typedef int UPSID;
#endif

#ifdef __cplusplus
extern "C"
{
#endif

    int uCreateSA(int AccessPermissions, USECURITY_ATTRIBUTES ** sa);

    int uReleaseSA(USECURITY_ATTRIBUTES * sa);

/* returns true if */
/*   Windows: current user is in the Administrators Group*/
/*   Unix: current user is root*/
/* returns false otherwise (including errors)*/
    int uIsAdmin(void);

#ifdef __cplusplus
}
#endif

#endif
