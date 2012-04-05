/*
 * File:  usecurity.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __USECURITY_H
#define __USECURITY_H

#ifdef _WIN32
#include <Accctrl.h>
#include <aclapi.h>
#endif


#include "u/u.h"

//#define REQUIRE_ROOT

#ifdef _WIN32
#define U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK             GENERIC_ALL
#define U_SEDNA_DIRECTORY_ACCESS_PERMISSIONS_MASK           GENERIC_ALL
#define U_SEDNA_SHMEM_ACCESS_PERMISSIONS_MASK               GENERIC_ALL
#define U_SEDNA_SEMAPHORE_ACCESS_PERMISSIONS_MASK           GENERIC_ALL
typedef SECURITY_ATTRIBUTES USECURITY_ATTRIBUTES;
typedef ACL UACL;
typedef PSID UPSID;
typedef DWORD UAccess_Permissions;
#else
#define U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK             S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP  /* 00660 */
#define U_SEDNA_SHMEM_ACCESS_PERMISSIONS_MASK               00666
#define U_SEDNA_DIRECTORY_ACCESS_PERMISSIONS_MASK           00755
#define U_SEDNA_SEMAPHORE_ACCESS_PERMISSIONS_MASK           00777
typedef mode_t USECURITY_ATTRIBUTES;
typedef mode_t UAccess_Permissions;
#endif

#ifdef __cplusplus
extern "C" {
#endif

    int uCreateSA(USECURITY_ATTRIBUTES** sa, UAccess_Permissions access_permissions, int inherit_handle, sys_call_error_fun fun);

    int uReleaseSA(USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun);

/* returns true if 
   Windows: current user is in the Administrators Group
   Unix: current user is root
   returns false otherwise (including errors) */
    int uIsAdmin(sys_call_error_fun fun);

#ifdef __cplusplus
}
#endif

#endif
