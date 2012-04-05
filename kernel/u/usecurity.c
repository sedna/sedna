/*
 * File:  usecurity.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "u/usecurity.h"

#ifdef _WIN32
#include <Accctrl.h>
#include <aclapi.h>
#endif

#include "common/errdbg/d_printf.h"
#include "u/uhdd.h"

int uCreateSA(USECURITY_ATTRIBUTES** sa, UAccess_Permissions access_permissions, int inherit_handle, sys_call_error_fun fun)
{
#ifdef _WIN32
    DWORD dwRes;
    PACL pACL = NULL;
    PSECURITY_DESCRIPTOR pSD = NULL;
    EXPLICIT_ACCESS ea;
    HANDLE hToken;
    PTOKEN_USER puser_info;
    DWORD token_info_size = 0;
    int res;

    *sa = (USECURITY_ATTRIBUTES *) malloc(sizeof(USECURITY_ATTRIBUTES));

    /* Initialize an EXPLICIT_ACCESS structure for an ACE. */
    ZeroMemory(&ea, sizeof(EXPLICIT_ACCESS));
    ea.grfAccessPermissions = access_permissions;
    ea.grfAccessMode = SET_ACCESS;
    ea.grfInheritance = NO_INHERITANCE;
    ea.Trustee.TrusteeForm = TRUSTEE_IS_SID;
    ea.Trustee.TrusteeType = TRUSTEE_IS_USER;

    /* get Current User SID */
    if (!OpenProcessToken(GetCurrentProcess(), TOKEN_READ, &hToken))
    {
        sys_call_error("OpenProcessToken");
        return -1;
    }

    /* Call GetTokenInformation to get the buffer size. */
    if (!GetTokenInformation(hToken, TokenUser, NULL, token_info_size, &token_info_size))
    {
        res = GetLastError();
        if (res != ERROR_INSUFFICIENT_BUFFER)
        {
             sys_call_error("GetTokenInformation");
             return -1;
         }
     }

     /* Allocate the buffer.*/
    puser_info = (PTOKEN_USER) (malloc(sizeof(char) * token_info_size));

     /* Call GetTokenInformation again to get the user information. */
    if (!GetTokenInformation(hToken, TokenUser, puser_info, token_info_size, &token_info_size))
    {
         sys_call_error("GetTokenInformation");
         return -1;
    }

    ea.Trustee.ptstrName = (LPTSTR) puser_info->User.Sid;

    /* Create a new ACL that contains the new ACEs.*/
    dwRes = SetEntriesInAcl(1, &ea, NULL, &pACL);
    if (ERROR_SUCCESS != dwRes)
    {
        sys_call_error("SetEntriesInAcl");
        return -1;
    }

    /* Initialize a security descriptor.  */
    pSD = (PSECURITY_DESCRIPTOR) (malloc(SECURITY_DESCRIPTOR_MIN_LENGTH));
    if (NULL == pSD)
        return -1;

    if (!InitializeSecurityDescriptor(pSD, SECURITY_DESCRIPTOR_REVISION))
    {
        sys_call_error("InitializeSecurityDescriptor");
        return -1;
    }

    /* Add the ACL to the security descriptor. */
    if (!SetSecurityDescriptorDacl(pSD, TRUE,   /* bDaclPresent flag   */
                                   pACL, FALSE))        /* not a default DACL */
    {
        sys_call_error("SetSecurityDescriptorDacl");
        return -1;
    }

    free(puser_info);
    if (CloseHandle(hToken) == 0) sys_call_error("CloseHandle");

    /* Initialize a security attributes structure. */
    (*sa)->nLength = sizeof(SECURITY_ATTRIBUTES);
    (*sa)->lpSecurityDescriptor = pSD;
    (*sa)->bInheritHandle = (BOOL)inherit_handle;

    return 0;
#else /*UNIX*/
    *sa = (USECURITY_ATTRIBUTES *) malloc(sizeof(USECURITY_ATTRIBUTES));
    (**sa) = access_permissions;
    return 0;
#endif
}

int uReleaseSA(USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun)
{
#ifdef _WIN32 /* Security is on; WIN */
    free(sa->lpSecurityDescriptor);
    free(sa);
    return 0;
#else         /* Security is on; UNIX */
    free(sa);
    return 0;
#endif
}

int uIsAdmin(sys_call_error_fun fun)
{
#ifdef _WIN32
    /* Open a handle to the access token for the calling process.*/
    BOOL b;
    SID_IDENTIFIER_AUTHORITY NtAuthority = SECURITY_NT_AUTHORITY;
    PSID AdministratorsGroup;
    b = AllocateAndInitializeSid(&NtAuthority, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, &AdministratorsGroup);
    if (b)
    {
        if (!CheckTokenMembership(NULL, AdministratorsGroup, &b))
        {
            b = (int)FALSE;
            sys_call_error("CheckTokenMembership");
        }
        FreeSid(AdministratorsGroup);
    }
    else sys_call_error("AllocateAndInitializeSid");

    return (int)b;
#else
    if (geteuid() == 0)
        return 1;
    else
        return 0;
#endif
}
