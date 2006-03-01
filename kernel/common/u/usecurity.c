/*
 * File:  usecurity.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "usecurity.h"

#ifdef _WIN32
#include <Accctrl.h>
#include <aclapi.h>
#include <windows.h>
#else
#endif

#include "d_printf.h"
#include "uhdd.h"

int uCreateSA(int AccessPermissions, USECURITY_ATTRIBUTES ** sa)        /*AccessPermissions - GENERIC_ALL*/
{
#ifdef AUTH_SWITCH
#if (AUTH_SWITCH == 0)          /*if security is off - Security attributes are set by default*/
    {
        *sa = NULL;
        return 0;
    }
# else

#ifdef _WIN32
    DWORD dwRes;
    PACL pACL = NULL;
    PSECURITY_DESCRIPTOR pSD = NULL;
    EXPLICIT_ACCESS ea;
    PSID SID;
    HANDLE hToken;
    PTOKEN_USER puser_info;
    int res;

    *sa = (USECURITY_ATTRIBUTES *) malloc(sizeof(USECURITY_ATTRIBUTES));

    /* Initialize an EXPLICIT_ACCESS structure for an ACE.*/
    /* The ACE will allow Everyone read access to the key.*/
    ZeroMemory(&ea, sizeof(EXPLICIT_ACCESS));
    ea.grfAccessPermissions = AccessPermissions;
    ea.grfAccessMode = SET_ACCESS;
    ea.grfInheritance = NO_INHERITANCE;
    ea.Trustee.TrusteeForm = TRUSTEE_IS_SID;
    ea.Trustee.TrusteeType = TRUSTEE_IS_USER;

    /* get Current User SID*/
    {
        DWORD token_info_size = 0;
        if (!OpenProcessToken(GetCurrentProcess(), TOKEN_READ, &hToken))
        {
            d_printf2("OpenProcessToken Error %u\n", GetLastError());
            return -1;
        }

        /* Call GetTokenInformation to get the buffer size.*/
        if (!GetTokenInformation(hToken, TokenUser, NULL, token_info_size, &token_info_size))
        {
            res = GetLastError();
            if (res != ERROR_INSUFFICIENT_BUFFER)
            {
                d_printf2("GetTokenInformation Error %u\n", res);
                return -1;
            }
        }

        /* Allocate the buffer.*/
        puser_info = (PTOKEN_USER) (malloc(sizeof(char) * token_info_size));

        /* Call GetTokenInformation again to get the user information.*/
        if (!GetTokenInformation(hToken, TokenUser, puser_info, token_info_size, &token_info_size))
        {
            d_printf2("GetTokenInformation Error %u\n", GetLastError());
            return -1;
        }
    }

    ea.Trustee.ptstrName = (LPTSTR) puser_info->User.Sid;

    /* Create a new ACL that contains the new ACEs.*/
    dwRes = SetEntriesInAcl(1, &ea, NULL, &pACL);
    if (ERROR_SUCCESS != dwRes) return -1;

    /* Initialize a security descriptor.  */
    pSD = (PSECURITY_DESCRIPTOR) (malloc(SECURITY_DESCRIPTOR_MIN_LENGTH));
    if (NULL == pSD) return -1;

    if (!InitializeSecurityDescriptor(pSD, SECURITY_DESCRIPTOR_REVISION)) return -1;

    /* Add the ACL to the security descriptor. */
    if (!SetSecurityDescriptorDacl(pSD, TRUE,   
                                   pACL, FALSE))        /* not a default DACL */
        return -1;

    free(puser_info);
    CloseHandle(hToken);

    /* Initialize a security attributes structure.*/
    (*sa)->nLength = sizeof(SECURITY_ATTRIBUTES);
    (*sa)->lpSecurityDescriptor = pSD;
    (*sa)->bInheritHandle = TRUE;

    return 0;
#else /*UNIX*/
    *sa = NULL;
    return 0;
#endif

#endif
#endif
    return 0;
}

int uReleaseSA(USECURITY_ATTRIBUTES * sa)
{
#ifdef AUTH_SWITCH
#if (AUTH_SWITCH == 0)          /*if security is off - Security attributes were not initialized*/
    return 0;
#else
#ifdef _WIN32
    free(sa->lpSecurityDescriptor);
    free(sa);
    return 0;
#else
    return 0;
#endif
#endif
#endif
    return 0;
}

#define MAX_NAME 256

int uIsAdmin(void)
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
            b = FALSE;
        }
        FreeSid(AdministratorsGroup);
    }

    return (b);
#else
    if (geteuid() == 0)
        return 1;
    else
        return 0;
#endif
}
