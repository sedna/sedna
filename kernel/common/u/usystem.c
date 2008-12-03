/*
 * File:  usystem.c
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/u/usystem.h"
#include "common/errdbg/d_printf.h"

/* returns 0 if succeeded
   returns 1 if failed */
int uUname(U_UTSNAME* s, sys_call_error_fun fun)
{
#ifdef _WIN32
    BOOL res, bIsWow64 = FALSE;
    OSVERSIONINFOEX osvi;
    LPFN_ISWOW64PROCESS fnIsWow64Process;
 
    ZeroMemory(&osvi, sizeof(OSVERSIONINFOEX));

    osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
    
    if((res = GetVersionEx ((OSVERSIONINFO *) &osvi)) == 0)
    {
        sys_call_error("GetVersionEX");
        return 1;
    }

    sprintf(s->sysname, "Windows");
    sprintf(s->release, "%lu.%lu.%lu", osvi.dwMajorVersion, osvi.dwMinorVersion, osvi.dwBuildNumber);
    sprintf(s->version, "SP%u.%u", osvi.wServicePackMajor, osvi.wServicePackMinor);
    
    fnIsWow64Process = (LPFN_ISWOW64PROCESS)GetProcAddress(GetModuleHandle("kernel32"),"IsWow64Process");
 
    if (NULL != fnIsWow64Process)
    {
        if (!fnIsWow64Process(GetCurrentProcess(),&bIsWow64))
        {
            sys_call_error("IsWow64Process");
            return 1;
        }
    }

    sprintf(s->machine, bIsWow64 ? "x64" : "x86");

    return 0;
#else
    int res = 0;
    res = uname(s);
    if(-1 == res)
    {
        sys_call_error("uname");
        return 1;
    }
    return 0;
#endif
}

