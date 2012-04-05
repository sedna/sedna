/*
 * File:  usystem.c
 * Copyright (C) 2008 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include "u/usystem.h"
#include "common/errdbg/d_printf.h"

/* Some definitions we need on Windows */
#ifdef _WIN32
typedef void (WINAPI *LPFN_GETNATIVESYSTEMINFO) (LPSYSTEM_INFO);
#endif

/* 
 * Collects system information into U_UTSNAME structure, including:
 * OS type, OS version, architecture type - x86, x64, etc.
 *     returns 0 if succeeded
 *     returns 1 if failed 
 */
int uUname(U_UTSNAME* s, sys_call_error_fun fun)
{
#ifdef _WIN32
    BOOL res;
    OSVERSIONINFOEX osvi;
    SYSTEM_INFO si;
    LPFN_GETNATIVESYSTEMINFO fnGetNativeSystemInfo;
 
    ZeroMemory(&si, sizeof(SYSTEM_INFO));
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
    
    fnGetNativeSystemInfo = (LPFN_GETNATIVESYSTEMINFO)GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")), "GetNativeSystemInfo");

    /* Call GetNativeSystemInfo if supported or GetSystemInfo otherwise. */
    if(NULL != fnGetNativeSystemInfo)
        fnGetNativeSystemInfo(&si);
    else
        GetSystemInfo(&si);
        
    if(si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_IA64)
        sprintf(s->machine, "IA64");
    else if(si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64)
        sprintf(s->machine, "x64");
    else if(si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_INTEL)
        sprintf(s->machine, "x86");
    else 
        sprintf(s->machine, "UNKNOWN");

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
