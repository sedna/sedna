/*
 * File:  u.c
 * Copyright (C) 2005 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "u.h"
#include "d_printf.h"

#if !(defined(SE_NO_EVENT_LOG))
#include "event_log.h"
#endif


static char ustrerror_buf[256];


void uSleep(unsigned int secs, sys_call_error_fun fun)
{
#ifdef _WIN32
    Sleep(secs * 1000);
#else
    sleep(secs);
#endif
}

/* ustrerror is not thread safe */
char* ustrerror(int errnum)
{
#ifdef _WIN32
    DWORD res = 0;
   
    res = FormatMessage( 
                FORMAT_MESSAGE_FROM_SYSTEM | 
                FORMAT_MESSAGE_IGNORE_INSERTS,
                NULL,
                errnum,
                MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                (LPTSTR)ustrerror_buf,
                255,
                NULL);

    if (!res) 
        sprintf(ustrerror_buf, "unrecognized error code (%d)", errnum);

    return ustrerror_buf;
#else
    return strerror(errnum);
#endif
}

int ustrerror_r(int errnum, char *buf, size_t n)
{
#ifdef _WIN32
    DWORD res = 0;
   
    res = FormatMessage( 
                FORMAT_MESSAGE_FROM_SYSTEM | 
                FORMAT_MESSAGE_IGNORE_INSERTS,
                NULL,
                errnum,
                MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                (LPTSTR)buf,
                n - 1,
                NULL);

    if (!res) 
        sprintf(buf, "unrecognized error code (%d)", errnum);
    else
    {
        if ((res > 1) && (buf[res - 1] == (char)10) && (buf[res - 2] == (char)13))
            buf[res - 1] = buf[res - 2] = '\0';
    }

    return 0;
#else
    return strerror_r(errnum, buf, n);
#endif
}

void uperror(const char *s)
{
#ifdef _WIN32
    char buf[256];
    ustrerror_r(GetLastError(), buf, 256);
    fprintf(stderr, "%s: %s\n", s, buf);
#else
    perror(s);
#endif
}


void __sys_call_error(const char *filename, int lineno, const char *funcname, const char *sys_call)
{
#if !(defined(SE_NO_EVENT_LOG))
    char buf[256];
#ifdef _WIN32
    int code = GetLastError();
#else
    int code = errno;
#endif
#endif

    d_perror(sys_call);

#if !(defined(SE_NO_EVENT_LOG))
    ustrerror_r(code, buf, 256);
    event_log_short_msg(EL_SYS, 
                        filename, 
                        lineno, 
                        funcname, 
                        "%s (code = %d): %s", 
                        sys_call, 
                        code, 
                        buf);
#endif
}

int uNotInheritDescriptor(UHANDLE h, sys_call_error_fun fun)
{
#ifdef _WIN32
    if (SetHandleInformation(h, HANDLE_FLAG_INHERIT, 0) == 0)
    {
        sys_call_error("SetHandleInformation");
        return -1;
    }
    else
        return 0;
#else

    if (fcntl(h, F_SETFD, FD_CLOEXEC) == -1)
    {
        sys_call_error("fcntl");
        return -1;
    }
    else
        return 0;
#endif
}
