/*
 * File:  u.c
 * Copyright (C) 2005 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "u.h"
/*#include "event_log.h"*/
#include "d_printf.h"


static char ustrerror_buf[256];


void uSleep(unsigned int secs)
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

void sys_call_error(const char *sys_call)
{
/*
    char buf[256];
#ifdef _WIN32
    int code = GetLastError();
#else
    int code = errno;
#endif
*/
    d_perror(sys_call);
/*
    ustrerror_r(code, buf, 256);
    elog(EL_SYS, ("%s (code = %d): %s", sys_call, code, buf));
*/
}

int uNotInheritDescriptor(UHANDLE h)
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
