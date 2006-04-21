/*
 * File:  u.c
 * Copyright (C) 2005 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "u.h"
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

/* FIXME: it is not thread safe */
char* ustrerror(int errnum)
{
#ifdef _WIN32
    DWORD res = 0;
    DWORD code = GetLastError();
   
    res = FormatMessage( 
                FORMAT_MESSAGE_FROM_SYSTEM | 
                FORMAT_MESSAGE_IGNORE_INSERTS,
                NULL,
                code,
                MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                (LPTSTR)ustrerror_buf,
                255,
                NULL);

    if (!res) 
        sprintf(ustrerror_buf, "unrecognized error code (%d)", code);

    return ustrerror_buf;
#else
    return strerror(errnum);
#endif
}

void uperror(const char *s)
{
#ifdef _WIN32
    fprintf(stderr, "%s: %s\n", s, ustrerror(errno));
#else
    perror(s);
#endif
}

int uNotInheritDescriptor(UHANDLE h)
{
#ifdef _WIN32
    if (SetHandleInformation(h, HANDLE_FLAG_INHERIT, 0) == 0)
        return -1;
    else
        return 0;
#else

    if (fcntl(h, F_SETFD, FD_CLOEXEC) == -1)
    {
        d_perror("fcntl (1)");
        return -1;
    }
    else
        return 0;
#endif
}
