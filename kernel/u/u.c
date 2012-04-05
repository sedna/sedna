/*
 * File:  u.c
 * Copyright (C) 2005 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifdef _WIN32
#include <io.h>
#endif

#include "u/u.h"
#include "common/errdbg/d_printf.h"

#if !(defined(SE_NO_EVENT_LOG))
#include "common/errdbg/event_log.h"
#endif

static char ustrerror_buf[256];


/*
 * The thread sleeps at least given time in seconds.
 */
void uSleep(unsigned int tm, sys_call_error_fun fun)
{
#ifdef _WIN32
    Sleep((DWORD) tm * 1000);
#else
    sleep(tm);
#endif
}

/*
 * The thread sleeps at least given time in microseconds.
 */
void uSleepMicro(unsigned int tm, sys_call_error_fun fun)
{
#ifdef _WIN32
    Sleep((DWORD) tm / 1000);
#else
    struct timeval	t;

    t.tv_sec = (time_t) (tm / 1000000);
    t.tv_usec = (suseconds_t ) (tm % 1000000);

    select(0, NULL, NULL, NULL, &t);
#endif
}


#if defined(DARWIN)
int u_is_nan(double d)
{
    return isnan(d);
}

bool u_is_neg_inf(double d)
{
     return (isinf(d) && (d) < 0.0);
}

bool u_is_pos_inf(double d)    
{
     return (isinf(d) && (d) > 0.0);
}
#endif

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
   
    memset(buf, '\0', n);
    res = FormatMessage( 
                FORMAT_MESSAGE_FROM_SYSTEM | 
                FORMAT_MESSAGE_IGNORE_INSERTS,
                NULL,
                errnum,
                MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                (LPTSTR)buf,
                (DWORD)(n - 1),
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
	/*	There is actually 2 incompatible strerror_r functions -
		cannot safely rely on return value which is either integer
		(nonzero indicates failure) or char pointer (NULL indicates
		failure).
	*/ 
    memset(buf, '\0', n);
	strncpy(buf, "Failed to obtain error message", n - 1);
    strerror_r(errnum, buf, n); 
    return 0;
#endif
}

int uerrno(const char *funcname, const void* arg)
{
#ifdef _WIN32
#if 0
    if (strcmp(funcname, "WSAStartup") == 0)
    {
        return (arg ? *(int*)arg : 0);
    }
    else if (   strcmp(funcname, "WSACleanup") == 0
             || strcmp(funcname, "socket") == 0
             || strcmp(funcname, "getaddrinfo") == 0
             || strcmp(funcname, "bind") == 0
             || strcmp(funcname, "connect") == 0
             || strcmp(funcname, "setsockopt") == 0
             || strcmp(funcname, "getsockopt") == 0
             || strcmp(funcname, "listen") == 0
             || strcmp(funcname, "accept") == 0
             || strcmp(funcname, "recv") == 0
             || strcmp(funcname, "send") == 0
             || strcmp(funcname, "closesocket") == 0
             || strcmp(funcname, "shutdown") == 0
             || strcmp(funcname, "select") == 0)
    {
        return WSAGetLastError();
    }
    else 
    {
        return GetLastError();
    }
#endif
    /* WSAGetLastError() is an alias for GetLastError() */ 
    return GetLastError();
#else
    return errno;
#endif
}

void uperror(const char *s)
{
#ifdef _WIN32
    char buf[256];
    ustrerror_r(uerrno(s, NULL), buf, 256);
    fprintf(stderr, "%s: %s\n", s, buf);
#else
    perror(s);
#endif
}

void __sys_call_error(const char *filename, int lineno, const char *funcname, const char *sys_call, const void* arg)
{
#if !(defined(SE_NO_EVENT_LOG))
    char buf[256];
    int code = uerrno(funcname, arg);
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

void __sys_call_error_nop(const char *filename, int lineno, const char *funcname, const char *sys_call, const void* arg)
{
}


/* Simply writes message to the event log 
 * Intended to be used inside u-functions to
 * write additional error condition information.
 */
void
 __u_call_error(const char *filename, 
               int lineno, 
               const char *funcname, 
               const char *message)
{
#if !(defined(SE_NO_EVENT_LOG))
    event_log_short_msg(EL_ERROR, 
                        filename, 
                        lineno, 
                        funcname, 
                        message);
#endif /* !SE_NO_EVENT_LOG */

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


int uMakeLowLevelDescriptorNonInheritable(FILE* f, sys_call_error_fun fun)
{
    int fd;

#ifdef _WIN32
    HANDLE hnd;

    fd = _fileno(f);
    if(fd == -1) 
    {
        sys_call_error("_fileno");
        return -1;
    }
    
    hnd = (HANDLE)_get_osfhandle(fd);
    if(hnd == INVALID_HANDLE_VALUE)
    {
        sys_call_error("_get_osfhandle");
        return -1;
    }

    if(SetHandleInformation(hnd, HANDLE_FLAG_INHERIT, 0) == 0)
    {
        sys_call_error("SetHandleInformation");
        return -1;
    }
    return 0;
#else
    fd = fileno(f);
    if(fd == -1) 
    {
        sys_call_error("fileno");
        return -1;
    }

    if (fcntl(fd, F_SETFD, FD_CLOEXEC) == -1)
    {
        sys_call_error("fcntl");
        return -1;
    }
    return 0;
#endif
}

