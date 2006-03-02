/*
 * File:  u.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __U_H
#define __U_H

#if (defined(_WIN32) && !defined(WIN32))
#define WIN32
#endif

#if (defined(__APPLE__) && defined(__MACH__))
#define DARWIN
#endif

#if (defined(__linux__))
#define LINUX
#endif

#if (defined(__FreeBSD__))
#define FreeBSD
#endif

#if (defined(__sun__))
#define SunOS
#endif



#if (defined(FreeBSD))
/* don't have gcvt() */
#else
#define HAVE_GCVT
#endif


#if (defined(DARWIN) || defined(FreeBSD))
/* don't have spinlocks */
#else
#define HAVE_SPINLOCKS
#endif


#if (defined(DARWIN) || defined(FreeBSD))
#define __MSG_NOSIGNAL 0        /*SO_NOSIGPIPE */
#else
#define __MSG_NOSIGNAL MSG_NOSIGNAL
#endif

#if (defined(DARWIN) || defined(FreeBSD))
#define HAVE_DEFAULT_LARGEFILE_FUNCTIONS
#else
/* use system calls with suffix '64' */
#endif


#if (defined(DARWIN))
#define PREDEFINED_DISK_SECTOR_SIZE DEV_BSIZE
#else
#if (defined(FreeBSD) || defined(SunOS)) /*??? for SunOS*/
#define PREDEFINED_DISK_SECTOR_SIZE 512
#else
/* disk sector size should be obtained programmatically */
#endif
#endif


#if (defined(DARWIN) || defined(FreeBSD))
/* don't have /proc */
#else
#define HAVE_PROC
#endif


#if (!defined(WIN32))
#if (defined(DARWIN) || defined(FreeBSD))
/* don't have /proc/?/exe */
#else
#define HAVE_PROC_EXE
#if (defined(LINUX))
#define PROC_EXE_SUFFIX "/exe"
#elif (defined(SunOS))
#define PROC_EXE_SUFFIX "/path/a.out"
#else
#error Define PROC_EXE_SUFFIX for your platform
#endif
#endif
#endif


typedef unsigned int uint32;

#define s_min(a, b)  (((a) < (b)) ? (a) : (b))
#define s_max(a, b)  (((a) > (b)) ? (a) : (b))


#ifdef _WIN32

#include <windows.h>
#include <float.h>
typedef const char *global_name;

#define U_INFINITE			INFINITE

typedef HANDLE UHANDLE;

#define U_MAX_PATH			_MAX_PATH
#define U_MAX_FNAME         _MAX_FNAME
#define U_MAX_DIR           _MAX_DIR


#else

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <ucontext.h>
#include <setjmp.h>
#include <limits.h>
#include <float.h>
#include <stdarg.h>


typedef key_t global_name;

#define U_INFINITE			INT_MAX

typedef int UHANDLE;

#define U_MAX_PATH			PATH_MAX
#define U_MAX_FNAME         NAME_MAX
#define U_MAX_DIR           NAME_MAX


typedef long long int __int64;
typedef int __int32;
typedef short int __int16;
typedef char __int8;



typedef union _LARGE_INTEGER
{
    struct
    {
        unsigned long LowPart;
        long HighPart;
    };
    struct
    {
        unsigned long LowPart;
        long HighPart;
    } u;
    __int64 QuadPart;
} LARGE_INTEGER;

#endif



/*/ Asserts*/
#ifdef _WIN32
#include <crtdbg.h>
#define U_ASSERT _ASSERT
#else
#define U_ASSERT(x)
#endif


char* ustrerror(int errnum);
void uperror(const char *s);

#ifdef __cplusplus
extern "C"
#endif
int uNotInheritDescriptor(UHANDLE h);

#endif
