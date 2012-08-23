/*
 * File:  u.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __U_H
#define __U_H


/*=============================================================================
 *                         Config Section
 *                         ~~~~~~~~~~~~~~
 *===========================================================================*/
#if (defined(__x86_64__) || defined(__ppc64__) || defined(_WIN64) || defined(__LP64__))
#define SEDNA_X64
#elif (defined(__i386__) || defined(_WIN32) || defined(__ppc__))
#define SEDNA_X32
#else
#error "error: cannot determine architecture or it's not supported"
#endif

#if (defined(_WIN32) && !defined(WIN32))
#define WIN32
#define _WIN32_WINNT 0x0400
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

#if (defined(FreeBSD) || defined(DARWIN) || defined(Linux))
#define HAVE_GETCWD
#else
/* don't have getcwd() */
#endif

#if (defined(FreeBSD) || defined(DARWIN))
/* don't have gcvt() */
#else
#define HAVE_GCVT
#endif


#if (defined(DARWIN) || defined(FreeBSD) || defined(LINUX) || defined(__cygwin__))
/* don't have spinlocks */
#else
#define HAVE_SPINLOCKS
#endif

#if defined(LINUX)
#define U_MAP_ANONYMOUS MAP_ANONYMOUS
#elif (defined(DARWIN) || defined(FreeBSD) || defined(SunOS))
#define U_MAP_ANONYMOUS MAP_ANON
#endif

#if (defined(LINUX) ||  defined(SunOS))
#define U_MAP_NORESERVE MAP_NORESERVE
#elif (defined(DARWIN) || defined(FreeBSD))
#define U_MAP_NORESERVE 0
#endif

#if defined(DARWIN) || defined(SunOS)
#define U_MSG_NOSIGNAL 0 //SO_NOSIGNAL can be used only in setsockopt() under Mac OS 10.2 and later
                         //The only way to block SIGPIPE under Solaris to block it with sigignore().
#else
#define U_MSG_NOSIGNAL MSG_NOSIGNAL
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
/* disk sector size should be obtained programmatically but if it could not
   then predefined size (of 512 byts) will be used */
#define PREDEFINED_DISK_SECTOR_SIZE 512
#endif
#endif

#if (defined(__ppc__) || defined(__POWERPC__))
#define BIG_ENDIAN_ORDER
#endif

#if (defined(DARWIN) || defined(FreeBSD) || defined(__cygwin__))
/* don't have /proc */
#else
#define HAVE_PROC
#endif

#if (defined(FreeBSD) || defined(DARWIN))
/* don't have malloc.h */
#else
#define HAVE_MALLOC
#endif


#if (!defined(WIN32) && !defined(__cygwin__))
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


// only for MSDEV 6.0
#if (_MSC_VER == 1200)
#define __SE_FUNCTION__ "<unknown>"
#else
#define __SE_FUNCTION__ __FUNCTION__
#endif

#ifdef _WIN32
#ifdef _MSC_VER
#define TLS_VAR_DECL    __declspec(thread)
#else
#define TLS_VAR_DECL
#endif
#else
#define TLS_VAR_DECL
#endif


// It is better to use some proprietary declaration to not
// depend on external one which can be changed.
// EXTERN_C is defined in winnt.h.
// C_extern is defined in chicken.h
#ifndef SE_EXTERN_C
#ifdef __cplusplus
#define SE_EXTERN_C extern "C"
#else
#define SE_EXTERN_C
#endif
#endif

#define HAVE_STRINGIZE

#define ALIGNOF_SHORT    2
#define ALIGNOF_INT      4
#define ALIGNOF_LONG     4
#define ALIGNOF_DOUBLE   8
#define MAXIMUM_ALIGNOF  4



/*=============================================================================
 *                         System File Includes Section
 *                         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *===========================================================================*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <float.h>
#include <inttypes.h>
#include <limits.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>


#ifdef _WIN32
#define _WINSOCKAPI_
#include <windows.h>
#include <sys/timeb.h>
#else
#include <sys/ipc.h>
#include <sys/file.h>
#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/fcntl.h>
#include <sys/time.h>
#include <unistd.h>
#include <signal.h>

#include <setjmp.h>
#include <fcntl.h>
#include <math.h>
#endif /* _WIN32 */

/// In FreeBSD PAGE_SIZE definition can be injected from the <sys/params.h>
#if defined(FreeBSD) && defined(PAGE_SIZE)
#undef PAGE_SIZE
#endif

/*=============================================================================
 *                             Types
 *                             ~~~~~
 *===========================================================================*/

/*
 * usize_t
 *		Size of any memory resident object, as returned by sizeof
 */
typedef size_t usize_t;


/*
 * bool
 *		Boolean value, either true or false.
 *
 * for C++ compilers, we assume the compiler has a compatible
 * built-in definition of bool
 */
#ifndef __cplusplus

#ifndef bool
typedef char bool;
#endif

#ifndef true
#define true	((bool) 1)
#endif

#ifndef false
#define false	((bool) 0)
#endif
#endif   /* not C++ */

/*
 * UHANDLE
 *     Handle (identifier) for resources
 */
#ifdef _WIN32
typedef HANDLE UHANDLE;
#else
typedef int UHANDLE;
#endif /* _WIN32 */

#ifdef _WIN32
typedef DWORD  UFlag;
#else
typedef int    UFlag;
#endif


/*=============================================================================
 *                       IsValid macros for system types
 *                       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *===========================================================================*/

/*
 * BoolIsValid
 *		True iff bool is valid.
 */
#define BoolIsValid(boolean)	((boolean) == false || (boolean) == true)

/*
 * PointerIsValid
 *		True iff pointer is valid.
 */
#define PointerIsValid(pointer) ((void*)(pointer) != NULL)

/*
 * PointerIsAligned
 *		True iff pointer is properly aligned to point to the given type.
 */
#define PointerIsAligned(pointer, type) \
		(((uintptr_t)(pointer) % (sizeof (type))) == 0)





/*=============================================================================
 *                       Assert, Trap, etc. macros
 *                       ~~~~~~~~~~~~~~~~~~~~~~~~~
 *===========================================================================*/

/*
 * Trap
 *     Traps if the given condition is true
 *
 */
#define Trap(condition, errorType) \
        do { \
            if (condition) \
                se_ExceptionalCondition(CppAsString(condition), (errorType), \
                                        __FILE__, __LINE__); \
        } while (0)

/*
 *	TrapMacro is the same as Trap but it's intended for use in macros:
 *
 *		#define foo(x) (AssertMacro(x != 0) && bar(x))
 *
 *	Isn't CPP fun?
 */
#define TrapMacro(condition, errorType) \
        ((bool) (!(condition) || \
                 (se_ExceptionalCondition(CppAsString(condition), (errorType), \
                                          __FILE__, __LINE__))))

/*
 * Define SE_ASSERT_CHECK if you want assert checking
 */
#define SE_ASSERT_CHECK
/*
 * Define SE_SLEEP_ON_ASSERT if you want a process to hang on on assert macro
 */
#define SE_SLEEP_ON_ASSERT


#if (defined(SE_ASSERT_CHECK) && defined(EL_DEBUG) && (EL_DEBUG == 1))
#define U_ASSERT(condition)          Trap(!(condition), "FailedAssertion")
#define U_ASSERT_MACRO(condition)    ((void)TrapMacro(!(condition), "FailedAssertion"))
#else
#define U_ASSERT(condition)
#define U_ASSERT_MACRO(condition)    ((void)true)
#endif   /* defined(SE_ASSERT_CHECK) && defined(EL_DEBUG) && (EL_DEBUG == 1) */


SE_EXTERN_C
int se_ExceptionalCondition(const char *conditionName, const char *errorType,
                            const char *fileName, int lineNumber);




/*=============================================================================
 *                             Other macros
 *                             ~~~~~~~~~~~~
 *===========================================================================*/

/*
 * min/max macros definition
 */
#define s_min(a, b)  (((a) < (b)) ? (a) : (b))
#define s_max(a, b)  (((a) > (b)) ? (a) : (b))


/*
 * These constants define the maximum length in bytes for the path and
 * for the individual fields within the path.
 * U_MAX_PATH  - Maximum length of full path
 * U_MAX_HOSTNAME - Maximum length of hostname
 */
#ifdef _WIN32
#define U_MAX_PATH          _MAX_PATH
#define U_MAX_HOSTNAME      255
#define U_PATH_DELIMITER    "\\"
#define U_PATH_DELIMITER_C  '\\'
#else
 #ifndef _POSIX_HOSTNAME_MAX
 #define _POSIX_HOSTNAME_MAX 255
 #endif
#define U_MAX_PATH          PATH_MAX
#define U_MAX_HOSTNAME     _POSIX_HOSTNAME_MAX
#define U_PATH_DELIMITER    "/"
#define U_PATH_DELIMITER_C  '/'
#endif /* _WIN32 */

/*
 * Designation of infinite value for some system calls
 */
#ifdef _WIN32
#define U_INFINITE          INFINITE
#else
#define U_INFINITE          INT_MAX
#endif /* _WIN32 */


/*
 * CppAsString
 *		Convert the argument to a string, using the C preprocessor
 * CppConcat
 *		Concatenate two arguments together, using the C preprocessor
 *
 * Note: the standard Autoconf macro AC_C_STRINGIZE actually only checks
 * whether #identifier works, but if we have that we likely have ## too
 */
#if defined(HAVE_STRINGIZE)

#define CppAsString(identifier) #identifier
#define CppConcat(x, y)			x##y
#else

#define CppAsString(identifier) "identifier"

/*
 * CppIdentity -- On Reiser based cpp's this is used to concatenate
 *		two tokens.  That is
 *				CppIdentity(A)B ==> AB
 *		We renamed it to _private_CppIdentity because it should not
 *		be referenced outside this file.  On other cpp's it
 *		produces  A  B.
 */
#define _priv_CppIdentity(x)x
#define CppConcat(x, y)			_priv_CppIdentity(x)y
#endif   /* HAVE_STRINGIZE */

/*
 * Alignment macros: align a length or address appropriately for a given type.
 *
 * NOTE: TYPEALIGN will not work if ALIGNVAL is not a power of 2.
 * That case seems extremely unlikely to occur in practice, however.
 *
 */
#define TYPEALIGN(ALIGNVAL,LEN)  \
	(((long) (LEN) + ((ALIGNVAL) - 1)) & ~((long) ((ALIGNVAL) - 1)))

#define SHORTALIGN(LEN)			TYPEALIGN(ALIGNOF_SHORT, (LEN))
#define INTALIGN(LEN)			TYPEALIGN(ALIGNOF_INT, (LEN))
#define LONGALIGN(LEN)			TYPEALIGN(ALIGNOF_LONG, (LEN))
#define DOUBLEALIGN(LEN)		TYPEALIGN(ALIGNOF_DOUBLE, (LEN))
#define MAXALIGN(LEN)			TYPEALIGN(MAXIMUM_ALIGNOF, (LEN))

/* Get a bit mask of the bits set in non-int32 aligned addresses */
#define INT_ALIGN_MASK (sizeof(int32_t) - 1)

/*
 * NaN, INF and -INF check functions
 * Portability notes:
 * 1. In Darwin isinf/isnan functions are not defined in C++ headers.
 *    At the moment we define C wrappers for them which can give a
 *    little runtime overhead though.
 * 2. In FreeBSD isinf() returns 1 in both cases INF and -INF
 *    so we need to check value itself also.
 */
#ifdef _WIN32
#define u_is_nan(d)         (_isnan(d))
#define u_is_neg_inf(d)     (_fpclass(d) == _FPCLASS_NINF)
#define u_is_pos_inf(d)     (_fpclass(d) == _FPCLASS_PINF)
#else

#if defined(DARWIN)
SE_EXTERN_C int u_is_nan(double d);
#else
#define u_is_nan(d)         (isnan(d))
#endif

#if  defined(FreeBSD)
#define u_is_neg_inf(d)     (isinf(d) && (d) < 0.0)
#define u_is_pos_inf(d)     (isinf(d) && (d) > 0.0)
#elif defined(DARWIN)
SE_EXTERN_C bool u_is_neg_inf(double d);
SE_EXTERN_C bool u_is_pos_inf(double d);
#elif defined(SunOS)
#include <ieeefp.h>
#define u_is_neg_inf(d)     (FP_NINF == fpclass(d))
#define u_is_pos_inf(d)     (FP_PINF == fpclass(d))
#else /* Linux */
#define u_is_neg_inf(d)     (isinf(d) == -1)
#define u_is_pos_inf(d)     (isinf(d) == 1)
#endif
#endif




/*=============================================================================
 *                       u-Calls Errors Handling
 *                       ~~~~~~~~~~~~~~~~~~~~~~~
 * sys_call_error - must be used inside u-function when it perfroms system
 *                  call to write (e.g. using perror()) why it has been failed
 * u_call_error   - must be used inside u-function when it is going to return
 *                  failed status. It allows additional error diagnostic to
 *                  be logged.
 *===========================================================================*/


#ifdef __cplusplus
extern "C" {
#endif

typedef const char *global_name;

#define SYS_CALL_ERROR(FN, SYSCALL_STR) \
    (((FN)?(FN):__sys_call_error_nop) \
    (__FILE__,__LINE__,__SE_FUNCTION__, (SYSCALL_STR), NULL))

#define SYS_CALL_ERROR2(FN, SYSCALL_STR, PARAMS_STR) \
    (((FN)?(FN):__sys_call_error_nop) \
    (__FILE__,__LINE__,__SE_FUNCTION__, (SYSCALL_STR), (PARAMS_STR)))

#define sys_call_error(sys_call)          (fun ? fun(__FILE__, __LINE__, __SE_FUNCTION__, sys_call, NULL) : (void)0)
#define sys_call_error2(sys_call, arg)    (fun ? fun(__FILE__, __LINE__, __SE_FUNCTION__, sys_call, arg) : (void)0)

#define u_call_error(message)             (fun ? fun(__FILE__, __LINE__, __SE_FUNCTION__, NULL, message) : (void)0)

typedef void (*sys_call_error_fun)(const char *filename, int lineno, const char *funcname, const char *sys_call, const void*);

char*  ustrerror(int errnum);
int    ustrerror_r(int errnum, char *buf, size_t n);
int    uerrno(const char *funcname, const void* arg);
void   uperror(const char *s);

void   __sys_call_error_nop(const char *filename, int lineno, const char *funcname, const char *sys_call, const void* arg);


/*=============================================================================
 *                           Common Functions
 *                           ~~~~~~~~~~~~~~~~
 *===========================================================================*/


void   uSleep(unsigned int secs, sys_call_error_fun fun);
void   uSleepMicro(unsigned int tm, sys_call_error_fun fun);
int    uNotInheritDescriptor(UHANDLE h, sys_call_error_fun fun);
int    uMakeLowLevelDescriptorNonInheritable(FILE* f, sys_call_error_fun fun);

#ifdef __cplusplus
}
#endif

#endif /* u.h */
