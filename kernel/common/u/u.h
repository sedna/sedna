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

#if(defined(FreeBSD) || defined(DARWIN))
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
#if (defined(_WIN32) && defined(__cplusplus))
/* To avod problems with dependencies generation on Cygwin environment */
#include <sstream>
#endif 

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

#ifndef __cygwin__
#include <ucontext.h>
#endif

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
 * signed integers types definition
 */
#ifdef _WIN32
/*
 * Already defined
 */
#else
typedef int8_t                __int8;
typedef int16_t               __int16;
typedef int32_t               __int32;
typedef int64_t               __int64;
#endif


/*
 * unsigned integers types definition
 */
typedef uint8_t                __uint8;
typedef uint16_t               __uint16;
typedef uint32_t               __uint32;
typedef uint64_t               __uint64;

#ifdef _WIN32
/*
 * Already defined
 */

#else
#define _I64_MAX (((__int64)0x7FFFFFFF << 32) | 0xFFFFFFFF)
#define _I64_MIN (-_I64_MAX - 1)
#endif 


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
 * global_name
 *     Interprocess name for IPC resources
 */
typedef const char *global_name;

/*
 * UHANDLE
 *     Handle (identifier) for resources
 */
#ifdef _WIN32
typedef HANDLE UHANDLE;
#else
typedef int UHANDLE;
#endif /* _WIN32 */


/*
 * LARGE_INTEGER
 *     Union for manipulating 64-bit integers as a whole or as 32-bit parts
 */
#ifdef _WIN32
/*
 * Already defined
 */
#else
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
		(((long)(pointer) % (sizeof (type))) == 0)





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
 * U_MAX_DIR   - Maximum length of directory component
 * U_MAX_FNAME - Maximum length of filename component
 */
#ifdef _WIN32
#define U_MAX_PATH          _MAX_PATH
#define U_MAX_FNAME         _MAX_FNAME
#define U_MAX_DIR           _MAX_DIR
#else
#define U_MAX_PATH          PATH_MAX 
#define U_MAX_FNAME         NAME_MAX
#define U_MAX_DIR           NAME_MAX
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



/*
 * StrNCpy
 *	Like standard library function strncpy(), except that result string
 *	is guaranteed to be null-terminated --- that is, at most N-1 bytes
 *	of the source string will be kept.
 *	Also, the macro returns no result (too hard to do that without
 *	evaluating the arguments multiple times, which seems worse).
 *
 *	BTW: when you need to copy a non-null-terminated string (like a text
 *	datum) and add a null, do not do it with StrNCpy(..., len+1).  That
 *	might seem to work, but it fetches one byte more than there is in the
 *	text object.  One fine day you'll have a SIGSEGV because there isn't
 *	another byte before the end of memory.	Don't laugh, we've had real
 *	live bug reports from real live users over exactly this mistake.
 *	Do it honestly with "memcpy(dst,src,len); dst[len] = '\0';", instead.
 */
#define StrNCpy(dst,src,len) \
	do \
	{ \
		char * _dst = (dst); \
		usize_t _len = (len); \
\
		if (_len > 0) \
		{ \
			strncpy(_dst, (src), _len); \
			_dst[_len-1] = '\0'; \
		} \
	} while (0)


/* Get a bit mask of the bits set in non-int32 aligned addresses */
#define INT_ALIGN_MASK (sizeof(__int32) - 1)

/*
 * MemSet
 *	Exactly the same as standard library function memset(), but considerably
 *	faster for zeroing small word-aligned structures (such as parsetree nodes).
 *	This has to be a macro because the main point is to avoid function-call
 *	overhead.	However, we have also found that the loop is faster than
 *	native libc memset() on some platforms, even those with assembler
 *	memset() functions.  More research needs to be done, perhaps with
 *	platform-specific MEMSET_LOOP_LIMIT values or tests in configure.
 *
 *	bjm 2002-10-08
 */
#define MemSet(start, val, len) \
	do \
	{ \
		/* must be void* because we don't know if it is integer aligned yet */ \
		void   *_vstart = (void *) (start); \
		int		_val = (val); \
		usize_t	_len = (len); \
\
		if ((((long) _vstart) & INT_ALIGN_MASK) == 0 && \
			(_len & INT_ALIGN_MASK) == 0 && \
			_val == 0 && \
			_len <= MEMSET_LOOP_LIMIT) \
		{ \
			__int32 *_start = (__int32 *) _vstart; \
			__int32 *_stop = (__int32 *) ((char *) _start + _len); \
			while (_start < _stop) \
				*_start++ = 0; \
		} \
		else \
			memset(_vstart, _val, _len); \
	} while (0)

#define MEMSET_LOOP_LIMIT  1024

/*
 * MemSetAligned is the same as MemSet except it omits the test to see if
 * "start" is word-aligned.  This is okay to use if the caller knows a-priori
 * that the pointer is suitably aligned (typically, because he just got it
 * from palloc(), which always delivers a max-aligned pointer).
 */
#define MemSetAligned(start, val, len) \
	do \
	{ \
		__int32  *_start = (__int32 *) (start); \
		int		_val = (val); \
		usize_t	_len = (len); \
\
		if ((_len & INT_ALIGN_MASK) == 0 && \
			_val == 0 && \
			_len <= MEMSET_LOOP_LIMIT) \
		{ \
			__int32 *_stop = (__int32 *) ((char *) _start + _len); \
			while (_start < _stop) \
				*_start++ = 0; \
		} \
		else \
			memset(_start, _val, _len); \
	} while (0)


/*
 * MemSetTest/MemSetLoop are a variant version that allow all the tests in
 * MemSet to be done at compile time in cases where "val" and "len" are
 * constants *and* we know the "start" pointer must be word-aligned.
 * If MemSetTest succeeds, then it is okay to use MemSetLoop, otherwise use
 * MemSetAligned.  Beware of multiple evaluations of the arguments when using
 * this approach.
 */
#define MemSetTest(val, len) \
	( ((len) & INT_ALIGN_MASK) == 0 && \
	(len) <= MEMSET_LOOP_LIMIT && \
	(val) == 0 )

#define MemSetLoop(start, val, len) \
	do \
	{ \
		__int32 * _start = (__int32 *) (start); \
		__int32 * _stop = (__int32 *) ((char *) _start + (usize_t) (len)); \
	\
		while (_start < _stop) \
			*_start++ = 0; \
	} while (0)


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

#define SYS_CALL_ERROR(FN, SYSCALL_STR) \
    (((FN)?(FN):__sys_call_error_nop) \
    (__FILE__,__LINE__,__SE_FUNCTION__, (SYSCALL_STR), NULL))

#define SYS_CALL_ERROR2(FN, SYSCALL_STR, PARAMS_STR) \
    (((FN)?(FN):__sys_call_error_nop) \
    (__FILE__,__LINE__,__SE_FUNCTION__, (SYSCALL_STR), (PARAMS_STR)))

#define sys_call_error(sys_call)          (fun ? fun(__FILE__, __LINE__, __SE_FUNCTION__, sys_call, NULL) : (void)0)
#define sys_call_error2(sys_call, arg)    (fun ? fun(__FILE__, __LINE__, __SE_FUNCTION__, sys_call, arg) : (void)0)

#define u_call_error(message)             (fun ? __u_call_error(__FILE__, __LINE__, __SE_FUNCTION__, message) : (void)0)

typedef void (*sys_call_error_fun)(const char *filename, int lineno, const char *funcname, const char *sys_call, const void*);

char*  ustrerror(int errnum);
int    ustrerror_r(int errnum, char *buf, size_t n);
void   uperror(const char *s);
void   __sys_call_error(const char *filename, int lineno, const char *funcname, const char *sys_call, const void* arg);
void   __sys_call_error_nop(const char *filename, int lineno, const char *funcname, const char *sys_call, const void* arg);
void   __u_call_error(const char *filename, int lineno, const char *funcname, const char *message);


/*=============================================================================
 *                           Common Functions
 *                           ~~~~~~~~~~~~~~~~
 *===========================================================================*/


void   uSleep(unsigned int secs, sys_call_error_fun fun);
int    uNotInheritDescriptor(UHANDLE h, sys_call_error_fun fun);
int    uMakeLowLevelDescriptorNonInheritable(FILE* f, sys_call_error_fun fun);

#ifdef __cplusplus
}
#endif

#endif /* u.h */
