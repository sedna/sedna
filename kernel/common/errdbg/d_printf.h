/*
 * File:  d_printf.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _D_PRINTF_H
#define _D_PRINTF_H

//#define EL_DEBUG_SYNC

#include "common/sedna.h"

#ifdef EL_DEBUG
#  if (EL_DEBUG == 1)
#    define D_PRINTF(ARGS)                      el_debug_printf ARGS
#    define d_printf1(s1)						el_debug_printf(s1)
#    define d_printf2(s1, s2)					el_debug_printf(s1, s2)
#    define d_printf3(s1, s2, s3)				el_debug_printf(s1, s2, s3)
#    define d_printf4(s1, s2, s3, s4)			el_debug_printf(s1, s2, s3, s4)
#    define d_printf5(s1, s2, s3, s4, s5)		el_debug_printf(s1, s2, s3, s4, s5)
#    define d_printf6(s1, s2, s3, s4, s5, s6)	el_debug_printf(s1, s2, s3, s4, s5, s6)
#    define d_perror(s)							el_debug_perror(s)
#    define d_flush()							el_debug_flush()
#    define ASSERT								U_ASSERT
#  else
#    define D_PRINTF(ARGS)                       (void)0
#    define d_printf1(s1)						0
#    define d_printf2(s1, s2)					0
#    define d_printf3(s1, s2, s3)				0
#    define d_printf4(s1, s2, s3, s4)			0
#    define d_printf5(s1, s2, s3, s4, s5)		0
#    define d_printf6(s1, s2, s3, s4, s5, s6)	0
#    define d_perror(s)							0
#    define d_flush()							0
#    define ASSERT(x)							0
#  endif
#else
#  define D_PRINTF(ARGS)                         (void)0
#  define d_printf1(s1)							0
#  define d_printf2(s1, s2)						0
#  define d_printf3(s1, s2, s3)					0
#  define d_printf4(s1, s2, s3, s4)				0
#  define d_printf5(s1, s2, s3, s4, s5)			0
#  define d_printf6(s1, s2, s3, s4, s5, s6)		0
#  define d_perror(s)							0
#  define d_flush()								0
#  define ASSERT(x)								0
#endif

#ifdef DEBUG_TRACE
#  define U_TRACE(P) do { el_debug_printf("[%s:%s:%d]", __FILE__, __FUNCTION__, __LINE__); el_debug_printf P;} while (0)
#else
#  define U_TRACE(P)
#endif

#ifdef __cplusplus
extern "C"
{
#endif


    int el_debug_printf(const char *s, ...);
    void el_debug_perror(const char *s);
    void el_debug_flush();


#ifdef EL_DEBUG_SYNC

#define SEDNA_EL_DEBUG_SEMAPHORE_NAME					"SEDNA_EL_DEBUG_SEMAPHORE_NAME"
#define SEDNA_EL_DEBUG_FILE_NAME						"se_debug_log"
#define SEDNA_EL_DEBUG_DUPLICATE_TO_STDERR				1

    extern FILE *el_debug_sync_output_stream;
#ifdef _WIN32
    extern HANDLE el_debug_sync_sem;
#endif

    void el_debug_sync_lock();
    void el_debug_sync_unlock();


#endif


#ifdef __cplusplus
}
#endif


#endif
