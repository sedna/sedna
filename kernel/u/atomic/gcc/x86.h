/*
 * File: x86.h
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

/* Full memory barrier */
AO_INLINE void
AO_mb_full(void)
{
    __asm__ __volatile__ ("lock; addl $0,0(%%esp)" : : : "memory");
}

#define HAVE_AO_MB_FULL
