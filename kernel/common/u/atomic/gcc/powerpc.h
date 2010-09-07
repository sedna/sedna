/*
 * File: powerpc.h
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

/* Full memory barrier */
AO_INLINE void
AO_mb_full(void)
{
  __asm__ __volatile__("sync" : : : "memory");
}

#define HAVE_AO_MB_FULL
