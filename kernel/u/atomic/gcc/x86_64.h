/*
 * File: x86_64.h
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

/* Full memory barrier */
AO_INLINE void
AO_mb_full(void)
{
  /* Note: "mfence" (SSE2) is supported on all x86_64/amd64 chips.      */
  __asm__ __volatile__("mfence" : : : "memory");
}

#define AO_HAVE_MB_FULL
