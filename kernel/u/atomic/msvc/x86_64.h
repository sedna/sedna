/*
 * File: x86_64.h
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

/* Full memory barrier */
#include <emmintrin.h>

/* Note: "mfence" (SSE2) is supported on all x86_64/amd64 chips.      */
#define AO_mb_full(void) _mm_mfence(void)

#define AO_HAVE_MB_FULL
