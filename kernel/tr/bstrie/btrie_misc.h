/*
* BTrie headers
* Copyright (c) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#ifndef __BTRIE_MISC_H
#define __BTRIE_MISC_H

#include <stdlib.h>
#include <string.h>

/** Read variable <dest> from octet array <src> and increment src by size of <dest> */
#define CAST_AND_READ(dest, src) { memcpy(&(dest), (src), sizeof(dest)); src += sizeof(dest); }

/** Write variable <src> to octet array <dest> and increment src by size of <src> */
#define CAST_AND_WRITE(dest, src) { memcpy((dest), &(src), sizeof(src)); dest += sizeof(src); }

/** Move memory segment from (A; B) to point <C> (intersection-safe) */
inline
static void * memmove_ABC(const void * A, const void * B, void * C)
{
    return memmove(C, A, ((char *) B) - ((char *) A));
}

/** Move memory segment from (A; B) by <d> octets (intersection-safe) */
inline
static void * memmove_ABd(void * A, const void * B, size_t d)
{
    return memmove(((char *) A) + d, A, ((char *) B) - ((char *) A));
}

/** Create a new string and copy the given one in it */
inline
static char * str_alloc_ncpy(const char * s, size_t len, void * (alloc_f)(size_t size))
{
    char * tmp = (char *) alloc_f(len + 1);
    strncpy(tmp, s, len + 1);
    return tmp;
}

/** Binary search in character array */
inline
static int bisearch_char(char k, char * A, int n)
{
    int l = 0, r = n - 1, m;

    if (n == 0) { return 0; }
    while (l <= r) {
        m = (l + r) >> 1;

        if (A[m] == k) { return m; }
        if (k > A[m]) { l = m+1; }
        else { r = m-1; }
    }

    return l;
}


//void __debug_walkthrough(btrie_t tree);

#endif /* __BTRIE_MISC_H */
