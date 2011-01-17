/*
* BTrie unification
* Copyright (c) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#ifndef __BTRIE_UNIFY_H
#define __BTRIE_UNIFY_H

#define SPECIALIZATION "sedna_specialization.h"

#include <stdint.h>
#include <assert.h>

#ifndef __cplusplus

/******************************
   Boolean for c
*/

typedef int bool;

enum boolean_literal {
    false = 0, true = 1
};

#endif /* __cplusplus */

/*

Specialization must define following macros and type definitions:

typedef ... sptr_t;

sptr_t is a type for in-page absoulte or relative pointers. It must be as short as possible.
Negative relative values are not used, so it can be unsigned. It MUST be castable to integer.

typedef ... xptr_t;
xptr_t is a type for system-wide page pointer. Should be used to address any data in system.

void * XADDR(xptr_t a)
Converts system-wide pointer to usual pointer.

sptr_t NO_EDGE
NO_EDGE is null in-page pointer. As soon as sptr_t can be related pointer, NO_EDGE MUST NOT be 0 (zero)

xptr_t XNULL
system-wide pointer to null

DEBUG_INFO(const char * string)
DEBUG_INFO is used to output some debug information.

sptr_t ST_PAGE_HEADER_OFFSET
The amount of system-specific block information, which is never touched by implementation

void WRITE_PAGE(xptr_t &a)
Informs system, that page is about to be changed. "a" should be a l-value and can be changed during this call.

void READ_PAGE(xptr_t &a)
Informs system, that page is about to be read. "a" should be a l-value and can be changed during this call

void RELEASE_PAGE(xptr_t &a)
Informs system, that implementation no longer need this page.

void NEW_PAGE(xptr_t &a)
Asks system for a new page. "a" should be a l-value and can be changed during this call
Implementation wants page to be writable right after this call.

xptr_t GET_PAGE_ADDRESS(xptr_t a)
Asks system to tell a page address that can be used with READ_PAGE by system-wide in-page pointer

bool SAME_PAGE(xptr_t a, xptr_t b)
Operator to compare to system-wide pointers to tell if they point to the same page

sptr_t CALC_PAGE_SHIFT(void * v, xptr_t p)
Converts usual pointer and the page it points to to absolute in-page pointer
Specialization may assert that v pointer is from the page pointed by p. Otherwise there is a critical error.

*/

#include SPECIALIZATION

#endif /* __BTRIE_UNIFY_H */
