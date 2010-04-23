/*
* BTrie Sedna unification
* Copyright (c) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#ifndef __BTRIE_UNIFY_H
#define __BTRIE_UNIFY_H

#include <stdint.h>
#include <assert.h>

#include "common/base.h"
#include "common/xptr.h"
#include "common/errdbg/d_printf.h"
#include "tr/vmm/vmm.h"

#ifndef __cplusplus

/******************************
   Boolean for c
*/

typedef int bool;

enum boolean_literal {
    false = 0, true = 1
};

#endif /* __cplusplus */

typedef uint16_t sptr_t;
#define NO_EDGE 0xffff

typedef xptr xptr_t;

#ifdef DEBUGI
#define DEBUG_INFO(m) d_printf1(m)
#else /* DEBUGI */
#define DEBUG_INFO(m)
#endif /* DEBUGI */

#define ST_PAGE_HEADER_OFFSET (sizeof(vmm_sm_blk_hdr))

#define WRITE_PAGE(a) WRITEP(a)
#define READ_PAGE(a) CHECKP(a)
#define NEW_PAGE(a) allocate_page(a)

#define GET_PAGE_ADDRESS(a) BLOCKXPTR(a)

#define SAME_PAGE(a, b) (same_block(a, b))
#define CALC_PAGE_SHIFT(v, p) (uint16_t) ((uint32_t) (v) - (uint32_t) (XADDR(p)))

inline
void allocate_page(xptr_t * block) {
    vmm_alloc_data_block(block);
}

#endif /* __BTRIE_UNIFY_H */
