/*
* BTrie Sedna specialization
* Copyright (c) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "common/sedna.h"
#include "common/base.h"
#include "common/xptr.h"

#include "tr/vmm/vmm.h"

typedef uint16_t sptr_t;
#define NO_EDGE ((sptr_t) 0xffff)

typedef xptr xptr_t;

#ifdef DEBUGI
#include "common/errdbg/d_printf.h"
#define DEBUG_INFO(m) d_printf1(m)
#else /* DEBUGI */
#define DEBUG_INFO(m)
#endif /* DEBUGI */

#define ST_PAGE_HEADER_OFFSET (sizeof(vmm_sm_blk_hdr))

#define WRITE_PAGE(a) do { WRITEP(a) } while(0)
#define READ_PAGE(a) do { CHECKP(a) } while(0)
#define RELEASE_PAGE(a)
#define NEW_PAGE(a) do { vmm_alloc_data_block(&(a)); WRITE_PAGE(a); } while (0)

#define GET_PAGE_ADDRESS(a) BLOCKXPTR(a)

#define SAME_PAGE(a, b) (same_block(a, b))
#define CALC_PAGE_SHIFT(v, p) (sptr_t) ((intptr_t) (v) - (intptr_t) (XADDR(BLOCKXPTR(p))))
