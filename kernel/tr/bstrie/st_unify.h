#ifndef __ST_UNIFY_H
#define __ST_UNIFY_H

#include <stdint.h>

#ifndef UNIT_BSTRIE_DEBUG

#include "common/sedna.h"
#include "common/xptr.h"
#include "common/sm_vmm_data.h"
#include "tr/vmm/vmm.h"

#define ST_PAGE_HEADER_OFFSET (sizeof(vmm_sm_blk_hdr))
#define CALC_PAGE_SHIFT(v, p) (uint16_t) ((uint32_t) (v) - (uint32_t) (XADDR(p)))

#else

#include <assert.h>

typedef int bool;
typedef uint64_t xptr;

#define false 0
#define true 1

#define U_ASSERT(x) assert(x)

#ifndef MIN
#define MIN(a, b) (a < b ? a : b)
#endif

//#define PAGE_SIZE (64*1024)
#define PAGE_SIZE (256)

#define XNULL 0ULL

#define ADDR2XPTR(a) ((uint64_t) (uint32_t) (a))

#define ST_PAGE_HEADER_OFFSET 16

#define CHECKP(a) 
#define XADDR(a) (void *) (uint32_t) ((a) & 0xffffffffULL)
#define PAGE_BIT_MASK 0xffffff00
#define PAGE_RBIT_MASK 0x000000ff
#define BLOCKXPTR(a) ((a) & 0xffffffffffffff00ULL)

#define CALC_PAGE_SHIFT(v, p) (uint16_t) ((uint32_t) (v) - (uint32_t) (XADDR(p)))

void vmm_alloc_data_block(xptr * block);

#endif

typedef xptr st_t;

#endif /* __ST_UNIFY_H */