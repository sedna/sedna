/*
 * File:  sm_vmm_data.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _SM_VMM_DATA_H
#define _SM_VMM_DATA_H

#include "xptr.h"

/* return the pointer to the block header */
#define GETBLOCKHDR_ADDR(p)   ((vmm_sm_blk_hdr*)((int)p & PAGE_BIT_MASK))
#define BLOCK_PARTS           16
#define BLOCK_PART_SIZE (PAGE_SIZE/BLOCK_PARTS)

//#define LRU

#ifdef LRU
typedef __int64 LRU_stamp;
#endif

struct vmm_sm_blk_hdr
{
    xptr p;		// the first 4 bytes is the layer of the block
    ramoffs roffs;	// address of block in buffer memory
    bool is_changed;
    LSN lsn;
    CP_counter cntrs[BLOCK_PARTS];
#ifdef LRU
    LRU_stamp lru;
#endif
//    int last_read;	// for timestamps (needed???)
//    int last_write;	// for timestamps (needed???)
};



#endif


