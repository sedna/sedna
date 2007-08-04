/*
 * File:  sm_vmm_data.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _SM_VMM_DATA_H
#define _SM_VMM_DATA_H

#include "common/sedna.h"
#include "common/xptr.h"

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
	int trid_wr_access;

	static void init(vmm_sm_blk_hdr* hdr)
	{
		int i = 0;
		hdr->p = XNULL;
		hdr->roffs = 0;
		hdr->is_changed = false;
		hdr->lsn = NULL_LSN;
		for (i = 0; i < BLOCK_PARTS; ++i)
			hdr->cntrs[i] = 0;
#ifdef LRU
		hdr->lru = 0;
#endif
        hdr->trid_wr_access = -1;
	}
};



#endif


