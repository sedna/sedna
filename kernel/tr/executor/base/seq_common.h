/*
 * File:  seq_common.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/xptr/sm_vmm_data.h"
#include "tr/vmm/vmm.h"
 
#ifndef _SEQ_COMMON_H
#define _SEQ_COMMON_H

struct seq_blk_hdr 
{
    vmm_sm_blk_hdr sm_vmm;	// sm/vmm parameters
    xptr nblk;				// next block
    int cursor;				// cursor

	static void init(void *p) 
    { 
        VMM_SIGNAL_MODIFICATION(ADDR2XPTR(p));
        ((seq_blk_hdr*)p)->nblk = XNULL; 
        ((seq_blk_hdr*)p)->cursor = sizeof(seq_blk_hdr);
    }
};


#define SEQ_BLK_HDR(p)					((seq_blk_hdr*)((uintptr_t)(XADDR(p)) & PAGE_BIT_MASK))
#define SEQ_BLK_FREE_SPACE(p)			(int)(PAGE_SIZE - ((seq_blk_hdr*)(p))->cursor)
#define SEQ_BLK_CURSOR(p)				((void*)((char*)(p) + ((seq_blk_hdr*)(p))->cursor))

#endif /* _SEQ_COMMON_H */

