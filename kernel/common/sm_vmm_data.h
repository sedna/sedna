/*
 * File:  sm_vmm_data.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _SM_VMM_DATA_H
#define _SM_VMM_DATA_H

#include "common/sedna.h"
#include "common/xptr.h"
#include "common/wustructures.h"

struct vmm_sm_blk_hdr
{
    xptr p;		// the first 4 bytes is the layer of the block
    LSN lsn;
	VersionsHeader versionsHeader;

	int blockType;  // (c) A. Kalinin
	ramoffs roffs;	// address of block in buffer memory
    int is_changed;
	int trid_wr_access;

	static void init(vmm_sm_blk_hdr *hdr)
	{
		hdr->p = XNULL;
		hdr->lsn = NULL_LSN;
		memset(hdr->versionsHeader.xptr, 0, sizeof hdr->versionsHeader.xptr);
		memset(hdr->versionsHeader.creatorTs, -1, sizeof hdr->versionsHeader.creatorTs);
		hdr->roffs = 0;
		hdr->blockType = 0;
		hdr->is_changed = false;
		hdr->trid_wr_access = -1;
	}
};

#endif
