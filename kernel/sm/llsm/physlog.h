/*
 * File:  physlog.h - Physical logging on sm
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * Only the user interface is specified here. For further information refer to physlog.cpp file.
 *
 */

#ifndef _LL_PHYS_LOG_
#define _LL_PHYS_LOG_

#include "common/llcommon/llMain.h"
#include "common/base.h"

#include "common/xptr/xptr.h"
#include "common/llcommon/lfsGlobals.h"
#include "common/xptr/wutypes.h"

#include "sm/wu/wu.h"

// Free block log record
// Parameters:
// 		phys_xptr - xptr of the free block
//		block - pointer to the contents of block in memory
//		size - number of bytes by block address
void llLogFreeBlocksInfo(xptr phys_xptr, void *block, unsigned size);

// Persistent snapshot add log record
// Parameters:
// 		blk_info - info about moved block (old and new xptrs)
//		ts - timestamp of the moved block
// Returns:
// 		lsn to flush the record when needed (this lsn must not be used to address the record!)
LSN llLogPersSnapshotInfo(WuVersionEntry *blk_info, TIMESTAMP ts);

// Decrease log record (used during data file extension)
// Parameters:
// 		old_size - previous size of the data file
void llLogDecrease(uint64_t old_size);

// Recordblock log record (used during hot backup process)
// Parameters:
// 		xblk - xptr of the given block
//		block - pointer to the contents of block in memory
//		size - number of bytes by block address
void llLogRecordBlock(xptr xblk, void *block, unsigned size);

// Checkpoint record
// Parameters:
// 		params - some info about snapshot (timestamp, number of versions etc.)
//      buf - array-info on blocks (old and new xptrs)
//      count - number of elements in buf
//      isGarbage - garbage flag (if true, blocks must be purged during recovery)
// Returns:
// 		0 - ok; -1 - error
int llLogCheckpoint(WuEnumerateVersionsParams *params, WuVersionEntry *buf, size_t count, int isGarbage);

#endif
