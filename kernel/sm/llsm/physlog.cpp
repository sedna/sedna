/*
 * File:  physlog.cpp - Physical logging on sm
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * This functions should be used to write physical information in logical log.
 *
 */

#include "sm/llsm/physlog.h"
#include "sm/llsm/llhb.h"
#include "common/xptr/sm_vmm_data.h"
#include "sm/bufmgr/bm_core.h"
#include "sm/bufmgr/blk_mngmt.h"

/*
 free blocks log record format:
 op (1 byte)
 prevLSN // lsn of the previous record in physical records chain
 size (4 bytes?)
 phys_xptr(xptr);
 block (size bytes)
*/
void llLogFreeBlocksInfo(xptr phys_xptr, void *block, unsigned size)
{
	LSN ret_lsn;

	ret_lsn = llLogGeneral(PHYS_RECORD, INVALID_TRID, LL_FREE_BLOCKS, true, 4,
                  &(llInfo->last_chain_lsn), sizeof(LSN),
	              &size, sizeof(unsigned), &phys_xptr, sizeof(xptr),
	              block, (size_t)size);

	/*
	 * for WAL purposes this record will be flushed only when the block is flushed
	 * (or when log buffer overflows).
	 */
	((vmm_sm_blk_hdr *)block)->lsn = ret_lsn;
}

/*
 persistent snapshot add log record format:
 op (1 byte)
 prevLSN // lsn of the previous record in LL_CHECKPOINT-LL_PERS_SNAPSHOT_ADD chain
 timestamp of the lxptr version (used to check if need to move version)
 SnapshotsVersion --- info about physical/logical xptr of block
*/
LSN llLogPersSnapshotInfo(WuVersionEntry *blk_info, TIMESTAMP ts)
{
    LSN ret_lsn;

    ret_lsn = llLogGeneral(PHYS_RECORD, INVALID_TRID, LL_PERS_SNAPSHOT_ADD, true, 3,
                &(llInfo->last_chain_lsn), sizeof(LSN),
                &ts, sizeof(TIMESTAMP),
                blk_info, sizeof(WuVersionEntry));

	return ret_lsn;
}

/*
 decrease log record format:
 op (1 byte)
 prevLSN // lsn of the previous record in physical records chain
 old_size (8 bytes)
*/
void llLogDecrease(uint64_t old_size)
{
    llLogGeneral(PHYS_RECORD, INVALID_TRID, LL_DECREASE, false, 2,
                &(llInfo->last_chain_lsn), sizeof(LSN),
                &old_size, sizeof(uint64_t));


	/*
	 * we need to flush this info right away, since file
	 * will expand immediately after this.
	 */
    llFlushAll();
}

/*
 recordblock log record format:

 op (1 byte)
 prevLSN // lsn of the previous record in physical records chain
 size (4 bytes?)
 phys_xptr(xptr);
 block (size bytes)
*/
void llLogRecordBlock(xptr xblk, void *block, unsigned size)
{
    llLogGeneral(PHYS_RECORD, INVALID_TRID, LL_HBBLOCK, false, 4,
                    &(llInfo->last_chain_lsn), sizeof(LSN),
                    &size, sizeof(unsigned),
                    &xblk, sizeof(xptr),
                    block, (size_t)size);
}

// updates minimal recovery lsn for active transactions during checkpoint
static void llupdateMinRcvLSN()
{
	LSN lsn = LFS_INVALID_LSN;
  
	for (int i = 0; i < CHARISMA_MAX_TRNS_NUMBER; i++)
	{
  		if (llInfo->llTransInfoTable[i].first_lsn == LFS_INVALID_LSN) continue;

		if (lsn == LFS_INVALID_LSN)
			lsn = llInfo->llTransInfoTable[i].first_lsn;
		else if (llInfo->llTransInfoTable[i].first_lsn < lsn)
			lsn = llInfo->llTransInfoTable[i].first_lsn;  
	}

  	llInfo->min_rcv_lsn = lsn;
}

// Checkpoint record format:
// First record:
//  	op (1 byte)
//      prevLSN // lsn of the previous record in physical records chain
//      is_garbage(int)
//      count of blocks(size_t)
//      info about each block(count times sizeof(WuVersionEntry))
//      masterblock
//      minimal recovery lsn (LSN)
// Other records:
//  	op (1 byte)
//      prevLSN // lsn of the previous record in physical records chain
//      is_garbage(int)
//      count of blocks(unsigned)
//      info about each block(count times sizeof(WuVersionEntry))
int llLogCheckpoint(WuEnumerateVersionsParams *params, WuVersionEntry *buf, size_t count, int isGarbage)
{
	LSN ret_lsn;
	bool isFirstRec;
	uint32_t count_u32 = (uint32_t)count;

	isFirstRec = (params->persVersionsSent == 0 && params->garbageVersionsSent == 0);

	if (count > UINT32_MAX)
        throw SYSTEM_EXCEPTION("Cannot log checkpount record: too many blocks!");

	if (isFirstRec)
	{
		llInfo->ts = params->persSnapshotTs;
		llInfo->last_chain_lsn = LFS_INVALID_LSN;
	}

	// first record designates start of the new recovery chain
	if (isFirstRec)
		llupdateMinRcvLSN();

    if (isFirstRec)
        ret_lsn = llLogGeneral(PHYS_RECORD, INVALID_TRID, LL_CHECKPOINT, false, 6,
                            &(llInfo->last_chain_lsn), sizeof(LSN),
                            &isGarbage, sizeof(int),
                            &count_u32, sizeof(uint32_t),
                            buf, sizeof(WuVersionEntry) * count,
                            mb, sizeof(bm_masterblock),
                            &(llInfo->min_rcv_lsn), sizeof(LSN));
    else
        ret_lsn = llLogGeneral(PHYS_RECORD, INVALID_TRID, LL_CHECKPOINT, false, 4,
                            &(llInfo->last_chain_lsn), sizeof(LSN),
                            &isGarbage, sizeof(int),
                            &count_u32, sizeof(uint32_t),
                            buf, sizeof(WuVersionEntry) * count);


	if (isFirstRec)
		llInfo->checkpoint_lsn = ret_lsn;

	return 1;
} 
