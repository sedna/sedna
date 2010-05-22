/*
 * File:  physlog.cpp - Physical logging on sm
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * This functions should be used to write physical information in logical log.
 *
 */

#include "sm/llsm/physlog.h"
#include "sm/llsm/llhb.h"
#include "common/sm_vmm_data.h"
#include "sm/bufmgr/bm_core.h"
#include "sm/bufmgr/bm_rcv.h"
#include "sm/bufmgr/blk_mngmt.h"

// mem copy with offset increment
inline
static
void inc_mem_copy(void* dest, int &offs, const void* src, int len)
{
	if (len == 0) return;
	memcpy((char*)dest + offs, src, len);
	offs += len;
}

/*
 free blocks log record format:
 op (1 byte)
 prevLSN // lsn of the previous record in physical records chain
 size (4 bytes?)
 phys_xptr(xptr);
 block (size bytes)
*/
void llLogFreeBlocksInfo(xptr phys_xptr, void *block, int size)
{
	RECOVERY_CRASH;

	char *tmp_rec;  
	int rec_len;
	LSN ret_lsn;

	rec_len = sizeof(char) + sizeof(LSN) + sizeof(int) + sizeof(xptr) + size;
	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

	char op = LL_FREE_BLOCKS;
	int offs = 0;

	//create record body
	inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
	inc_mem_copy(tmp_rec, offs, &(llInfo->last_chain_lsn), sizeof(LSN));
	inc_mem_copy(tmp_rec, offs, &size, sizeof(int));
	inc_mem_copy(tmp_rec, offs, &phys_xptr, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, block, size);

	ret_lsn = llInsertRecord(tmp_rec, rec_len, -1);

	((vmm_sm_blk_hdr *)block)->lsn = ret_lsn + llGetRecordSize(NULL, rec_len); // for WAL purposes

	free(tmp_rec);
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
	RECOVERY_CRASH;

	char *tmp_rec;  
	int rec_len;
	LSN ret_lsn;

	rec_len = sizeof(char) + sizeof(LSN) + sizeof(WuVersionEntry) + sizeof(TIMESTAMP);
	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

	char op = LL_PERS_SNAPSHOT_ADD;
	int offs = 0;

	//create record body
	inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
	inc_mem_copy(tmp_rec, offs, &(llInfo->last_chain_lsn), sizeof(LSN));
	inc_mem_copy(tmp_rec, offs, &ts, sizeof(TIMESTAMP));
	inc_mem_copy(tmp_rec, offs, blk_info, sizeof(WuVersionEntry));

	ret_lsn = llInsertRecord(tmp_rec, rec_len, -1);

	free(tmp_rec);

	return ret_lsn + llGetRecordSize(NULL, rec_len); // for WAL purposes
}

/*
 decrease log record format:
 op (1 byte)
 prevLSN // lsn of the previous record in physical records chain
 old_size (8 bytes)
*/
void llLogDecrease(uint64_t old_size)
{
	RECOVERY_CRASH;

	char *tmp_rec;  
	int rec_len;

	rec_len = sizeof(char) + sizeof(LSN) + sizeof(uint64_t);
	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

	char op = LL_DECREASE;
	int offs = 0;

	//create record body
	inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
	inc_mem_copy(tmp_rec, offs, &(llInfo->last_chain_lsn), sizeof(LSN));
	inc_mem_copy(tmp_rec, offs, &old_size, sizeof(uint64_t));

	llInsertRecord(tmp_rec, rec_len, -1);

	llFlushAll(); // we need to flush this info right away

	free(tmp_rec);
}

/*
 recordblock log record format:

 op (1 byte)
 prevLSN // lsn of the previous record in physical records chain
 size (4 bytes?)
 phys_xptr(xptr);
 block (size bytes)
*/
void llLogRecordBlock(xptr xblk, void *block, int size)
{
	RECOVERY_CRASH;

	char *tmp_rec;  
	int rec_len;

	if (!llHbIsNeedAddLogging())
		return;

	rec_len = sizeof(char) + sizeof(LSN) + sizeof(int) + sizeof(xptr) + size;
	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

	char op = LL_HBBLOCK;
	int offs = 0;

	//create record body
	inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
	inc_mem_copy(tmp_rec, offs, &(llInfo->last_chain_lsn), sizeof(LSN));
	inc_mem_copy(tmp_rec, offs, &size, sizeof(int));
	inc_mem_copy(tmp_rec, offs, &xblk, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, block, size);

	llInsertRecord(tmp_rec, rec_len, -1);

	free(tmp_rec);
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
	char *tmp_rec;  
	unsigned rec_len;
	char op = LL_CHECKPOINT;
	int offs = 0;
	LSN ret_lsn;
	bool isFirstRec;

	isFirstRec = (params->persVersionsSent == 0 && params->garbageVersionsSent == 0);

	if (count > UINT32_MAX)
        throw SYSTEM_EXCEPTION("Cannot log checkpount record: too many blocks!");

    rec_len = sizeof(char) + sizeof(LSN) + sizeof(int) + sizeof(unsigned) + 
				sizeof(WuVersionEntry) * (unsigned)count;

	if (isFirstRec)
	{
		llInfo->ts = params->persSnapshotTs;
		llInfo->last_chain_lsn = LFS_INVALID_LSN;

		rec_len += sizeof(bm_masterblock) + sizeof(LSN);
	}

	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

	inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
	inc_mem_copy(tmp_rec, offs, &(llInfo->last_chain_lsn), sizeof(LSN));
	inc_mem_copy(tmp_rec, offs, &isGarbage, sizeof(int));
	inc_mem_copy(tmp_rec, offs, &count, sizeof(unsigned));

	for (unsigned int i = 0; i < count; i++)
		inc_mem_copy(tmp_rec, offs, &buf[i], sizeof(WuVersionEntry));
	
	// additional info for the first record
	if (isFirstRec)
	{
		llupdateMinRcvLSN();
		inc_mem_copy(tmp_rec, offs, mb, sizeof(bm_masterblock));
		inc_mem_copy(tmp_rec, offs, &(llInfo->min_rcv_lsn), sizeof(LSN));
	}

    ret_lsn = llInsertRecord(tmp_rec, rec_len, -1);

	if (isFirstRec)
		llInfo->checkpoint_lsn = ret_lsn;

	free(tmp_rec);

	return 1;
} 
