/*
 * File:  physrcv.cpp - Physical recovery on sm
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * This functions should be used to recover physical state of a database
 *
 */

#include "sm/smtypes.h"

#include "u/uutils.h"
#include "sm/wu/wu.h"

#include <stdlib.h>
#include <string>

static unsigned int sector_size = 512;
static void *ctrl_blk = NULL;

static void ll_rcv_master_block(const void *p)
{
    memset(mb, '\0', MASTER_BLOCK_SIZE);
    memcpy(mb, p, sizeof(bm_masterblock));

    flush_master_block();
}

// Recovery functions for physical records
static void llRcvCheckpoint(LSN lsn, void *RecBuf)
{
	char *offs;
	int is_garbage;
    uint32_t count;
	WuVersionEntry *blocks_info;

	offs = (char *)RecBuf + sizeof(char) + sizeof(LSN); // skip operation code and next-chain-lsn

	is_garbage = *((int *)offs);
	offs += sizeof(int);

	count = *((uint32_t *)offs);
	offs += sizeof(uint32_t);

	blocks_info = (WuVersionEntry *)offs;
	for (uint32_t i = 0; i < count; i++)
	{
		if (is_garbage)
			push_to_persistent_free_blocks_stack(&(mb->free_data_blocks),
			        WuExternaliseXptr(blocks_info[i].xptr));
		else
		{
		    read_block_addr(WuExternaliseXptr(blocks_info[i].xptr),
		            ctrl_blk, PAGE_SIZE, false);
			write_block_addr(WuExternaliseXptr(blocks_info[i].lxptr),
			        ctrl_blk, PAGE_SIZE, false);
			push_to_persistent_free_blocks_stack(&(mb->free_data_blocks),
			        WuExternaliseXptr(blocks_info[i].xptr));
		}
	}

	RECOVERY_CRASH;
}

static void llRcvFreeBlock(LSN lsn, void *RecBuf)
{
    char *offs;
	int free_blk_info_size;
	void *free_blk_info;
	xptr free_blk_info_xptr;

	offs = (char *)RecBuf + sizeof(char) + sizeof(LSN);

	free_blk_info_size = *((unsigned *)offs);
	offs += sizeof(unsigned);

	free_blk_info_xptr = *((xptr *)offs);
	offs += sizeof(xptr);

	free_blk_info = offs;

	// we need block in sector-aligned buffer here
	memcpy(ctrl_blk, free_blk_info, free_blk_info_size);
	write_block_addr(free_blk_info_xptr, ctrl_blk, free_blk_info_size, false);

	RECOVERY_CRASH;
}

static void llRcvPersSnpAdd(LSN lsn, void *RecBuf)
{
    char *offs;
	TIMESTAMP ts;
	WuVersionEntry *blocks_info;

	offs = (char *)RecBuf + sizeof(char) + sizeof(LSN); // skip operation code and next-chain-lsn

	ts = *((TIMESTAMP *)offs);
	offs += sizeof(TIMESTAMP);

	blocks_info = (WuVersionEntry *)offs;

	// read "last" block's header to check timestamp
	read_block_addr(WuExternaliseXptr(blocks_info->lxptr), ctrl_blk,
	    (sizeof(vmm_sm_blk_hdr) + sector_size - 1) & ~(sector_size - 1), false);

	// we must recover block only if it was moved to "blocks_info->xptr" place
	if (ts != ((vmm_sm_blk_hdr *)ctrl_blk)->versionsHeader.creatorTs[0])
	{
	    read_block_addr(WuExternaliseXptr(blocks_info->xptr),
	            ctrl_blk, PAGE_SIZE, false);

		U_ASSERT(
			((vmm_sm_blk_hdr *)ctrl_blk)->versionsHeader.creatorTs[0] == ts &&
			((vmm_sm_blk_hdr *)ctrl_blk)->p == WuExternaliseXptr(blocks_info->lxptr) &&
			((vmm_sm_blk_hdr *)ctrl_blk)->versionsHeader.xptr[0] == blocks_info->lxptr
		);

		write_block_addr(WuExternaliseXptr(blocks_info->lxptr), ctrl_blk,
		        PAGE_SIZE, false);
	}
}

static void llRcvDecrease(LSN lsn, void *RecBuf)
{
	uint64_t old_size;
	char *offs;

	offs = (char *)RecBuf + sizeof(char) + sizeof(LSN);

	old_size = *((uint64_t *)offs);

    if (uSetEndOfFile(data_file_handle, old_size, U_FILE_BEGIN,
            __sys_call_error) == 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot decrease data file");
}

static void llRcvBlock(LSN lsn, void *RecBuf)
{
    char *offs;
	int blk_info_size;
	void *blk_info;
	xptr blk_info_xptr;

	if (!llInfo->hotbackup_needed) return;

	offs = (char *)RecBuf + sizeof(char) + sizeof(LSN);

	blk_info_size = *((unsigned *)offs);
	offs += sizeof(unsigned);

	blk_info_xptr = *((xptr *)offs);
	offs += sizeof(xptr);

	blk_info = offs;

	// we need block in sector-aligned buffer here
	memcpy(ctrl_blk, blk_info, blk_info_size);
	write_block_addr(blk_info_xptr, ctrl_blk, blk_info_size, false);

	RECOVERY_CRASH;
}

// returns next lsn in physical chain
static LSN llRcvGetNextPhysLsn(LSN curr_lsn, void *RecBuf)
{
	return *((LSN *)((char *)RecBuf + sizeof(char)));
}

// Main structure for physical recovery
static
struct llRecInfo llRcvPhysRecsInfo[] =
{
	{LL_CHECKPOINT,         llRcvCheckpoint},
	{LL_FREE_BLOCKS,        llRcvFreeBlock},
	{LL_PERS_SNAPSHOT_ADD,  llRcvPersSnpAdd},
	{LL_DECREASE,           llRcvDecrease},
	{LL_HBBLOCK,            llRcvBlock},
};
static int llRcvPhysRecsInfoLen = sizeof(llRcvPhysRecsInfo) / sizeof(llRecInfo);

// This function restores persistent snapshot and free blocks information
LSN llRecoverPhysicalState()
{
    char *rec, *offs;
    LSN lsn;
    uint32_t count;
    void *ctrl_blk_buf;

    lsn = llInfo->checkpoint_lsn;

    if (lsn == LFS_INVALID_LSN)
    {
            // we are in big trouble here :)
            return LFS_INVALID_LSN;
    }

    // recover info from first checkpoint record
    rec = (char *)llGetRecordFromDisc(&lsn);

    if (rec[0] != LL_CHECKPOINT)
            throw SYSTEM_EXCEPTION("Invalid checkpoint record");

    offs = rec + sizeof(char) + sizeof(LSN) + sizeof(int);
    count = *((uint32_t *)offs);
    offs += sizeof(uint32_t) + sizeof(WuVersionEntry) * count;

    // recover master block
    ll_rcv_master_block(offs);
    offs += sizeof(bm_masterblock);

    // recover minimal lsn for logical recovery
    llInfo->min_rcv_lsn = *((LSN *)offs);

    lsn = llInfo->last_chain_lsn;

    if (uGetDiskSectorSize(&sector_size, databaseOptions->dataFilePath.c_str(), __sys_call_error) == 0) {
        throw USER_EXCEPTION(SE4051);
    }

    if ((ctrl_blk_buf = malloc(2 * PAGE_SIZE)) == NULL)
            throw SYSTEM_EXCEPTION("Cannot allocate memory");
    // sector alligned buffer
    ctrl_blk = (void *)((uintptr_t)((char *)ctrl_blk_buf + sector_size - 1) &
            ~(uintptr_t)(sector_size - 1));

    // recover physical state by scaning all physical records
    llScanRecords(llRcvPhysRecsInfo, llRcvPhysRecsInfoLen, lsn,
            llRcvGetNextPhysLsn, NULL);

    free(ctrl_blk_buf);
    ctrl_blk = NULL;

    // return lsn for logical recovery
    return (llInfo->min_rcv_lsn == LFS_INVALID_LSN) ?
            llInfo->checkpoint_lsn :llInfo->min_rcv_lsn;
}
