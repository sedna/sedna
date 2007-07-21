/*
 * File:  llmgr.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _LLMGR_H
#define _LLMGR_H

#include "common/sedna.h"
#include "common/base.h"

void ll_logical_log_startup();

void ll_logical_log_shutdown();

//LONG_LSN ll_logical_log_checkpoint();
void ll_logical_log_checkpoint(void *userData, SnapshotsVersionInfo *buf, size_t count);

void ll_logical_log_flush();

void ll_log_flush_last_records();

void ll_logical_log_flush_last_record();

void ll_truncate_logical_log();

void ll_freePrevPersSnapshotBlocks(LONG_LSN last_lsn);

void ll_add_free_blocks_info(XPTR phys_xptr, void *block, int size);

void ll_add_decrease_info(__int64 old_size);

void ll_add_pers_snapshot_block_info(transaction_id trid, SnapshotsVersionInfo *blk_info);

TIMESTAMP ll_returnTimestampOfPersSnapshot();

__int64 getCurPhCounter();

__int64 getNewPhCounter();

__int64 ll_copy_ph_file();

void ll_delete_prev_ph_file(__int64 prev_ph_counter);

//LONG_LSN ll_getLastChainLSN();

void ll_updateMinRcvLSN();

#endif

