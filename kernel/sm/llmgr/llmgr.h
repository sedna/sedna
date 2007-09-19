/*
 * File:  llmgr.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _LLMGR_H
#define _LLMGR_H

#include "common/sedna.h"
#include "common/base.h"

#include "sm/wu/wu.h"

bool ll_logical_log_startup(int &sedna_db_version);

void ll_logical_log_shutdown();

//LONG_LSN ll_logical_log_checkpoint();
//void ll_logical_log_checkpoint(void *userData, SnapshotsVersionInfo *buf, size_t count);
int ll_logical_log_checkpoint(WuEnumerateVersionsParams *params, WuVersionEntry *buf, size_t count, int isGarbage);

void ll_logical_log_flush();

//void ll_log_flush_last_records();

void ll_logical_log_flush_lsn(LONG_LSN lsn);

//void ll_logical_log_flush_last_record();

void ll_truncate_logical_log();

//void ll_freePrevPersSnapshotBlocks(LONG_LSN last_lsn);

void ll_add_free_blocks_info(XPTR phys_xptr, void *block, int size);

void ll_add_decrease_info(__int64 old_size);

LONG_LSN ll_add_pers_snapshot_block_info(WuVersionEntry *blk_info);

TIMESTAMP ll_returnTimestampOfPersSnapshot();

//LONG_LSN ll_getLastChainLSN();

void ll_updateMinRcvLSN();

void ll_set_phys_rec_flag(bool flag); // enable/disable write of physical records in log

void ll_set_checkpoint_on_flag(bool flag); // set flag determing if checkpoint thread is active

void ll_log_set_checkpoint_flag(bool flag); // set flag to enable/disable checkpoint

LONG_LSN ll_recover_db_by_phys_records(); // physical recovery by log; returns lsn for logical recovery

void ll_recover_pers_heap();

//void ll_flush_file_head();

#endif

