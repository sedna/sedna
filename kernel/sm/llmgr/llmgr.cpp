/*
 * File:  llmgr.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <string>

#include "sm/llmgr/llmgr_core.h"
#include "sm/sm_globals.h"
#include "common/errdbg/d_printf.h"
#include "common/tr_debug.h"
#include "common/base.h"
#include "sm/plmgr/plmgr.h"

using namespace std;

llmgr_core* logical_log_mgr;

bool ll_logical_log_startup(int &sedna_db_version)
{
#ifdef LOGICAL_LOG
  bool ret;

  d_printf2("db_files_path=%s\n", db_files_path);
  logical_log_mgr = new llmgr_core();
//d_printf1("1\n");
  ret = logical_log_mgr->ll_log_create(db_files_path, db_name/*, phys_log_mgr*/, sedna_db_version);
//d_printf1("2\n");
  logical_log_mgr->ll_log_create_shared_mem();
//d_printf1("3\n");
  string str = string("ll_logical_log_startup finished\n");
  WRITE_DEBUG_LOG(str.c_str());

  return ret;
#endif
}


void ll_logical_log_shutdown()
{
#ifdef LOGICAL_LOG
  logical_log_mgr->ll_log_release_shared_mem();
  logical_log_mgr->ll_log_release();
  delete logical_log_mgr;

  string str = string("ll_logical_log_shutdown finished\n");
  WRITE_DEBUG_LOG(str.c_str());
#endif
}

void ll_logical_log_checkpoint(void *userData, SnapshotsVersionInfo *buf, size_t count)
{
#ifdef LOGICAL_LOG
  LONG_LSN  ret_lsn;
  /*ret_lsn =*/ logical_log_mgr->ll_log_checkpoint(userData, buf, count);

  string str = string("ll_logical_log_checkpoint finished\n");
  WRITE_DEBUG_LOG(str.c_str());

//  return ret_lsn;
#else
  return -1;
#endif
}

void ll_logical_log_flush()
{
#ifdef LOGICAL_LOG
  logical_log_mgr->ll_log_flush(true);

  string str = string("ll_logical_log_flush finished\n");
  WRITE_DEBUG_LOG(str.c_str());
#endif
}

void ll_logical_log_flush_last_record()
{
#ifdef LOGICAL_LOG
  logical_log_mgr->ll_log_flush_last_record(true);
#endif
}

void ll_log_flush_last_records()
{
#ifdef LOGICAL_LOG
  logical_log_mgr->ll_log_flush_all_last_records(true);
#endif
}

void ll_truncate_logical_log()
{
#ifdef LOGICAL_LOG
  logical_log_mgr->ll_truncate_log(true);
#endif

}

void ll_freePrevPersSnapshotBlocks(LONG_LSN last_lsn)
{
#ifdef LOGICAL_LOG
  logical_log_mgr->freePrevCheckpointBlocks(last_lsn, true);
#endif
}

void ll_add_free_blocks_info(XPTR phys_xptr, void *block, int size)
{
#ifdef LOGICAL_LOG
  logical_log_mgr->ll_log_free_blocks(phys_xptr, block, size, true);
  logical_log_mgr->ll_log_flush(true);
  logical_log_mgr->ll_log_flush_all_last_records(true);
  logical_log_mgr->flush_file_head(true);
#endif
}

void ll_add_decrease_info(__int64 old_size)
{
#ifdef LOGICAL_LOG
  logical_log_mgr->ll_log_decrease(old_size, true);
  logical_log_mgr->ll_log_flush(true);
  logical_log_mgr->ll_log_flush_all_last_records(true);
  logical_log_mgr->flush_file_head(true);
#endif
}

void ll_add_pers_snapshot_block_info(transaction_id trid, SnapshotsVersionInfo *blk_info)
{
#ifdef LOGICAL_LOG
  logical_log_mgr->ll_log_pers_snapshot_add(trid, blk_info, true);
  logical_log_mgr->ll_log_flush(true);
//  logical_log_mgr->ll_log_flush_all_last_records(true);
  logical_log_mgr->flush_file_head(true);
#endif
}

TIMESTAMP ll_returnTimestampOfPersSnapshot()
{
#ifdef LOGICAL_LOG
  return logical_log_mgr->returnTimestampOfPersSnapshot(true);
#endif
}

__int64 getCurPhCounter()
{
#ifdef LOGICAL_LOG
  return logical_log_mgr->getCurPhCounter();
#endif
}

__int64 getNewPhCounter()
{
#ifdef LOGICAL_LOG
  return logical_log_mgr->getNewPhCounter();
#endif
}

__int64 ll_copy_ph_file()
{
#ifdef LOGICAL_LOG
  return logical_log_mgr->copyPhFile();
#endif
}

void ll_delete_prev_ph_file(__int64 prev_ph_counter)
{
#ifdef LOGICAL_LOG
  logical_log_mgr->deletePrevPhFile(prev_ph_counter);
#endif
}

/*LONG_LSN ll_getLastChainLSN()
{
#ifdef LOGICAL_LOG
  logical_log_mgr->getLastChainLSN();
#endif
}
*/

void ll_updateMinRcvLSN()
{
#ifdef LOGICAL_LOG
  logical_log_mgr->updateMinRcvLSN();
#endif
}
