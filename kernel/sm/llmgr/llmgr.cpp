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
#include "sm/llmgr/sm_llmgr.h"

using namespace std;

sm_llmgr* logical_log_mgr;
bool enable_write_of_phys_recs = true;

bool ll_logical_log_startup(int &sedna_db_version)
{
#ifdef LOGICAL_LOG
  bool ret;

  d_printf2("db_files_path=%s\n", db_files_path);
  logical_log_mgr = new sm_llmgr();
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

int ll_logical_log_checkpoint(WuEnumerateVersionsParams *params, WuVersionEntry *buf, size_t count, int isGarbage)
{
#ifdef LOGICAL_LOG
  LONG_LSN  ret_lsn;
  /*ret_lsn =*/ logical_log_mgr->ll_log_checkpoint(params, buf, count, isGarbage);

  string str = string("ll_logical_log_checkpoint finished\n");
  WRITE_DEBUG_LOG(str.c_str());

//  return ret_lsn;
  return 1;
#else
  return 0;
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

/*
void ll_freePrevPersSnapshotBlocks(LONG_LSN last_lsn)
{
#ifdef LOGICAL_LOG
  logical_log_mgr->freePrevCheckpointBlocks(last_lsn, true);
#endif
}
*/

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

void ll_add_pers_snapshot_block_info(WuVersionEntry *blk_info)
{
#ifdef LOGICAL_LOG
  bool isGarbage = false;

  logical_log_mgr->ll_log_pers_snapshot_add(blk_info, isGarbage, true);
  logical_log_mgr->ll_log_flush(true);
  logical_log_mgr->ll_log_flush_all_last_records(true);
  logical_log_mgr->flush_file_head(true);
#endif
}

TIMESTAMP ll_returnTimestampOfPersSnapshot()
{
#ifdef LOGICAL_LOG
  return logical_log_mgr->returnTimestampOfPersSnapshot(true);
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

void ll_set_phys_rec_flag(bool flag)
{
#ifdef LOGICAL_LOG
	enable_write_of_phys_recs = flag;
#endif
}

void ll_set_checkpoint_on_flag(bool flag)
{
#ifdef LOGICAL_LOG
	logical_log_mgr->set_checkpoint_on_flag(flag);
#endif
}

LONG_LSN ll_recover_db_by_phys_records()
{
#ifdef LOGICAL_LOG
	return logical_log_mgr->recover_db_by_phys_records(false);
#endif
}

void ll_log_set_checkpoint_flag(bool flag)
{
#ifdef PHYS_LOG
  logical_log_mgr->set_checkpoint_flag(flag, true);
#endif  
}

void ll_recover_pers_heap()
{
#ifdef LOGICAL_LOG
	logical_log_mgr->restorePh();
#endif
}

void ll_flush_file_head()
{
#ifdef LOGICAL_LOG
	logical_log_mgr->flush_file_head(true);
#endif
}

