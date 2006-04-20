/*
 * File:  llmgr.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>

#include "llmgr_core.h"
#include "sm_globals.h"
#include "d_printf.h"
#include "tr_debug.h"
#include "base.h"
#include "plmgr.h"

using namespace std;

llmgr_core* logical_log_mgr;

void ll_logical_log_startup()
{
#ifdef LOGICAL_LOG
  d_printf2("db_files_path=%s\n", db_files_path);
  logical_log_mgr = new llmgr_core();
//d_printf1("1\n");
  logical_log_mgr->ll_log_create(db_files_path, db_name, phys_log_mgr);
//d_printf1("2\n");
  logical_log_mgr->ll_log_create_shared_mem();
//d_printf1("3\n");
  string str = string("ll_logical_log_startup finished\n");
  WRITE_DEBUG_LOG(str.c_str());

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

LONG_LSN ll_logical_log_checkpoint()
{
#ifdef LOGICAL_LOG
  LONG_LSN  ret_lsn;
  ret_lsn = logical_log_mgr->ll_log_checkpoint(true);

  string str = string("ll_logical_log_checkpoint finished\n");
  WRITE_DEBUG_LOG(str.c_str());

  return ret_lsn;
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

void ll_truncate_logical_log()
{
#ifdef LOGICAL_LOG
  logical_log_mgr->ll_truncate_log(true);
#endif

}
