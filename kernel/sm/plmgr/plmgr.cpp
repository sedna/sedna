/*
 * File:  plmgr.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>
#include <iostream>
#include "base.h"
//#include "nodes.h"
#include "plmgr_core.h"
#include "sm_plmgr.h"
#include "sm_globals.h"
#include "d_printf.h"
#include "tr_debug.h"
#include "sm_vmm_data.h"

using namespace std;

sm_plmgr* phys_log_mgr;

bool is_write_to_phys_log = true;

//variables for debug
int log_change_times =0;
int log_change_size =0;

int log_decrease_times =0;

int log_master_block_times = 0;
int log_master_block_size = 0;

int log_create_node_blk_times = 0;

int log_flush_log_times = 0;

void ll_phys_log_change(void *p, shft size)
{
#ifdef PHYS_LOG
  if (is_write_to_phys_log == true)
  {
     log_change_times++;
     log_change_size+=size;
     phys_log_mgr->logInsert(p, size, PL_CHANGE);    

     string str = string("ll_phys_log_change, size=") + int2string(size) + string("\n");
     WRITE_DEBUG_LOG(str.c_str());
     //d_printf2("ll_log_change size=%d\n", size);
  }          
#endif  
  
}

// decrease size of the file
void ll_phys_log_decrease(__int64 old_size)
{
#ifdef PHYS_LOG
  if (is_write_to_phys_log == true)
  {
     log_decrease_times++;
     phys_log_mgr->logInsert(&old_size, sizeof(__int64), PL_DECREASE);
     string str = string("ll_phys_log_decrease, old_size=") + int2string(old_size) + string("\n");
     WRITE_DEBUG_LOG(str.c_str());
     d_printf1("phys log decrease\n");
  }

#endif
  
}

// save master block in log
void ll_phys_log_master_blk(void *p, shft size)
{
#ifdef PHYS_LOG
  if (is_write_to_phys_log == true)
  {
     log_master_block_times++;
     log_master_block_size+=size;  
     phys_log_mgr->logInsert(p, size, PL_CHANGE_MASTER);
     string str = string("ll_phys_log_master_blk, size=") + int2string(size) + string("\n");
     WRITE_DEBUG_LOG(str.c_str());
  }
#endif
}


// create empty node block
void ll_phys_log_create_node_blk(void *p)
{
#ifdef PHYS_LOG
     log_create_node_blk_times++;
     phys_log_mgr->logInsert(p, 0, PL_CREATE_NODE_BLK);

     d_printf1("ll_create node blk=\n");
     string str = string("ll_phys_log_create_node_blk\n");
     WRITE_DEBUG_LOG(str.c_str());
#endif
}

// flush block to disk (function is used for implementing WAL)
void ll_phys_log_flush_blk(void *p, bool sync)
{
#ifdef PHYS_LOG
  log_flush_log_times++;
 //phys_log_mgr->logFlush(GETBLOCKHDR_ADDR(p)->lsn);
  d_printf2("in flush func lsn=%d\n", sm_get_block_hdr(p)->lsn);
  sm_addr2xptr(p).print(); 
  phys_log_mgr->logFlushPortionOfRecords(sm_get_block_hdr(p)->lsn, sync);

  string str = string("ll_phys_log_flush_blk finished\n");
  WRITE_DEBUG_LOG(str.c_str());

#endif
}

void ll_phys_log_flush(bool sync)
{
#ifdef PHYS_LOG
  phys_log_mgr->logFlush(sync);
  string str = string("ll_phys_log_flush finished\n");
  WRITE_DEBUG_LOG(str.c_str());
#endif
}


// recover database after soft fail
LONG_LSN ll_phys_log_recover_db()
{
#ifdef PHYS_LOG
  return phys_log_mgr->recoverDataBase();

//  string str = string("ll_phys_log_recover_db finished\n");
//  WRITE_DEBUG_LOG(str.c_str());
#else
  return NULL_LSN;
#endif
}


// clear physical log
void ll_phys_log_clear(const LONG_LSN& last_cp_lsn, bool sync)
{
#ifdef PHYS_LOG
  phys_log_mgr->logClear(last_cp_lsn, sync);
#endif
}


bool ll_phys_log_startup()
{
#ifdef PHYS_LOG
  phys_log_mgr = new sm_plmgr();
  return phys_log_mgr->create_phys_log(db_files_path);
#else
  return true;
#endif
}

void ll_phys_log_startup_shared_mem()
{
#ifdef PHYS_LOG
  phys_log_mgr->create_shared_mem(phys_log_ext_portion);
#endif
}

void ll_phys_log_shutdown()
{
#ifdef PHYS_LOG
  file_head head = phys_log_mgr->get_file_head();
  phys_log_mgr->release_shared_mem();
  phys_log_mgr->release_phys_log(head);
  
  d_printf1("Physical log statistics:\n");
  d_printf2("log_change_times=%d\n", log_change_times);
  d_printf2("log_change_size=%d\n",log_change_size);
  d_printf2("log_decrease_times=%d\n",log_decrease_times);
  d_printf2("log_master_block_times=%d\n",log_master_block_times);
  d_printf2("log_master_block_size=%d\n",log_master_block_size);
  d_printf2("log_create_node_blk_times=%d\n",log_create_node_blk_times);
  d_printf2("log_flush_log_times=%d\n",log_flush_log_times);

  string str = string("ll_phys_log_shutdown finished\n");
  WRITE_DEBUG_LOG(str.c_str());

#endif
}

void ll_phys_log_set_checkpoint_flag(bool flag)
{
#ifdef PHYS_LOG
  phys_log_mgr->set_checkpoint_flag(flag);

  string str = string("ll_phys_log_set_checkpoint_flag finished\n");
  WRITE_DEBUG_LOG(str.c_str());

#endif  
}

void ll_phys_log_set_phys_log_flag(bool flag)
{
#ifdef PHYS_LOG
  is_write_to_phys_log = flag;
#endif
}

void ll_phys_log_set_checkpoint_on_flag(bool flag)
{
#ifdef PHYS_LOG
  phys_log_mgr->set_checkpoint_on_flag(flag);
#endif
}

void ll_phys_log_set_empty_bulk_load_blks()
{
#ifdef PHYS_LOG
  for (int i=0; i<CHARISMA_MAX_TRNS_NUMBER; i++)
    phys_log_mgr->set_empty_bulk_load_blk(i, '1');
#endif
}

void ll_phys_log_set_ph_bu_to_ph(bool ph_bu_to_ph)
{
#ifdef PHYS_LOG
   phys_log_mgr->set_ph_bu_to_ph(ph_bu_to_ph);
#endif
}
