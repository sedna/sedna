/*
 * File:  plmgr.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PLMGR_H
#define _PLMGR_H

#include "common/sedna.h"
#include "common/base.h"
#include "sm/plmgr/plmgr_core.h"
#include "sm/plmgr/sm_plmgr.h"

extern bool is_write_to_phys_log;
extern sm_plmgr* phys_log_mgr;

// block change
void ll_phys_log_change(void *p, shft size);

// decrease size of the file
void ll_phys_log_decrease(__int64 old_size);

// create empty node block
//void ll_phys_log_create_node_blk(void *p);

// flush block to disk (function is used for implementing WAL)
void ll_phys_log_flush_blk(void *p, bool sync = true);

//flush phys log
void ll_phys_log_flush(bool sync = true);

// save master block in log
void ll_phys_log_master_blk(void *p, shft size);

// recover database after soft fail, function returns lsn of last completed checkpoint in logical log
LONG_LSN ll_phys_log_recover_db();

// clear physical log
void ll_phys_log_clear(const LONG_LSN& last_cp_lsn, bool sync = true);

bool ll_phys_log_startup(int& sedna_db_version);

void ll_phys_log_startup_shared_mem();

void ll_phys_log_shutdown();


void ll_phys_log_set_checkpoint_flag(bool flag);

void ll_phys_log_set_phys_log_flag(bool flag);

void ll_phys_log_set_checkpoint_on_flag(bool flag);

void ll_phys_log_set_empty_bulk_load_blks();

void ll_phys_log_set_ph_bu_to_ph(bool ph_bu_to_ph);
#endif

