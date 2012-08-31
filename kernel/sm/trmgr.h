/*
 * File:  trmgr.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _TR_MGR_H
#define _TR_MGR_H

#include "common/sedna.h"
#include "common/base.h"
#include "common/llcommon/lfsGlobals.h"

#include "u/uevent.h"

/******************************************************************************
 * Some exclusive mode stuff
 ******************************************************************************/

transaction_id xmGetExclusiveModeId();
void xmEnterExclusiveMode(session_id sid);
void xmExitExclusiveMode();
void xmBlockSession(session_id sid);
void xmTryToStartExclusive();

/******************************************************************************
 * Some checkpoint stuff
 ******************************************************************************/

void start_chekpoint_thread();
void init_checkpoint_sems();
void shutdown_chekpoint_thread();
void release_checkpoint_sems();

extern UEvent start_checkpoint_snapshot;
extern UEvent end_of_rotr_event;

/******************************************************************************
 * Some trid management stuff
 ******************************************************************************/

void init_transaction_ids_table();
void release_transaction_ids_table();
transaction_id get_transaction_id(bool is_read_only);
void give_transaction_id(transaction_id& trid, bool is_read_only);
size_t get_active_transaction_num(bool updaters_only);

/******************************************************************************
 * Some recovery stuff
 ******************************************************************************/

void execute_recovery_by_logical_log_process();

volatile extern bool is_recovery_mode;

#endif /* _TR_MGR_H */
