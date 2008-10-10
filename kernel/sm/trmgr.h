/*
 * File:  trmgr.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _TR_MGR_H
#define _TR_MGR_H

#include "common/sedna.h"
#include "common/base.h"

#include "common/u/uevent.h"

#include "common/lfsGlobals.h"

#ifdef _WIN32
#define CHEKPOINT_THREAD_STACK_SIZE		10024
#else
#define CHEKPOINT_THREAD_STACK_SIZE		102400
#endif

#define TRMGR_ON
//must be uncommneted
#define CHECKPOINT_ON
//#define TEST_CHECKPOINT_ON
//must be uncommneted
#define RECOVERY_ON
//must be uncommneted
//#define TEST_RECOVERY_ON
//must be uncommneted
#define RECOVERY_EXEC_MICRO_OP

void start_chekpoint_thread();
void init_checkpoint_sems();
void shutdown_chekpoint_thread();
void release_checkpoint_sems();
void execute_recovery_by_logical_log_process(LSN last_checkpoint_lsn);
void recover_database_by_physical_and_logical_log(int db_id);

void init_transaction_ids_table();
void release_transaction_ids_table();
transaction_id get_transaction_id();
void give_transaction_id(transaction_id& trid);

//extern USemaphore wait_for_checkpoint; 
extern USemaphore checkpoint_finished;

extern UEvent start_checkpoint_snapshot;
extern UEvent end_of_rotr_event;

volatile extern bool is_recovery_mode;

int PhOnInitialSnapshotCreate(TIMESTAMP ts);
int PhOnSnapshotCreate(TIMESTAMP ts);
void PhOnSnapshotDelete(TIMESTAMP ts, bool isDelete = true);
int GetPhIndex(TIMESTAMP ts);
#endif
