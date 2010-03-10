 /*
 * File:  trmgr.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>
#include <vector>
#include <iostream>

#include "common/sedna.h"

#include "common/u/usem.h"
#include "common/u/uevent.h"
#include "common/u/uthread.h"
#include "common/u/uprocess.h"
#include "common/u/uutils.h"
#include "sm/bufmgr/bm_core.h"
#include "common/u/uprocess.h"
#include "common/base.h"
#include "sm/trmgr.h"
#include "sm/sm_globals.h"
#include "common/errdbg/d_printf.h"
#include "common/tr_debug.h"
#include "common/u/uutils.h"
#include "sm/bufmgr/bm_functions.h"
#include "common/SSMMsg.h"
#include "sm/trmgr.h"
#include "sm/sm_functions.h"
#include "sm/llsm/physlog.h"
#include "sm/llsm/llMain.h"
#include "common/ipc_ops.h"

#include "common/rcv_test.h"

using namespace std;

/* global variables for checkpoint */
USemaphore concurrent_trns_sem;

USemaphore wait_for_recovery;

UTHANDLE  checkpoint_thread_dsc;

static volatile bool shutdown_checkpoint_thread = false;
volatile bool shutdown_event_call = false;
volatile bool is_recovery_mode = false;

UEvent start_checkpoint_snapshot; // this event signals the need for checkpoint/snapshot advancement
UEvent end_of_rotr_event; // this event signals

/* global variables for transaction ids table */
USemaphore trn_table_ids_sync_sem;
USemaphore trns_num_sem;
vector<transaction_id> _ids_table_;

/*****************************************************************************
                  !!! Checkpoint functions
******************************************************************************/

// some very rough criterion for snapshot advancement
//#define UPDATED_PART_THRESHOLD 0.25
#define UPDATED_PART_THRESHOLD sm_globals::upd_crt
#define CREATED_PART_THRESHOLD 0.25

static bool SnapshotAdvanceCriterion()
{
	WuSnapshotStats wuStats;

    // obtain very rough statistics :)
    WuGatherSnapshotsStatsExn(&wuStats);

    size_t updatedBlocks = wuStats.curSnapshotVersionsCount;
    size_t blocksCount = mb->data_file_cur_size / PAGE_SIZE - wuStats.versionsCount;
    size_t createdBlocks = 0; // TODO: add this statistics in proper manner

    double updFract = (double)updatedBlocks / (blocksCount - createdBlocks);
    double crtFract = (double)createdBlocks / (blocksCount - createdBlocks);

    // first rule: some part of the database is updated
    if (updFract >= UPDATED_PART_THRESHOLD) return true;
    // second rule: portion of a new data is created
    if (crtFract >= CREATED_PART_THRESHOLD) return true;

    return false;
}

// this function tries to advance snapshots; if fails it will wait for signaled event on ro-transaction end
void AdvanceSnapshots()
{
	while (!WuTryAdvanceSnapshotsExn())
	{
   		if (UEventWait(&end_of_rotr_event,  __sys_call_error) != 0)
   			throw SYSTEM_EXCEPTION("Checkpoint or snapshot advancement thread waiting failed");
	}
	RECOVERY_CRASH;
//	ll_updateMinRcvLSN();
}

/* Important notes on checkpoints during transaction activity.
 * READ BEFORE ENABLING SUCH CHECKPOINTS!
 *
 * Problems:
 *  1) Checkpoint log records may be inconsistent in case there are more than one. This happens if physical records
 *      and/or transaction commits are enabled during version enumeration procedure. We must be sure we flush checkpoint
 *      info in log header only after all corresponding checkpoint records are in journal (enumeration is done).
 *  2) There can be working versions when we advance snapshots. If last snapshot becomes committed one we must include info
 *      about such versions in log since working versions must be discarded on recovery. In other words we must replace
 *      working versions with last committed ones. We do it as our usual routine anyway but info on working versions
 *      would not be in garbage lists at the moment. Maybe we should analyze function list during enumeration?
 *  3) If there are working transactions then min_rcv_lsn will be some time in the past. Since on recovery we'd go from that
 *      time we must be sure we don't redo transactions committed before checkpoint (with min_rcv_lsn < lsn's < checkpoint_lsn).
 *  4) Consider this: AdvanceSnapshot() -> delete_lc_version X (w/o creating working one) -> commit -> enumeration. X will be
 *      considered as garbage during recovery since it is a bogus version. This is incorrect. Possibly we must disable commits
 *      during (Advance; enumeration] interval.
 */

// new checkpoint and snaphot advancer thread
U_THREAD_PROC (checkpoint_thread, arg)
{
#ifdef CHECKPOINT_ON
	bool isGiantLockObtained = false;
 try
 {

    while (true)
    {
        if (UEventWait(&start_checkpoint_snapshot,  __sys_call_error) != 0)
           throw SYSTEM_EXCEPTION("Checkpoint or snapshot advancement thread waiting failed");
        
        shutdown_event_call = shutdown_checkpoint_thread;
        
        // we come to this thread at the end of each updater transaction to check if we need to
        // make checkpoint or advance snapshots
        
        // we do checkpoint if:
        //	1) when transaction says so (currently only at commit via se:checkpoint() user call or bulk-load optimizations)
        //  2) when se_sm is being shutdowned
        //  3) when we need truncation of log
        
        // we advance snapshots only if SnapshotAdvanceCriterion() says so
        if (shutdown_event_call || llGetCheckpointActiveFlag()) // checkpoint is needed
        {
            for (int i=0; i<CHARISMA_MAX_TRNS_NUMBER; i++)
            {
                if (USemaphoreDown(concurrent_trns_sem, __sys_call_error) !=0 )
                 throw SYSTEM_EXCEPTION("Can't down semaphore concurrent micro ops number semaphore");
            }
            d_printf1("All checkpoint sems acquired\n");
            
            elog(EL_LOG, ("Starting checkpoint procedure"));
        
            WuSnapshotStats wuStats;
            // obtain timestamps of persistent and current snapshots
            WuGatherSnapshotsStatsExn(&wuStats);
        
            /* It is critical for now to advance snapshots before checkpoint:
             *     1) When we relocate persistent version, we check only if LC equals persistent one
             *     2) min_rcv_lsn in logical log for now doesn't reflect the situation -- we could have LC versions younger than persistent ones,
             *        so in that case we should start recovery from some point past in time to consider some committed transactions;
             *        for now, though, min_rcv_lsn takes into consideration only active transactions
             *        
             */
            AdvanceSnapshots(); // Persistent snapshot will be the most recent one
        
            WuOnBeginCheckpointExn();
        
            RECOVERY_CRASH;
        
            ObtainGiantLock(); isGiantLockObtained = true;
            {
                d_printf1("flushing all data buffers...\n");
                flush_data_buffers();
                d_printf1("flushing all data buffers completed\n");
        
                WuEnumerateVersionsParams params;
                WuEnumerateVersionsForCheckpointExn(&params, llLogCheckpoint);
        
                RECOVERY_CRASH;
        
                d_printf1("flushing logical log...\n");
                llFlushAll();
                d_printf1("flushing logical log completed\n");
        
                RECOVERY_CRASH;
        
                llTruncateLog();
        
                WuOnCompleteCheckpointExn();
            }
            ReleaseGiantLock(); isGiantLockObtained = false;
        
            for (int i=0; i<CHARISMA_MAX_TRNS_NUMBER; i++)
                if (USemaphoreUp(concurrent_trns_sem, __sys_call_error) !=0 )
                 throw SYSTEM_EXCEPTION("Can't up semaphore concurrent micro ops number semaphore");
        
            llOnCheckpointFinish();
        
            elog(EL_LOG, ("Checkpoint procedure is finished"));
        }
        else // we need only snapshot advance (subject to criterion)
        {
            if (!is_recovery_mode && SnapshotAdvanceCriterion())
            {
                for (int i=0; i<CHARISMA_MAX_TRNS_NUMBER; i++)
                {
                    if (USemaphoreDown(concurrent_trns_sem, __sys_call_error) !=0 )
                     throw SYSTEM_EXCEPTION("Can't down semaphore concurrent micro ops number semaphore");
                }
                d_printf1("All advancement sems acquired\n");
        
                elog(EL_LOG, ("Starting snapshot advancement"));
                ObtainGiantLock(); isGiantLockObtained = true;
                {
                    AdvanceSnapshots();
                }
                ReleaseGiantLock(); isGiantLockObtained = false;
        
                RECOVERY_CRASH;
        
                for (int i=0; i<CHARISMA_MAX_TRNS_NUMBER; i++)
                    if (USemaphoreUp(concurrent_trns_sem, __sys_call_error) !=0 )
                     throw SYSTEM_EXCEPTION("Can't up semaphore concurrent micro ops number semaphore");
                
                elog(EL_LOG, ("Completed snapshot advancement"));
            }
        }
        
        if (shutdown_event_call == true) return 0;
    }
 } 
 catch(SednaException &e) 
 {
   if (isGiantLockObtained) ReleaseGiantLock();
   sedna_soft_fault(e, EL_SM);
 } 
 catch (ANY_SE_EXCEPTION) 
 {
   if (isGiantLockObtained) ReleaseGiantLock();
   sedna_soft_fault(EL_SM);
 }
#endif

 return 0;
}


void start_chekpoint_thread()
{
#ifdef CHECKPOINT_ON

  if (0 != uCreateThread(checkpoint_thread, NULL, &checkpoint_thread_dsc, CHEKPOINT_THREAD_STACK_SIZE, NULL, __sys_call_error))
     throw USER_EXCEPTION2(SE4060, "checkpoint thread");

  shutdown_checkpoint_thread = false;

#endif
}

void init_checkpoint_sems()
{
#ifdef CHECKPOINT_ON
//  if (USemaphoreCreate(&wait_for_checkpoint, 0, 1, CHARISMA_WAIT_FOR_CHECKPOINT, NULL, __sys_call_error) != 0)
//     throw USER_EXCEPTION2(SE4010, "CHARISMA_WAIT_FOR_CHECKPOINT");
/*
  if (USemaphoreCreate(&checkpoint_finished, 0, 1, SEDNA_CHECKPOINT_FINISHED_SEM, NULL, __sys_call_error) != 0)
     throw USER_EXCEPTION2(SE4010, "SEDNA_CHECKPOINT_FINISHED_SEM");


  if (USemaphoreCreate(&checkpoint_sem, 1, 1, CHARISMA_CHECKPOINT_SEM, NULL, __sys_call_error) != 0)
     throw USER_EXCEPTION2(SE4010, "CHARISMA_CHECKPOINT_SEM");
*/
  USECURITY_ATTRIBUTES *sa;
  if(uCreateSA(&sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error)!=0) throw USER_EXCEPTION(SE3060);

  if (0 != UEventCreate(&start_checkpoint_snapshot, sa, U_AUTORESET_EVENT, false, SNAPSHOT_CHECKPOINT_EVENT, __sys_call_error))
     throw USER_EXCEPTION2(SE4010, "SNAPSHOT_CHECKPOINT_EVENT");

  if (0 != UEventCreate(&end_of_rotr_event, sa, U_AUTORESET_EVENT, false, TRY_ADVANCE_SNAPSHOT_EVENT, __sys_call_error))
     throw USER_EXCEPTION2(SE4010, "TRY_ADVANCE_SNAPSHOT_EVENT");

  if(uReleaseSA(sa, __sys_call_error)!=0) throw USER_EXCEPTION(SE3063);

  if (USemaphoreCreate(&concurrent_trns_sem, CHARISMA_MAX_TRNS_NUMBER, CHARISMA_MAX_TRNS_NUMBER, SEDNA_TRNS_FINISHED, NULL, __sys_call_error) != 0)
     throw USER_EXCEPTION2(SE4010, "SEDNA_TRNS_FINISHED");
#endif
}

void shutdown_chekpoint_thread()
{
#ifdef CHECKPOINT_ON
  //shutdown thread
  shutdown_checkpoint_thread = true;

//  if (USemaphoreUp(wait_for_checkpoint, __sys_call_error) !=0)
//     throw SYSTEM_EXCEPTION("Can't Up WAIT_FOR_CHECKPOINT semaphore");

  if (UEventSet(&start_checkpoint_snapshot,  __sys_call_error) != 0)
     throw SYSTEM_EXCEPTION("Event signaling for checkpoint_snapshot thread shutdown failed");

  if (uThreadJoin(checkpoint_thread_dsc, __sys_call_error) != 0)
     throw USER_EXCEPTION(SE4210);


  if (uCloseThreadHandle(checkpoint_thread_dsc, __sys_call_error) != 0)
     throw USER_EXCEPTION2(SE4063, "checkpoint thread");
#endif
}

void release_checkpoint_sems()
{
#ifdef CHECKPOINT_ON
  //release semaphores
//  if (USemaphoreRelease(wait_for_checkpoint, __sys_call_error) != 0)
//     throw USER_EXCEPTION2(SE4011, "CHARISMA_WAIT_FOR_CHECKPOINT");
/*
  if (USemaphoreRelease(checkpoint_finished, __sys_call_error) != 0)
     throw USER_EXCEPTION2(SE4011, "SEDNA_CHECKPOINT_FINISHED_SEM");
*/

//  if (USemaphoreRelease(checkpoint_sem, __sys_call_error) != 0)
//     throw USER_EXCEPTION2(SE4011, "CHARISMA_CHECKPOINT_SEM");

  if (USemaphoreRelease(concurrent_trns_sem, __sys_call_error) != 0)
     throw USER_EXCEPTION2(SE4011, "CHEKPOINT_THREAD_STACK_SIZE");


  if (UEventCloseAndUnlink(&start_checkpoint_snapshot, __sys_call_error) != 0)
     throw USER_EXCEPTION2(SE4011, "SNAPSHOT_CHECKPOINT_EVENT");

  if (UEventCloseAndUnlink(&end_of_rotr_event, __sys_call_error) != 0)
     throw USER_EXCEPTION2(SE4011, "TRY_ADVANCE_SNAPSHOT_EVENT");

#endif
}


void execute_recovery_by_logical_log_process(LSN last_checkpoint_lsn)
{
#ifdef RECOVERY_ON
  if (USemaphoreCreate(&wait_for_recovery, 0, 1, CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG, NULL, __sys_call_error) != 0)
     throw USER_EXCEPTION2(SE4010, "CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG");

  //create recovery process
  int res, res2;
  char buf[U_MAX_PATH + SE_MAX_DB_NAME_LENGTH + 16];
  char buf2[1024];
  UPID pid;
  UPHANDLE h;

  string command_line = uGetImageProcPath(buf, __sys_call_error) +
                        string("/se_trn ") + string(sm_globals::db_name) + string(" dummy");

  strcpy(buf, command_line.c_str());

  uSetEnvironmentVariable(SEDNA_OS_PRIMITIVES_ID_MIN_BOUND,
                          u_itoa(GOV_HEADER_GLOBAL_PTR -> os_primitives_id_min_bound, buf2, 10),
                          NULL,
                          __sys_call_error);

  uSetEnvironmentVariable(SEDNA_RUN_RECOVERY_TRANSACTION,
                          "1",
                          NULL,
                          __sys_call_error);

#ifndef TEST_RECOVERY_ON
  res = uCreateProcess(
                  buf,
                  false,
                  NULL,
                  U_DETACHED_PROCESS/*0*/,
                  &h,
                  NULL,
                  &pid,
                  NULL,
                  NULL,
                  __sys_call_error);

  if (res != 0)
     throw USER_EXCEPTION2(SE4070,"recovery process");
#endif

  for (;;)
  {
     res =USemaphoreDownTimeout(wait_for_recovery, 15000, __sys_call_error);
     if (res == 0) break;
     else if (res == 1)
        throw USER_EXCEPTION2(SE4015, "CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG");
     else // timeout expired
     {
       /* We need this call since uIsProcessExist returns 1 on zombies! */
       uNonBlockingWaitForChildProcess(pid);
       res2 = uIsProcessExist(pid, h, __sys_call_error);
       if (res2 != 0) throw USER_EXCEPTION(SE4501);
     }
  }

#endif
}



/*****************************************************************************
                        Transaction identifiers table functions
******************************************************************************/
void init_transaction_ids_table()
{
#ifdef TRMGR_ON
  if (0 != USemaphoreCreate(&trn_table_ids_sync_sem, 1, 1, CHARISMA_SYNC_TRN_IDS_TABLE, NULL, __sys_call_error))
     throw USER_EXCEPTION2(SE4010, "CHARISMA_SYNC_TRN_IDS_TABLE");

  for (size_t i=0; i< CHARISMA_MAX_TRNS_NUMBER; i++)
      _ids_table_.push_back(i);

#endif
}

void release_transaction_ids_table()
{
#ifdef TRMGR_ON
  if (0 != USemaphoreRelease(trn_table_ids_sync_sem, __sys_call_error))
     throw USER_EXCEPTION2(SE4011, "CHARISMA_SYNC_TRN_IDS_TABLE");

  _ids_table_.clear();
#endif
}

transaction_id get_transaction_id()
{
#ifdef TRMGR_ON


  if (USemaphoreDown(trn_table_ids_sync_sem, __sys_call_error) != 0)
     throw SYSTEM_EXCEPTION("Can't down semaphore: CHARISMA_SYNC_TRN_IDS_TABLE");


  transaction_id id;
  if (_ids_table_.empty())
     id = -1;
  else
  {
     id = _ids_table_.back();
     _ids_table_.pop_back();
  }


  if (USemaphoreUp(trn_table_ids_sync_sem, __sys_call_error) != 0)
     throw SYSTEM_EXCEPTION("Can't up semaphore: CHARISMA_SYNC_TRN_IDS_TABLE");

  d_printf2("get trid=%d\n", id);

  return id;
#endif
}

void give_transaction_id(transaction_id& trid)
{
#ifdef TRMGR_ON
  d_printf2("return trid=%d\n", trid);


  if (USemaphoreDown(trn_table_ids_sync_sem, __sys_call_error) != 0)
     throw SYSTEM_EXCEPTION("Can't down semaphore: CHARISMA_SYNC_TRN_IDS_TABLE");

  if (!(trid < 0 || trid >= CHARISMA_MAX_TRNS_NUMBER))
  {//check that there is no the same identifier
     for (size_t i=0; i< _ids_table_.size(); i++)
     {
         if (_ids_table_[i] == trid)
            throw SYSTEM_EXCEPTION("Error in logic of Transaction Identifiers Table (double identifier)");
     }

     _ids_table_.push_back(trid);
  }


  if (USemaphoreUp(trn_table_ids_sync_sem, __sys_call_error) != 0)
     throw SYSTEM_EXCEPTION("Can't up semaphore: CHARISMA_SYNC_TRN_IDS_TABLE");
#endif
}

