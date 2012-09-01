 /*
 * File:  trmgr.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "trmgr.h"

#include "common/errdbg/d_printf.h"
#include "common/structures/config_data.h"

#include "u/usem.h"
#include "u/umutex.h"
#include "u/uevent.h"
#include "u/uthread.h"
#include "u/uprocess.h"
#include "u/uutils.h"

#include "sm/bufmgr/bm_core.h"
#include "sm/bufmgr/bm_functions.h"
#include "sm/trmgr.h"
#include "sm/llsm/physlog.h"
#include "sm/lockwarden.h"
#include "sm/sm_globals.h"

#include "common/protocol/int_sp.h"
#include "auxiliary/processwarden.h"

#include <string>
#include <vector>
#include <iostream>


using namespace std;

/* global variables for checkpoint */
static USemaphore concurrent_trns_sem;
static UTHANDLE  checkpoint_thread_dsc;
static volatile bool shutdown_checkpoint_thread = false;
volatile bool shutdown_event_call = false;
volatile bool is_recovery_mode = false;

UEvent start_checkpoint_snapshot; // this event signals the need for checkpoint/snapshot advancement
UEvent end_of_rotr_event; // this event signals

static USemaphore wait_for_recovery;

/* global variables for transaction ids table */
static uMutexType trid_tbl_sync;
static vector<transaction_id> _ids_table_;

/*****************************************************************************
                  !!! Checkpoint functions
******************************************************************************/

#define CHEKPOINT_THREAD_STACK_SIZE     102400

#define UPDATED_PART_THRESHOLD (databaseOptions->updateCriteria)
#define CREATED_PART_THRESHOLD 0.25

static bool SnapshotAdvanceCriterion()
{
	WuSnapshotStats wuStats;

    // obtain very rough statistics :)
    WuGatherSnapshotsStatsExn(&wuStats);

    size_t updatedBlocks = wuStats.curSnapshotVersionsCount;
    size_t blocksCount = (size_t)(mb->data_file_cur_size / PAGE_SIZE - wuStats.versionsCount);
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
    while (!WuTryAdvanceSnapshotsExn()) {
        if (UEventWait(&end_of_rotr_event,  __sys_call_error) != 0) {
            throw SYSTEM_EXCEPTION("Checkpoint or snapshot advancement thread waiting failed");
        }
    }
//    ll_updateMinRcvLSN();
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
                for (int i=0; i < SEDNA_MAX_TRN_NUMBER; i++)
                {
                    if (USemaphoreDown(concurrent_trns_sem, __sys_call_error) !=0 )
                        throw SYSTEM_EXCEPTION("Can't down semaphore concurrent micro ops number semaphore");
                }
        
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

                {
                    GaintLockWarden gaintLock(true);

                    elog(EL_INFO, ("Flushing all data buffers..."));
                    /* Just throw out from SM's buffers dirty pages */
                    flush_data_buffers();
                    elog(EL_INFO, ("Flushing all data buffers completed"));
        
                    WuEnumerateVersionsParams params;
                    WuEnumerateVersionsForCheckpointExn(&params, llLogCheckpoint);
        
                    RECOVERY_CRASH;
        
                    elog(EL_INFO, ("Flushing logical log...\n"));
                    llFlushAll();
                    elog(EL_INFO, ("Flushing logical log completed"));
        
                    RECOVERY_CRASH;
        
                    llTruncateLog();
        
                    WuOnCompleteCheckpointExn();
                    gaintLock.release();
                }
        
                for (int i=0; i < SEDNA_MAX_TRN_NUMBER; i++)
                    if (USemaphoreUp(concurrent_trns_sem, __sys_call_error) !=0 )
                     throw SYSTEM_EXCEPTION("Can't up semaphore concurrent micro ops number semaphore");
                
                llOnCheckpointFinish();

                elog(EL_LOG, ("Checkpoint procedure is finished"));
            }
            else // we need only snapshot advance (subject to criterion)
            {
                if (!is_recovery_mode && SnapshotAdvanceCriterion())
                {
                    for (int i=0; i < SEDNA_MAX_TRN_NUMBER; i++) {
                        if (USemaphoreDown(concurrent_trns_sem, __sys_call_error) !=0 ) {
                            throw SYSTEM_EXCEPTION("Can't down semaphore concurrent micro ops number semaphore");
                        }
                    }

                    elog(EL_LOG, ("Starting snapshot advancement"));
                    {
                        GaintLockWarden gaintLock(true);
                        AdvanceSnapshots();
                        gaintLock.release();
                    }

                    RECOVERY_CRASH;

                    for (int i = 0; i < SEDNA_MAX_TRN_NUMBER; i++) {
                        if (USemaphoreUp(concurrent_trns_sem, __sys_call_error) !=0 ) {
                            throw SYSTEM_EXCEPTION("Can't up semaphore concurrent micro ops number semaphore");
                        }
                    }

                    elog(EL_LOG, ("Completed snapshot advancement"));
                }
            }
        
            if (shutdown_event_call) return 0;
        }
    }
    catch(SednaException &e)
    {
        sedna_soft_fault(e, EL_SM);
    }
    catch (ANY_SE_EXCEPTION)
    {
        sedna_soft_fault(EL_SM);
    }

    return 0;
}


void start_chekpoint_thread()
{
    if (0 != uCreateThread(checkpoint_thread, NULL, &checkpoint_thread_dsc, CHEKPOINT_THREAD_STACK_SIZE, NULL, __sys_call_error))
        throw USER_EXCEPTION2(SE4060, "checkpoint thread");

    shutdown_checkpoint_thread = false;
}

static global_name tryAdvanceSnapshotEventName = {GN_DATABASE, "try_advance_snapshot"};

void init_checkpoint_sems()
{
    USECURITY_ATTRIBUTES *sa;

    CHECK_ENV(uCreateSA(&sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error),
        SE3060, "");

    CHECK_ENV(UEventCreate(&start_checkpoint_snapshot, sa, U_AUTORESET_EVENT, false, checkpointEventName, __sys_call_error),
        SE4010, checkpointEventName.name);

    CHECK_ENV(UEventCreate(&end_of_rotr_event, sa, U_AUTORESET_EVENT, false, tryAdvanceSnapshotEventName, __sys_call_error),
        SE4010, tryAdvanceSnapshotEventName.name);

    CHECK_ENV(uReleaseSA(sa, __sys_call_error), SE3063, "");

    CHECK_ENV(USemaphoreCreate(&concurrent_trns_sem, SEDNA_MAX_TRN_NUMBER, SEDNA_MAX_TRN_NUMBER, activeTrnCounterSem, NULL, __sys_call_error),
        SE4010, activeTrnCounterSem.name);
}

void shutdown_chekpoint_thread()
{
    //shutdown thread
    shutdown_checkpoint_thread = true;

    CHECK_ENV(UEventSet(&start_checkpoint_snapshot,  __sys_call_error),
        SE1000, "Event signaling for checkpoint_snapshot thread shutdown failed");

    CHECK_ENV(uThreadJoin(checkpoint_thread_dsc, __sys_call_error),
        SE4210, "Checkpoint thread");

    CHECK_ENV(uCloseThreadHandle(checkpoint_thread_dsc, __sys_call_error),
        SE4063, "Checkpoint thread");
}

void release_checkpoint_sems()
{
    CHECK_ENV(USemaphoreRelease(concurrent_trns_sem, __sys_call_error),
        SE4011, activeTrnCounterSem.name)

    CHECK_ENV(UEventCloseAndUnlink(&start_checkpoint_snapshot, __sys_call_error),
        SE4011, checkpointEventName.name);

    CHECK_ENV(UEventCloseAndUnlink(&end_of_rotr_event, __sys_call_error),
        SE4011, tryAdvanceSnapshotEventName.name);
}

void execute_recovery_by_logical_log_process()
{
    try {
        /* WARNING: There was once semaphore check, but I do not understand why it is needed, so it is commented out
         * PLEASE, see commit history if any problems found.
         */

        ExecuteProcess recoveryProcess(NULL, SESSION_EXE, "--recovery");
        /* All other parameters are passed via environment */

        recoveryProcess.execute(0, false);

        if (0 != recoveryProcess.wait4()) {
            throw USER_EXCEPTION(SE4501);
        };

    }
    catch(SednaException &e)
    {
        sedna_soft_fault(e, EL_SM);
    }
}

/*****************************************************************************
                        Transaction identifiers table functions
******************************************************************************/
static size_t ro_tr_count = 0;

void init_transaction_ids_table()
{
    if (uMutexInit(&trid_tbl_sync, __sys_call_error) != 0)
        throw SYSTEM_EXCEPTION("cannot initialize trid table sync mutex");

    for (transaction_id i=0; i < SEDNA_MAX_TRN_NUMBER; i++)
        _ids_table_.push_back(i);
}

void release_transaction_ids_table()
{
    if (uMutexDestroy(&trid_tbl_sync, __sys_call_error) != 0)
        throw SYSTEM_EXCEPTION("cannot destroy trid table sync mutex");

    _ids_table_.clear();
}

size_t get_active_transaction_num(bool updaters_only)
{
    size_t res;

    if (uMutexLock(&trid_tbl_sync, __sys_call_error) != 0)
        throw SYSTEM_EXCEPTION("cannot obtain trid table sync mutex");

    res = SEDNA_MAX_TRN_NUMBER - _ids_table_.size();

    if (updaters_only) res -= ro_tr_count;

    if (uMutexUnlock(&trid_tbl_sync, __sys_call_error) != 0)
        throw SYSTEM_EXCEPTION("cannot unlock trid table sync mutex");

    return res;
}
transaction_id get_transaction_id(bool is_read_only)
{
    if (uMutexLock(&trid_tbl_sync, __sys_call_error) != 0)
        throw SYSTEM_EXCEPTION("cannot obtain trid table sync mutex");

    transaction_id id;

    if (_ids_table_.empty())
    {
        id = -1;
    }
    else
    {
        id = _ids_table_.back();
        _ids_table_.pop_back();

        if (is_read_only) ro_tr_count++;
    }

    if (uMutexUnlock(&trid_tbl_sync, __sys_call_error) != 0)
        throw SYSTEM_EXCEPTION("cannot unlock trid table sync mutex");

    d_printf2("get trid=%d\n", id);

    return id;
}

void give_transaction_id(transaction_id& trid, bool is_read_only)
{
    d_printf2("return trid=%d\n", trid);

    if (uMutexLock(&trid_tbl_sync, __sys_call_error) != 0)
        throw SYSTEM_EXCEPTION("cannot obtain trid table sync mutex");

    if (!(trid < 0 || trid >= SEDNA_MAX_TRN_NUMBER))
    {//check that there is no the same identifier
        for (size_t i=0; i< _ids_table_.size(); i++)
        {
            if (_ids_table_[i] == trid)
                throw SYSTEM_EXCEPTION("Error in logic of Transaction Identifiers Table (double identifier)");
        }

        _ids_table_.push_back(trid);

        if (is_read_only) ro_tr_count--;
    }

    if (uMutexUnlock(&trid_tbl_sync, __sys_call_error) != 0)
        throw SYSTEM_EXCEPTION("cannot unlock trid table sync mutex");
}

/******************************************************************************
 * Some exclusive mode stuff
 ******************************************************************************/

static session_id exclusive_sid = -1;
static std::vector<session_id> wait_list;

transaction_id xmGetExclusiveModeId()
{
    return exclusive_sid;
}

void xmEnterExclusiveMode(session_id sid)
{
    U_ASSERT(exclusive_sid == -1);
    exclusive_sid = sid;
    xmTryToStartExclusive();
}

void xmExitExclusiveMode()
{
    USemaphore sem;

    // on exit from exclusive mode we want to unblock all waiting sessions
    for (size_t i = 0; i < wait_list.size(); i++)
    {
        /* NOTE: Set valid transaction context for global names */
        SetSessionIdString sessid(wait_list[i]);

        if (USemaphoreOpen(&sem, transactionLockName, __sys_call_error) != 0)
            throw SYSTEM_EXCEPTION("Can't open session waiting semaphore");
        if (USemaphoreUp(sem, __sys_call_error) != 0)
            throw SYSTEM_EXCEPTION("Can't up session waiting semaphore");
        if (USemaphoreClose(sem, __sys_call_error) != 0)
            throw SYSTEM_EXCEPTION("Can't close session waiting semaphore");
    }

    wait_list.clear();

    exclusive_sid = -1;
}

void xmBlockSession(session_id sid)
{
    wait_list.push_back(sid);
}

void xmTryToStartExclusive()
{
    USemaphore sem;

    U_ASSERT(exclusive_sid != -1);

    // wake exclusive transaction only if we've got no other updaters
    if (get_active_transaction_num(true) == 1)
    {
        /* NOTE: Set valid transaction context for global names */
        SetSessionIdString sessid(exclusive_sid);
        
        if (USemaphoreOpen(&sem, transactionLockName, __sys_call_error) != 0)
            throw SYSTEM_EXCEPTION("Can't open session waiting semaphore");
        if (USemaphoreUp(sem, __sys_call_error) != 0)
            throw SYSTEM_EXCEPTION("Can't up session waiting semaphore");
        if (USemaphoreClose(sem, __sys_call_error) != 0)
            throw SYSTEM_EXCEPTION("Can't close session waiting semaphore");
    }
}
