 /*
 * File:  trmgr.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>
#include <vector>
#include <iostream>

#include "common/sedna.h"

#include "common/u/usem.h"
#include "common/u/umutex.h"
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
#include "common/llcommon/llMain.h"
#include "common/ipc_ops.h"

#include "common/rcv_test.h"

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
                for (int i=0; i<SEDNA_MAX_TRNS_NUMBER; i++)
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
                    /* Just throw out from SM's buffers dirty pages */
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
        
                for (int i=0; i<SEDNA_MAX_TRNS_NUMBER; i++)
                    if (USemaphoreUp(concurrent_trns_sem, __sys_call_error) !=0 )
                     throw SYSTEM_EXCEPTION("Can't up semaphore concurrent micro ops number semaphore");
                
                llOnCheckpointFinish();

                elog(EL_LOG, ("Checkpoint procedure is finished"));
            }
            else // we need only snapshot advance (subject to criterion)
            {
                if (!is_recovery_mode && SnapshotAdvanceCriterion())
                {
                    for (int i=0; i<SEDNA_MAX_TRNS_NUMBER; i++)
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

                    for (int i=0; i<SEDNA_MAX_TRNS_NUMBER; i++)
                        if (USemaphoreUp(concurrent_trns_sem, __sys_call_error) !=0 )
                         throw SYSTEM_EXCEPTION("Can't up semaphore concurrent micro ops number semaphore");

                    elog(EL_LOG, ("Completed snapshot advancement"));
                }
            }
        
            if (shutdown_event_call) return 0;
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

    return 0;
}


void start_chekpoint_thread()
{
    if (0 != uCreateThread(checkpoint_thread, NULL, &checkpoint_thread_dsc, CHEKPOINT_THREAD_STACK_SIZE, NULL, __sys_call_error))
        throw USER_EXCEPTION2(SE4060, "checkpoint thread");

    shutdown_checkpoint_thread = false;
}

void init_checkpoint_sems()
{
    USECURITY_ATTRIBUTES *sa;

    if (uCreateSA(&sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error) != 0)
        throw USER_EXCEPTION(SE3060);

    if (0 != UEventCreate(&start_checkpoint_snapshot, sa, U_AUTORESET_EVENT,
                          false, SNAPSHOT_CHECKPOINT_EVENT, __sys_call_error))
        throw USER_EXCEPTION2(SE4010, "SNAPSHOT_CHECKPOINT_EVENT");

    if (0 != UEventCreate(&end_of_rotr_event, sa, U_AUTORESET_EVENT, false,
                          TRY_ADVANCE_SNAPSHOT_EVENT, __sys_call_error))
        throw USER_EXCEPTION2(SE4010, "TRY_ADVANCE_SNAPSHOT_EVENT");

    if (uReleaseSA(sa, __sys_call_error) != 0)
        throw USER_EXCEPTION(SE3063);

    if (USemaphoreCreate(&concurrent_trns_sem, SEDNA_MAX_TRNS_NUMBER,
                         SEDNA_MAX_TRNS_NUMBER, SEDNA_TRNS_FINISHED, NULL,
                         __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4010, "SEDNA_TRNS_FINISHED");
}

void shutdown_chekpoint_thread()
{
    //shutdown thread
    shutdown_checkpoint_thread = true;

    if (UEventSet(&start_checkpoint_snapshot,  __sys_call_error) != 0)
        throw SYSTEM_EXCEPTION("Event signaling for checkpoint_snapshot thread shutdown failed");

    if (uThreadJoin(checkpoint_thread_dsc, __sys_call_error) != 0)
        throw USER_EXCEPTION(SE4210);

    if (uCloseThreadHandle(checkpoint_thread_dsc, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4063, "checkpoint thread");
}

void release_checkpoint_sems()
{
    if (USemaphoreRelease(concurrent_trns_sem, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4011, "SEDNA_TRNS_FINISHED");

    if (UEventCloseAndUnlink(&start_checkpoint_snapshot, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4011, "SNAPSHOT_CHECKPOINT_EVENT");

    if (UEventCloseAndUnlink(&end_of_rotr_event, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4011, "TRY_ADVANCE_SNAPSHOT_EVENT");
}

void execute_recovery_by_logical_log_process(LSN last_checkpoint_lsn)
{
    int res, res2;
    char buf[U_MAX_PATH + SE_MAX_DB_NAME_LENGTH + 16];
    char buf2[1024];
    UPID pid;
    UPHANDLE h;

    try
    {
        if (USemaphoreCreate(&wait_for_recovery, 0, 1, SEDNA_DB_RECOVERED_BY_LOGICAL_LOG, NULL, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4010, "SEDNA_DB_RECOVERED_BY_LOGICAL_LOG");

        string command_line = uGetImageProcPath(buf, __sys_call_error) 
                            + string("/") + SESSION_EXE
                            + string(" -db-id ") + int2string(sm_globals::db_id) 
                            + string(" -min-bound ")+ int2string(sm_globals::os_primitives_min_bound) 
                            + string(" -max-stack-depth ") + int2string(sm_globals::max_stack_depth) 
                            + string(" -sedna-data ") + string(sm_globals::sedna_data) 
                            + string(" ") 
                            + string(sm_globals::db_name) 
                            + string(" dummy");
        elog(EL_LOG, (command_line.c_str()));

        strcpy(buf, command_line.c_str());

//         uSetEnvironmentVariable(SEDNA_OS_PRIMITIVES_ID_MIN_BOUND,
//                                 u_itoa(sm_globals::os_primitives_min_bound, buf2, 10),
//                                 NULL,
//                                 __sys_call_error);

        // to make se_trn run recovery logic
        uSetEnvironmentVariable(SEDNA_RUN_RECOVERY_TRANSACTION,
                                "1",
                                NULL,
                                __sys_call_error);

        res = uCreateProcess(
                      buf,
                      false,
                      NULL,
                      0, //U_DETACHED_PROCESS/*0*/,
                      &h,
                      NULL,
                      &pid,
                      NULL,
                      NULL,
                      __sys_call_error);

        if (res != 0)
            throw USER_EXCEPTION2(SE4070,"recovery process");

        // wait for recovery to end
        for (;;)
        {
            res = USemaphoreDownTimeout(wait_for_recovery, 15000, __sys_call_error);
            if (res == 0) /* normal down */
            {
                break;
            }
            else if (res == 2) /* timeout */
            {
                /* We need this call since uIsProcessExist returns 1 on zombies! */
                uNonBlockingWaitForChildProcess(pid);
                res2 = uIsProcessExist(pid, h, __sys_call_error);
                if (res2 != 0) throw USER_EXCEPTION(SE4501);
            }
            else /* error */
            {
                throw USER_EXCEPTION2(SE4015, "SEDNA_DB_RECOVERED_BY_LOGICAL_LOG");
            }
        }
    }
    catch(SednaException &e)
    {
        if (USemaphoreRelease(wait_for_recovery, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4011, "SEDNA_DB_RECOVERED_BY_LOGICAL_LOG");

        sedna_soft_fault(e, EL_SM);
    }

    if (USemaphoreRelease(wait_for_recovery, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4011, "SEDNA_DB_RECOVERED_BY_LOGICAL_LOG");
}

/*****************************************************************************
                        Transaction identifiers table functions
******************************************************************************/
static size_t ro_tr_count = 0;

void init_transaction_ids_table()
{
    if (uMutexInit(&trid_tbl_sync, __sys_call_error) != 0)
        throw SYSTEM_EXCEPTION("cannot initialize trid table sync mutex");

    for (transaction_id i=0; i< SEDNA_MAX_TRNS_NUMBER; i++)
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

    res = SEDNA_MAX_TRNS_NUMBER - _ids_table_.size();

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

    if (!(trid < 0 || trid >= SEDNA_MAX_TRNS_NUMBER))
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
    char buf[1024];

    // on exit from exclusive mode we want to unblock all waiting sessions
    for (size_t i = 0; i < wait_list.size(); i++)
    {
        if (USemaphoreOpen(&sem, SEDNA_TRANSACTION_LOCK(wait_list[i], buf, 1024), __sys_call_error) != 0)
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
    char buf[1024];

    U_ASSERT(exclusive_sid != -1);

    // wake exclusive transaction only if we've got no other updaters
    if (get_active_transaction_num(true) == 1)
    {
        if (USemaphoreOpen(&sem, SEDNA_TRANSACTION_LOCK(exclusive_sid, buf, 1024), __sys_call_error) != 0)
            throw SYSTEM_EXCEPTION("Can't open session waiting semaphore");
        if (USemaphoreUp(sem, __sys_call_error) != 0)
            throw SYSTEM_EXCEPTION("Can't up session waiting semaphore");
        if (USemaphoreClose(sem, __sys_call_error) != 0)
            throw SYSTEM_EXCEPTION("Can't close session waiting semaphore");
    }
}
