 /*
 * File:  trmgr.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <string>
#include <vector>
#include <iostream>
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

#include "common/rcv_test.h"

using namespace std;

/* global variables for checkpoint */
//USemaphore wait_for_checkpoint;
USemaphore checkpoint_finished;
USemaphore checkpoint_sem; 
USemaphore concurrent_trns_sem;

USemaphore wait_for_recovery;

UTHANDLE  checkpoint_thread_dsc;

volatile bool shutdown_checkpoint_thread = false;
volatile bool shutdown_event_call = false;
volatile bool is_recovery_mode = false;

UEvent start_checkpoint_snapshot; // this event signals the need for checkpoint/snapshot advancement
UEvent end_of_rotr_event; // this event signals 

/* global variables for transaction ids table */
USemaphore trn_table_ids_sync_sem;
USemaphore trns_num_sem;
vector<transaction_id> _ids_table_;
//int trn_ids[CHARISMA_MAX_TRNS_NUMBER];//values: 0->free; 1->busy

/*****************************************************************************
                  !!! Checkpoint functions
******************************************************************************/

// some very rough criterion for snapshot advancement
//#define UPDATED_PART_THRESHOLD 0.25
#define UPDATED_PART_THRESHOLD upd_crt
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

// new checkpoint and snaphot advancer thread
U_THREAD_PROC (checkpoint_thread, arg)
{
#ifdef CHECKPOINT_ON
	bool isGiantLockObtained = false;
 try{


  int times=1;  
  LSN cp_lsn;
  while (true)
  {
//    if (USemaphoreDown(wait_for_checkpoint, __sys_call_error) !=0 )
//       throw SYSTEM_EXCEPTION("Can't down semaphore for checkpoint wait");
    if (UEventWait(&start_checkpoint_snapshot,  __sys_call_error) != 0)
       throw SYSTEM_EXCEPTION("Checkpoint or snapshot advancement thread waiting failed");

    d_printf2("checkpoint thread procedure started num=%d\n", times);

    shutdown_event_call = shutdown_checkpoint_thread;
    
    // we come to this thread at the end of each updater transaction to check if we need to
    // make checkpoint or advance snapshots
    
    // we do checkpoint if:
    //	1) when transaction says so (currently only at commit via se:checkpoint() user call)
    //  2) when se_sm is being shutdowned
    //  3) when we need truncation of log
    
    // we advance snapshots only if SnapshotAdvanceCriterion() says so
    if (shutdown_event_call || llGetCheckpointActiveFlag() || llNeedCheckpoint()) // checkpoint is needed
    {
	    for (int i=0; i<CHARISMA_MAX_TRNS_NUMBER; i++)    
    	{
        	if (USemaphoreDown(concurrent_trns_sem, __sys_call_error) !=0 )
	         throw SYSTEM_EXCEPTION("Can't down semaphore concurrent micro ops number semaphore");
		
        	d_printf2("Sem %d acquired\n", i);

	    }
    	d_printf1("All sems acquired\n");

		WuSnapshotStats wuStats;
   		// obtain timestamps of persistent and current snapshots
   		WuGatherSnapshotsStatsExn(&wuStats);
			
		//if (!is_recovery_mode)
//			if (shutdown_event_call || wuStats.curSnapshotTs == wuStats.persSnapshotTs || SnapshotAdvanceCriterion())
		AdvanceSnapshots(); // TODO: check for shutdown on recovery
    	
   		WuOnBeginCheckpointExn();

		RECOVERY_CRASH;
		
		ObtainGiantLock(); isGiantLockObtained = true;
		{	
			flush_data_buffers();
    			d_printf1("flush data buffers completed\n");

			WuEnumerateVersionsParams params;
   			WuEnumerateVersionsForCheckpointExn(&params, llLogCheckpoint);

			RECOVERY_CRASH;

			llFlushAll();
    			d_printf1("flush logical log completed\n");

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
		
	        	d_printf2("Sem %d acquired\n", i);

		    }
    		d_printf1("All sems acquired\n");

			ObtainGiantLock(); isGiantLockObtained = true;
			{
				AdvanceSnapshots();
			}
			ReleaseGiantLock(); isGiantLockObtained = false;

			RECOVERY_CRASH;

	    	for (int i=0; i<CHARISMA_MAX_TRNS_NUMBER; i++)    
    	    	if (USemaphoreUp(concurrent_trns_sem, __sys_call_error) !=0 )
	    	     throw SYSTEM_EXCEPTION("Can't up semaphore concurrent micro ops number semaphore");
	    }
   	}
   	
    d_printf2("checkpoint finished times=%d\n", times);

    times++;

///////////////////
//for DEBUG    
//exit(1);
///////////////////

//   	if (shutdown_checkpoint_thread == true) return 0;
   	if (shutdown_event_call == true) return 0;


  }//end while
 } catch(SednaException &e) {
   if (isGiantLockObtained) ReleaseGiantLock();
   sedna_soft_fault(e, EL_SM);
 } catch (ANY_SE_EXCEPTION) {
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
  char buf3[1024];
  UPID pid;
  UPHANDLE h;

  //string command_line = string(SEDNA_DATA) + string("/bin/rcv_db.exe ") + string(db_name);

  string command_line = uGetImageProcPath(buf, __sys_call_error) +
                        string("/se_rcv ") + string(db_name) + string(" ") +

                        u_i64toa(last_checkpoint_lsn, buf2, 10);
  strcpy(buf, command_line.c_str());

  uSetEnvironmentVariable(SEDNA_OS_PRIMITIVES_ID_MIN_BOUND, u_itoa(((gov_config_struct*)gov_shm_pointer)->gov_vars.os_primitives_id_min_bound, buf3, 10), __sys_call_error);
  
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
       uNonBlockingWaitForChildProcess(pid); /// We need this call since uIsProcessExist returns 1 on zombies!
       res2 = uIsProcessExist(pid, h, __sys_call_error);
       if (res2 != 1) throw USER_EXCEPTION(SE4501);
     }
  }
#endif
}


/*****************************************************************************
      Thread which handles Read and Write operations to logical log file
******************************************************************************/




/*****************************************************************************
                        Transaction identifiers table functions
******************************************************************************/
void init_transaction_ids_table()
{
#ifdef TRMGR_ON
  if (0 != USemaphoreCreate(&trn_table_ids_sync_sem, 1, 1, CHARISMA_SYNC_TRN_IDS_TABLE, NULL, __sys_call_error))
     throw USER_EXCEPTION2(SE4010, "CHARISMA_SYNC_TRN_IDS_TABLE");

  for (int i=0; i< CHARISMA_MAX_TRNS_NUMBER; i++)
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
     for(int i=0; i< _ids_table_.size(); i++)
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

/*****************************************************************************
                       Persistent heap management functions
******************************************************************************/

static TIMESTAMP ts_0 = 0, ts_1 = 0; // timestamps for each type of snapshot (0 means that this type is free)
static TIMESTAMP ts_tmp = 0; // for the case of third snapshot
char buf[20];

static UFile ph_file_0, ph_file_1;
static UMMap		ph_file_mapping_0, ph_file_mapping_1;
static USemaphore	ph_semaphore_0, ph_semaphore_1;

// this function inits ph on snapshot init
int PhOnInitialSnapshotCreate(TIMESTAMP ts)
{
    string ph_file_name = string(db_files_path) + string(db_name) + "." + string(u_ui64toa(ts, buf, 10)) + ".seph";

	if (!ts_0)
	{
	    ph_file_0 = uOpenFile(ph_file_name.c_str(), U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, 
	    	U_NO_BUFFERING, __sys_call_error);
    	if (ph_file_0 == U_INVALID_FD)
        	throw USER_ENV_EXCEPTION("Cannot open persistent heap", false);
    	
	    ph_file_mapping_0 = uCreateFileMapping(ph_file_0, 0, CHARISMA_PH_0_SNP_SHARED_MEMORY_NAME, NULL, __sys_call_error);
    	if (U_INVALID_FILEMAPPING(ph_file_mapping_0))
        	throw USER_ENV_EXCEPTION("Cannot open persistent heap", false);
    	
	    if (USemaphoreCreate(&ph_semaphore_0, 1, 1, PERS_HEAP_0_SNP_SEMAPHORE_STR, NULL, __sys_call_error) != 0)
        	throw USER_ENV_EXCEPTION("Cannot open persistent heap", false);
    	
        ts_0 = ts;

        return 0;
    }
	else if (!ts_1)
	{
	    ph_file_1 = uOpenFile(ph_file_name.c_str(), U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, 
	    	U_NO_BUFFERING, __sys_call_error);
    	if (ph_file_1 == U_INVALID_FD)
        	throw USER_ENV_EXCEPTION("Cannot open persistent heap", false);
    	
	    ph_file_mapping_1 = uCreateFileMapping(ph_file_1, 0, CHARISMA_PH_1_SNP_SHARED_MEMORY_NAME, NULL, __sys_call_error);
    	if (U_INVALID_FILEMAPPING(ph_file_mapping_1))
        	throw USER_ENV_EXCEPTION("Cannot open persistent heap", false);
    	
	    if (USemaphoreCreate(&ph_semaphore_1, 1, 1, PERS_HEAP_1_SNP_SEMAPHORE_STR, NULL, __sys_call_error) != 0)
        	throw USER_ENV_EXCEPTION("Cannot open persistent heap", false);
    	
        ts_1 = ts;

        return 1;
    }
    else
    	throw USER_EXCEPTION(SE4605);
}

// this function inits ph on snapshot init
int PhOnSnapshotCreate(TIMESTAMP ts, TIMESTAMP *damTs, int damTsSize)
{
	int i;
	
    string ph_file_name = string(db_files_path) + string(db_name) + "." + string(u_ui64toa(ts, buf, 10)) + ".seph";
	string ph_cur_file_name = string(db_files_path) + string(db_name) + ".seph";

	if (uCopyFile(ph_cur_file_name.c_str(), ph_file_name.c_str(), false, __sys_call_error) == 0)
      throw USER_EXCEPTION(SE4306);
	
	if (ts_0 && ts_1) // the case of "three" snapshots
	{
		//ts_tmp = (ts_0 < ts_1) ? ts_0 : ts_1;
		// find damaged snapshot
		for (i = 0; i < damTsSize; i++)
			if (damTs[i] == ts_0)
			{
				ts_tmp = ts_0;
				break;
			}
			else if (damTs[i] == ts_1)
			{
				ts_tmp = ts_1;
				break;
			}

		U_ASSERT(ts_tmp == ts_0 || ts_tmp == ts_1);
		PhOnSnapshotDelete(ts_tmp, false);
	}
				
	if (!ts_0)
	{
	    ph_file_0 = uOpenFile(ph_file_name.c_str(), U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, 
	    	U_NO_BUFFERING, __sys_call_error);
    	if (ph_file_0 == U_INVALID_FD)
        	throw USER_ENV_EXCEPTION("Cannot open persistent heap", false);
    	
	    ph_file_mapping_0 = uCreateFileMapping(ph_file_0, 0, CHARISMA_PH_0_SNP_SHARED_MEMORY_NAME, NULL, __sys_call_error);
    	if (U_INVALID_FILEMAPPING(ph_file_mapping_0))
        	throw USER_ENV_EXCEPTION("Cannot open persistent heap", false);
    	
	    if (USemaphoreCreate(&ph_semaphore_0, 1, 1, PERS_HEAP_0_SNP_SEMAPHORE_STR, NULL, __sys_call_error) != 0)
        	throw USER_ENV_EXCEPTION("Cannot open persistent heap", false);
    	
        ts_0 = ts;

        return 0;
    }
	else if (!ts_1)
	{
	    ph_file_1 = uOpenFile(ph_file_name.c_str(), U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, 
	    	U_NO_BUFFERING, __sys_call_error);
    	if (ph_file_1 == U_INVALID_FD)
        	throw USER_ENV_EXCEPTION("Cannot open persistent heap", false);
    	
	    ph_file_mapping_1 = uCreateFileMapping(ph_file_1, 0, CHARISMA_PH_1_SNP_SHARED_MEMORY_NAME, NULL, __sys_call_error);
    	if (U_INVALID_FILEMAPPING(ph_file_mapping_1))
        	throw USER_ENV_EXCEPTION("Cannot open persistent heap", false);
    	
	    if (USemaphoreCreate(&ph_semaphore_1, 1, 1, PERS_HEAP_1_SNP_SEMAPHORE_STR, NULL, __sys_call_error) != 0)
        	throw USER_ENV_EXCEPTION("Cannot open persistent heap", false);
    	
        ts_1 = ts;

        return 1;
    }
    else
    	
    	throw USER_EXCEPTION(SE4605);
}

// this function releases ph on snapshot deletion
void PhOnSnapshotDelete(TIMESTAMP ts, bool isDelete)
{
    string ph_file_name = string(db_files_path) + string(db_name) + "." + string(u_ui64toa(ts, buf, 10)) + ".seph";

    if (ts == ts_0)
    {
    	if (USemaphoreRelease(ph_semaphore_0, __sys_call_error) != 0)
	        throw USER_ENV_EXCEPTION("Cannot close persistent heap", false);

   		if (uReleaseFileMapping(ph_file_mapping_0, NULL, __sys_call_error) != 0)
	        throw USER_ENV_EXCEPTION("Cannot close persistent heap", false);

    	if (uCloseFile(ph_file_0, __sys_call_error) == 0)
	        throw USER_ENV_EXCEPTION("Cannot close persistent heap", false);

	    ts_0 = 0;
    }
    else if (ts == ts_1)
    {
    	if (USemaphoreRelease(ph_semaphore_1, __sys_call_error) != 0)
	        throw USER_ENV_EXCEPTION("Cannot close persistent heap", false);

   		if (uReleaseFileMapping(ph_file_mapping_1, NULL, __sys_call_error) != 0)
	        throw USER_ENV_EXCEPTION("Cannot close persistent heap", false);

    	if (uCloseFile(ph_file_1, __sys_call_error) == 0)
	        throw USER_ENV_EXCEPTION("Cannot close persistent heap", false);

	    ts_1 = 0;
    }
    else if (ts == ts_tmp)
    {
        if (uDeleteFile(ph_file_name.c_str(), __sys_call_error) == 0)
    	   throw USER_EXCEPTION2(SE4041, ph_file_name.c_str());

    	ts_tmp = 0;
    	return;
    }
    else
    	throw USER_EXCEPTION(SE4605);
    
    if (isDelete && ts != llGetPersTimestamp())
	    if (uDeleteFile(ph_file_name.c_str(), __sys_call_error) == 0)
    	   throw USER_EXCEPTION2(SE4041, ph_file_name.c_str());
}

int GetPhIndex(TIMESTAMP ts)
{
	if (ts_0 == ts)
		return 0;
	else if (ts_1 == ts)
		return 1;
	else
    	throw USER_EXCEPTION(SE4605);
}
