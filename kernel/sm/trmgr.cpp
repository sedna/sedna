/*
 * File:  trmgr.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>
#include <vector>
#include <iostream>
#include "usem.h"
#include "uthread.h"
#include "uprocess.h"
#include "exceptions.h"
#include "bm_core.h"
#include "plmgr.h"
#include "llmgr.h"
#include "uprocess.h"
#include "base.h"
#include "trmgr.h"
#include "sm_globals.h"
#include "d_printf.h"
#include "tr_debug.h"
#include "plmgr_core.h"

using namespace std;

/* global variables for checkpoint */
USemaphore wait_for_checkpoint; 
USemaphore checkpoint_sem; 
USemaphore concurrent_ops_sem;

USemaphore wait_for_recovery;

UTHANDLE  checkpoint_thread_dsc;
bool shutdown_checkpoint_thread;

/* global variables for transaction ids table */
USemaphore trn_table_ids_sync_sem;
USemaphore trns_num_sem;
vector<transaction_id> _ids_table_;
//int trn_ids[CHARISMA_MAX_TRNS_NUMBER];//values: 0->free; 1->busy

/*****************************************************************************
                  !!! Checkpoint functions
******************************************************************************/


U_THREAD_PROC (checkpoint_thread, arg)
{
#ifdef CHECKPOINT_ON
 try{


  int times=1;  
  LONG_LSN cp_lsn;

  while (true)
  {
    if (USemaphoreDown(wait_for_checkpoint) !=0 )
       throw SYSTEM_EXCEPTION("Can't down semaphore for checkpoint wait");

    string str;
    if (shutdown_checkpoint_thread)
        str = string("checkpoint started (shutdown_flag=true)"); 
    else
        str = string("checkpoint started (shutdown_flag=false)"); 
    
    WRITE_DEBUG_LOG(str.c_str());

    //syn with all micro ops
    if (USemaphoreDown(checkpoint_sem) != 0)
       throw SYSTEM_EXCEPTION("Can't down semaphore for beginning checkpoint");

    //!!!init global variable isCheckpoint with interlocked func to true!!!

    d_printf2("checkpoint started num=%d\n", times);



    int i=0;
    for (i=0; i<CHARISMA_MAX_TRNS_NUMBER; i++)    
    {
        if (USemaphoreDown(concurrent_ops_sem) !=0 )
         throw SYSTEM_EXCEPTION("Can't down semaphore concurrent micro ops number semaphore");

        d_printf2("Sem %d acquired\n", i);

    }
    d_printf1("All sems acquired\n");

    flush_master_block();
    d_printf1("flush master block completed\n");

    //d_printf1("all semaphores for concurrent ops_sem acquired\n");
    //flush phys log
    ll_phys_log_flush();

    //d_printf1("ll_phys_log_flush completed\n");

    flush_data_buffers();
    d_printf1("flush buffers completed\n");


    flush_ph();
    d_printf1("flush_ph completed\n");

    //write chekpoint record in logical log
    cp_lsn = ll_logical_log_checkpoint();
    d_printf1("checkpoint record written\n");

    d_printf2("last checkpoint lsn=%d\n", cp_lsn);

    //flush logical log
    ll_logical_log_flush();
    d_printf1("flush logical log completed\n");

    ll_logical_log_flush_last_record();
    d_printf1("checkpoint record has been flushed\n");

    //clear physical log
    ll_phys_log_clear(cp_lsn);
    d_printf1("ll_phys_log_clear completed\n");

  
    backup_ph();

    ll_phys_log_set_ph_bu_to_ph(true);
    d_printf1("ll_phys_log_set_ph_bu_to_ph completed\n");

    ll_phys_log_set_empty_bulk_load_blks();

    d_printf2("checkpoint finished times=%d\n", times);
    for (i=0; i<CHARISMA_MAX_TRNS_NUMBER; i++)    
        if (USemaphoreUp(concurrent_ops_sem) !=0 )
         throw SYSTEM_EXCEPTION("Can't up semaphore concurrent micro ops number semaphore");


    if (USemaphoreUp(checkpoint_sem) != 0)
       throw SYSTEM_EXCEPTION("Can't up semaphore for beginning checkpoint");

    ll_phys_log_set_checkpoint_on_flag(false);
    str = "checkpoint finished\n"; 
    WRITE_DEBUG_LOG(str.c_str());

    times++;

///////////////////
//for DEBUG    
//exit(1);
///////////////////

    if (shutdown_checkpoint_thread == true) return 0;


  }//end while
 } catch(SednaException &e) {
   sedna_soft_fault(e);
 } catch (...) {
   sedna_soft_fault();
 }
#endif

 return 0;
}


void start_chekpoint_thread()
{
#ifdef CHECKPOINT_ON
  if (0 != uCreateThread(checkpoint_thread, NULL, &checkpoint_thread_dsc, CHEKPOINT_THREAD_STACK_SIZE, NULL))
     throw USER_EXCEPTION2(SE4060, "checkpoint thread");

  string str = string("start_checkpoint_thread finished\n");
  WRITE_DEBUG_LOG(str.c_str());

#endif
}

void init_checkpoint_sems()
{
#ifdef CHECKPOINT_ON
  if (USemaphoreCreate(&wait_for_checkpoint, 0, 1, CHARISMA_WAIT_FOR_CHECKPOINT, NULL) != 0)
     throw USER_EXCEPTION2(SE4010, "CHARISMA_WAIT_FOR_CHECKPOINT");

  if (USemaphoreCreate(&checkpoint_sem, 1, 1, CHARISMA_CHECKPOINT_SEM, NULL) != 0)
     throw USER_EXCEPTION2(SE4010, "CHARISMA_CHECKPOINT_SEM");

  if (USemaphoreCreate(&concurrent_ops_sem, CHARISMA_MAX_TRNS_NUMBER, CHARISMA_MAX_TRNS_NUMBER, CHARISMA_LOGICAL_OPERATION_ATOMICITY, NULL) != 0)
     throw USER_EXCEPTION2(SE4010, "CHARISMA_LOGICAL_OPERATION_ATOMICITY");
#endif
}

void shutdown_chekpoint_thread()
{
#ifdef CHECKPOINT_ON
  //shutdown thread
  shutdown_checkpoint_thread = true;
  if (USemaphoreUp(wait_for_checkpoint) !=0)
     throw SYSTEM_EXCEPTION("Can't Up WAIT_FOR_CHECKPOINT semaphore");

  if (uThreadJoin(checkpoint_thread_dsc) != 0)
     throw USER_EXCEPTION(SE4210);

     
  if (uCloseThreadHandle(checkpoint_thread_dsc) != 0)
     throw USER_EXCEPTION2(SE4063, "checkpoint thread");

  //string str = string("shutdown_checkpoint_thread finished\n");
  //WRITE_DEBUG_LOG(str.c_str());

#endif
}

void release_checkpoint_sems()
{
#ifdef CHECKPOINT_ON
  //release semaphores
  if (USemaphoreRelease(wait_for_checkpoint) != 0)
     throw USER_EXCEPTION2(SE4011, "CHARISMA_WAIT_FOR_CHECKPOINT");

  if (USemaphoreRelease(checkpoint_sem) != 0)
     throw USER_EXCEPTION2(SE4011, "CHARISMA_CHECKPOINT_SEM");

  if (USemaphoreRelease(concurrent_ops_sem) != 0)
     throw USER_EXCEPTION2(SE4011, "CHEKPOINT_THREAD_STACK_SIZE");


#endif
}

void execute_recovery_by_logical_log_process()
{
#ifdef RECOVERY_ON
  if (USemaphoreCreate(&wait_for_recovery, 0, 1, CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG, NULL) != 0)
     throw USER_EXCEPTION2(SE4010, "CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG");

  //create recovery process
  int res;

  //string command_line = string(SEDNA_DATA) + string("/bin/rcv_db.exe ") + string(db_name);

  string command_line = uGetImageProcPath() + string("/se_rcv_db.exe ") + string(db_name) ;
  char *command_line_str = new char[command_line.size() + 1];
  strcpy(command_line_str, command_line.c_str());

#ifndef TEST_RECOVERY_ON
  res = uCreateProcess(
                  command_line_str,
                  false,
                  NULL,
                  /*U_NO_WINDOW*/0,
                  NULL,
                  NULL,
                  NULL,
                  NULL,
                  NULL);

  if (res != 0)
     throw USER_EXCEPTION2(SE4070,"recovery process");
#endif

  delete [] command_line_str;
  

  if (USemaphoreDown(wait_for_recovery) != 0)
     throw USER_EXCEPTION2(SE4015, "CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG");

#endif
}

/*****************************************************************************
                        Transaction identifiers table functions
******************************************************************************/
void init_transaction_ids_table()
{
#ifdef TRMGR_ON
  if (0 != USemaphoreCreate(&trn_table_ids_sync_sem, 1, 1, CHARISMA_SYNC_TRN_IDS_TABLE, NULL))
     throw USER_EXCEPTION2(SE4010, "CHARISMA_SYNC_TRN_IDS_TABLE");

  for (int i=0; i< CHARISMA_MAX_TRNS_NUMBER; i++)
      _ids_table_.push_back(i);

#endif
}

void release_transaction_ids_table()
{
#ifdef TRMGR_ON
  if (0 != USemaphoreRelease(trn_table_ids_sync_sem))
     throw USER_EXCEPTION2(SE4011, "CHARISMA_SYNC_TRN_IDS_TABLE");
#endif
}

transaction_id get_transaction_id()
{
#ifdef TRMGR_ON


  if (USemaphoreDown(trn_table_ids_sync_sem) != 0)
     throw SYSTEM_EXCEPTION("Can't down semaphore: CHARISMA_SYNC_TRN_IDS_TABLE");

  
  transaction_id id;
  if (_ids_table_.empty())
     id = -1;
  else
  {
     id = _ids_table_.back();
     _ids_table_.pop_back();
  }


  if (USemaphoreUp(trn_table_ids_sync_sem) != 0)
     throw SYSTEM_EXCEPTION("Can't up semaphore: CHARISMA_SYNC_TRN_IDS_TABLE");

  d_printf2("get trid=%d\n", id);

  return id;
#endif
}

void give_transaction_id(transaction_id& trid)
{
#ifdef TRMGR_ON
  d_printf2("return trid=%d\n", trid);
  

  if (USemaphoreDown(trn_table_ids_sync_sem) != 0)
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


  if (USemaphoreUp(trn_table_ids_sync_sem) != 0)
     throw SYSTEM_EXCEPTION("Can't up semaphore: CHARISMA_SYNC_TRN_IDS_TABLE");
#endif
}
