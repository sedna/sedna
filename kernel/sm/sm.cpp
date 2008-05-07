/*
 * File:  sm.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <iostream>
#include "sm/sm_globals.h"
#include "sm/sm_functions.h"
#include "sm/bufmgr/bm_functions.h"
#include "sm/bufmgr/bm_core.h"
#include "sm/llmgr/llmgr.h"
#include "common/u/usem.h"
#include "common/u/uevent.h"
#include "common/SSMMsg.h"
#include "common/errdbg/d_printf.h"
#include "sm/trmgr.h"
#include "common/pping.h"
#include "common/version.h"
#include "common/lm_base.h"
#include "sm/lm/lm_globals.h"
#include "common/gmm.h"
#include "common/mmgr/memutils.h"
#include "common/config.h"
#include "common/ipc_ops.h"

#include "sm/wu/wu.h"

#include "common/rcv_test.h"

#include "sm/hb_utils.h"

using namespace std;

SSMMsg *ssmmsg;

USemaphore wait_for_shutdown;


#define SM_BACKGROUND_MODE_TIMEOUT		60000



#ifdef _WIN32
BOOL SMCtrlHandler(DWORD fdwCtrlType) 
{ 
    switch (fdwCtrlType) 
    { 
        case CTRL_C_EVENT		: // Handle the CTRL+C signal. 
        case CTRL_CLOSE_EVENT	: // CTRL+CLOSE: confirm that the user wants to exit. 
        case CTRL_BREAK_EVENT	: 
        case CTRL_LOGOFF_EVENT	: 
        case CTRL_SHUTDOWN_EVENT: 
                                  Beep(1000, 1000); 

                                  send_stop_sm_msg();

                                  return TRUE; 
        default					: return FALSE; 
    } 
} 
#else
#include <signal.h>

void SMCtrlHandler(int signo)
{
	if (   signo == SIGINT 
        || signo == SIGQUIT
        || signo == SIGTERM) 
	{
        //beep(); 
        send_stop_sm_msg();
	}
}
#endif

int sm_server_handler(void *arg)
{
    //d_printf1("query received\n");   

    sm_msg_struct *msg = (sm_msg_struct*)arg;
	bool isGiantLockObtained = false;

    try {
		ObtainGiantLock(); isGiantLockObtained = true;
        switch (msg->cmd)
        {   
            case 1:  {//get identifier for transaction
                         msg->trid = get_transaction_id();
                         break;
                     }
            case 2:  {//give identifier for transaction
                         give_transaction_id(msg->trid);
                         
                         if (msg->data.data[0]) // query has just finished; snapshot advancement might be possible
  						 {	
  						 	if (UEventSet(&end_of_rotr_event, __sys_call_error) != 0)
                         		throw SYSTEM_EXCEPTION("Event signaling for possibility of snapshot advancement failed");
						 }
						 else // updater has just ended; check for need to advance snapshots
						 {
  						 	if (UEventSet(&start_checkpoint_snapshot,  __sys_call_error) != 0)
                         		throw SYSTEM_EXCEPTION("Event signaling for checking of snapshot advancement failed");
                         }
                         	
                         break;
                     }
#ifdef LOCK_MGR_ON
            case 3:  {//obtain lock on database entity
                         lock_mode mode;
                         resource_kind kind;

                         //d_printf1("case 3 (obtain lock)\n");

//                         d_printf1("lock table before lock operation\n");
//                         lm_table.print();
//                         d_printf1("\n");

                         //d_printf1("trans table before lock operation\n");
                         //tr_table.print();
                         //d_printf1("\n");
                         

                         if (msg->data.data[0] == 's') mode = lm_s;
                         else if (msg->data.data[0] == 'x') mode = lm_x;
                         else if (msg->data.data[0] == 'r') mode = lm_is;
                         else mode = lm_ix;
                       
                         if (msg->data.data[1] == 'd') kind = LM_DOCUMENT;
                         else if (msg->data.data[1] == 'c') kind = LM_COLLECTION;
                         else if (msg->data.data[1] == 'i') kind = LM_INDEX;
                         else if (msg->data.data[1] == 't') kind = LM_TRIGGER;
                         else kind = LM_DATABASE;
  
                         lock_reply r = lm_table.lock(msg->trid, msg->sid, resource_id(string((msg->data.data)+2), kind), mode, LOCK_LONG, 0/*timeout is not important by now*/);

                         if (r == LOCK_OK) msg->data.data[0] = '1';
                         else if (r == LOCK_NOT_LOCKED && !lm_table.deadlock(true)) msg->data.data[0] = '0';
                         else
                         {
                             msg->data.data[0] = '2';
                             tr_lock_head* tr_head = tr_table.find_tr_lock_head(msg->trid);
                             if (tr_head == NULL) throw SYSTEM_EXCEPTION("Incorrect logic in SM's lock manager");
                             tr_head->tran->status = ROLLING_BACK_AFTER_DEADLOCK;
                         }
                          
//                         d_printf1("lock table after lock operation\n");
//                         lm_table.print();
//                         d_printf1("\n");

                         //d_printf1("trans table after lock operation\n");
                         //tr_table.print();
                         //d_printf1("\n");

                         //d_printf2("reply=%c\n", msg->data.data[0]);
                         break;
                     }

            case 4:  {//release all transaction's locks
//                         d_printf1("case4 (release all locks)\n");
//                         d_printf1("lock table before release locks operation\n");
//                         lm_table.print();
//                         d_printf1("\n");      

                         //d_printf1("trans table before release lock operation\n");
                         //tr_table.print();
                         //d_printf1("\n");

                         lm_table.release_tr_locks(msg->trid);

//                         d_printf1("lock table after release locks operation\n");
//                         lm_table.print();
//                         d_printf1("\n");

                         //d_printf1("trans table after release lock operation\n");
                         //tr_table.print();
                         //d_printf1("\n");


                         break;
                     }  
            case 5:  {
//                         d_printf2("case 5(release lock on doc=%s)\n", (msg->data.data)+2);
//                         d_printf1("lock table before release locks operation\n");
//                         lm_table.print();
//                         d_printf1("\n");      

                         //d_printf1("trans table before release lock operation\n");
                         //tr_table.print();
                         //d_printf1("\n");

                         resource_kind kind;


                         if (msg->data.data[1] == 'd') kind = LM_DOCUMENT;
                         else if (msg->data.data[1] == 'c') kind = LM_COLLECTION;
                         else if (msg->data.data[1] == 'i') kind = LM_INDEX;
                         else if (msg->data.data[1] == 't') kind = LM_TRIGGER;
                         else kind = LM_DATABASE;
  
                         lock_reply r = lm_table.unlock(msg->trid, resource_id(string((msg->data.data)+2), kind));


//                         d_printf1("lock table after release locks operation\n");
//                         lm_table.print();
//                         d_printf1("\n");

                         //d_printf1("trans table after release lock operation\n");
                         //tr_table.print();
                         //d_printf1("\n");


                         break;

                     }        
#endif

            case 10: {
                         //d_printf1("query 10: soft shutdown\n");
                         USemaphoreUp(wait_for_shutdown, __sys_call_error);
                         msg->cmd = 0;
                         break;
                     }
            case 11: {
                         //d_printf1("query 11: hard shutdown\n");
                         USemaphoreUp(wait_for_shutdown, __sys_call_error);
                         msg->cmd = 0;
                         break;
                     }
            case 21: {
                         //d_printf1("query 21: bm_register_session\n");
                         persistent_db_data* pdb;
                         bm_register_session(msg->sid, &pdb, msg->data.reg.num);
                         msg->data.reg.num = bufs_num;
                         msg->data.reg.mptr = pdb;
                         msg->cmd = 0;
                         break;
                     }
            case 22: {
                         //d_printf1("query 22: bm_unregister_session\n");
                         bm_unregister_session(msg->sid);
                         msg->cmd = 0;
                         break;
                     }
            case 23: {
                         //d_printf1("query 23: bm_allocate_data_block\n");
                         WuAllocateDataBlockExn(msg->sid, 
                                                (xptr*)(&(msg->data.swap_data.ptr)), 
                                                (ramoffs*)(&(msg->data.swap_data.offs)), 
                                                (xptr*)(&(msg->data.swap_data.swapped)));
                         msg->cmd = 0;
                         break;
                     }
            case 24: {
                         //d_printf1("query 24: bm_allocate_tmp_block\n");
                         WuAllocateTempBlockExn(msg->sid, 
                                               (xptr*)(&(msg->data.swap_data.ptr)), 
                                               (ramoffs*)(&(msg->data.swap_data.offs)), 
                                               (xptr*)(&(msg->data.swap_data.swapped)));
                         msg->cmd = 0;
                         break;
                     }
            case 25: {
                         //d_printf1("query 25: bm_delete_block\n");
                         WuDeleteBlockExn(msg->sid, *(xptr*)(&(msg->data.ptr)));
                         msg->cmd = 0;

                         break;
                     }
            case 26: {
                         //d_printf1("query 26: bm_get_block\n");
                         WuGetBlockExn(msg->sid, 
                                      *(xptr*)(&(msg->data.swap_data.ptr)), 
                                      (ramoffs*)(&(msg->data.swap_data.offs)), 
                                      (xptr*)(&(msg->data.swap_data.swapped)));
                         msg->cmd = 0;
                         break;
                     }
            case 27: {
                         //d_printf1("query 27: bm_enter_exclusive_mode\n");
                         bm_enter_exclusive_mode(msg->sid, &(msg->data.reg.num));
                         msg->cmd = 0;
                         break;
                     }
            case 28: {
                         //d_printf1("query 28: bm_exit_exclusive_mode\n");
                         bm_exit_exclusive_mode(msg->sid);
                         msg->cmd = 0;
                         break;
                     }
            case 29: {
                         //d_printf1("query 29: bm_memlock_block\n");
                         bm_memlock_block(msg->sid, *(xptr*)(&(msg->data.ptr)));
                         msg->cmd = 0;
                         break;
                     }
            case 30: {
                         //d_printf1("query 30: bm_memunlock_block\n");
                         bm_memunlock_block(msg->sid, *(xptr*)(&(msg->data.ptr)));
                         msg->cmd = 0;
                         break;
                     }
            case 31: {
                         //d_printf1("query 31: bm_block_statistics\n");
                         bm_block_statistics(&(msg->data.stat));
                         msg->cmd = 0;
                         break;
                     }
            case 32: {
                         //d_printf1("query 32: bm_pseudo_allocate_data_block\n");
                         //bm_pseudo_allocate_data_block(msg->sid, (xptr*)(&(msg->data.ptr)));
                         msg->cmd = 0;
                         break;
                     }
            case 33: {
                         //d_printf1("query 33: bm_pseudo_delete_data_block\n");
                         //bm_pseudo_delete_data_block(msg->sid, *(xptr*)(&(msg->data.ptr)));
                         msg->cmd = 0;
                         break;
                     }
            case 34: {
                         //d_printf1("query 34: bm_delete_tmp_blocks\n");
                         bm_delete_tmp_blocks(msg->sid);
                         msg->cmd = 0;
                         break;
                     }
            case 35: {
                         //d_printf1("query 35: bm_register_transaction\n");
						 bool isUsingSnapshot = msg->data.data[0];

                         bm_register_transaction(msg->sid, msg->trid);
						 try
						 {
							 WuOnRegisterTransactionExn(msg->sid, isUsingSnapshot, (TIMESTAMP*) &msg->data.snp_info.ts, &msg->data.snp_info.type_of_snp);
						 }
						 catch(ANY_SE_EXCEPTION)
						 {
							 bm_unregister_transaction(msg->sid, msg->trid);
							 throw;
						 }

                         msg->cmd = 0;
                         break;
                     }
            case 36: {
                         //d_printf1("query 36: bm_unregister_transaction\n");
						 WuOnUnregisterTransactionExn(msg->sid);
                         bm_unregister_transaction(msg->sid, msg->trid);

						 /* TODO: check if we can advance snapshots and probably advance */ 
/*
						 {
							static int cntr=0;
							++cntr;
							if (cntr % 3 == 0) WuAdvanceSnapshotsExn();
						 }
*/
                         msg->cmd = 0;
                         break;
                     }
			case 37:
                     {
						 /* create version for the block */ 
                         WuCreateBlockVersionExn(msg->sid, 
                                      *(xptr*)(&(msg->data.swap_data.ptr)), 
                                      (ramoffs*)(&(msg->data.swap_data.offs)), 
                                      (xptr*)(&(msg->data.swap_data.swapped)));
                         msg->cmd = 0;
                         break;
                     }
			case 38:
                     {
						 /* rollback or commit notification */ 
						 bool isRollback = msg->data.data[0];
						 if (isRollback)
						 {
							 WuOnRollbackTransactionExn(msg->sid);
						 }
						 else
						 {
							 WuOnCommitTransactionExn(msg->sid);
						 }
						 msg->cmd = 0;
						 break;
                     }
			case 39:
                     {
						 /* 
						  * hot-backup request
						  * important note: sm doesn't check consistency of requests. it presumes correct sequence of calls.
						  * for now such checkings are performed in gov process, so we should be ok with this.
						  */

						 if (msg->data.hb_struct.state == HB_START || msg->data.hb_struct.state == HB_START_CHECKPOINT)
							msg->data.hb_struct.state =	hbProcessStartRequest(msg->data.hb_struct.state);

						 else if (msg->data.hb_struct.state == HB_ARCHIVELOG)
						 	msg->data.hb_struct.state =	hbProcessLogArchRequest(&(msg->data.hb_struct.lnumber));
						 
						 else if (msg->data.hb_struct.state == HB_GETPERSTS)
						 	msg->data.hb_struct.state =	hbProcessGetTsRequest(&(msg->data.hb_struct.ts));

						 else if (msg->data.hb_struct.state == HB_GETPREVLOG)
						 	msg->data.hb_struct.state =	hbProcessGetPrevLogRequest(&(msg->data.hb_struct.lnumber));
						 
						 else if (msg->data.hb_struct.state == HB_END)
						 	msg->data.hb_struct.state =	hbProcessEndRequest();
						 
						 else
						 	msg->data.hb_struct.state = HB_ERR;

						 msg->cmd = 0;
						 break;
                     }

            default: {
                         //d_printf2("query unknown (%d)\n", msg->cmd);
                         msg->cmd = 1;
                         break;
                     }
        }
		ReleaseGiantLock(); isGiantLockObtained = false;
    } catch (SednaUserException &e) {
		if (isGiantLockObtained) ReleaseGiantLock();
        switch (e.get_code())
        {
            case SE1011:  // Data file has reached its maximum size.
                          msg->cmd = 21;
                          break;
            case SE1012:  // Temporary file has reached its maximum size.
                          msg->cmd = 22;
                          break;
            case SE1013:  // Cannot extend data file.
                          msg->cmd = 23;
                          break;
            case SE1014:  // Cannot extend temporary file.
                          msg->cmd = 24;
                          break;
            case SE1018:  // Transaction with this id already exists.
                          msg->cmd = 25;
                          break;
            case SE1019:  // There is no transaction with this id.
                          msg->cmd = 26;
                          break;
            case SE1020:  // Transaction's limit on locked blocks in memory is exceeded.
                          msg->cmd = 27;
                          break;
            case SE1021:  // Cannot lock block in memory because it is not in memory.
                          msg->cmd = 28;
                          break;
            default    :  sedna_soft_fault(e, EL_SM);
        }
    } catch (SednaException &e) {
		if (isGiantLockObtained) ReleaseGiantLock();
        sedna_soft_fault(e, EL_SM);
    } catch (ANY_SE_EXCEPTION) {
		if (isGiantLockObtained) ReleaseGiantLock();
        sedna_soft_fault(EL_SM);
    }

    return 0;
}


void print_sm_usage()
{
  throw USER_SOFT_EXCEPTION((string("Usage: se_sm [options] dbname\n\n") +
                             string("options:\n") + string(arg_glossary(sm_argtable, narg, "  ")) + string("\n")).c_str());
}


int main(int argc, char **argv)
{
    program_name_argv_0 = argv[0];
    pping_client *ppc = NULL;
    bool is_ppc_closed = true;
    char buf[1024];
    UShMem gov_mem_dsc;

    SednaUserException ppc_ex = USER_EXCEPTION(SE4400); // used below in ppc->startup() 

    try {

#ifdef SE_MEMORY_MNG
        SafeMemoryContextInit();
#endif

        if (argc == 1)
        {
            print_sm_usage();
        }
        else
        {
            int arg_scan_ret_val = 0; // 1 - parsed successful, 0 - there was errors
            arg_scan_ret_val = arg_scanargv(argc, argv, sm_argtable, narg, NULL, buf, NULL);

            if (sm_help == 1 ) print_sm_usage();
            if (sm_version == 1) { print_version_and_copyright("Sedna Storage Manager"); throw USER_SOFT_EXCEPTION(""); }

            if (arg_scan_ret_val == 0)
                throw USER_ENV_EXCEPTION(buf, false);
            if (strcmp(db_name, "???") == 0)
               throw USER_ENV_EXCEPTION("unexpected command line parameters: no dbname parameter", false);
        }

        gov_header_struct cfg;
        get_default_sednaconf_values(&cfg);
        get_gov_config_parameters_from_sednaconf(&cfg);//get config parameters from sednaconf

		InitGlobalNames(cfg.os_primitives_id_min_bound, INT_MAX);
		SetGlobalNames();
        gov_shm_pointer = open_gov_shm(&gov_mem_dsc);

        db_id = get_db_id_by_name((gov_config_struct*)gov_shm_pointer, db_name);

        if (db_id == -1)//there is no such database
           throw USER_EXCEPTION2(SE4200, db_name);

        SEDNA_DATA = ((gov_header_struct *) gov_shm_pointer)->SEDNA_DATA;

		SetGlobalNamesDB(db_id);

        //set_global_names(cfg.os_primitives_id_min_bound, db_id);

		/* event_logger_init must be after set_global_names */
        event_logger_init(EL_SM, db_name, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
        elog(EL_LOG, ("SM event log is ready"));
        
        elog(EL_LOG, ("SM set global names done"));

#ifdef REQUIRE_ROOT
        if (!uIsAdmin(__sys_call_error)) throw USER_EXCEPTION(SE3064);
#endif

        if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION(SE3001);

		InitGiantLock(); atexit(DestroyGiantLock);
        
        ppc = new pping_client(cfg.ping_port_number, EL_SM);
        ppc->startup(ppc_ex);
        is_ppc_closed = false;

        elog(EL_LOG, ("SM ping started"));

        open_global_memory_mapping(SE4400);
        get_vmm_region_values();
        close_global_memory_mapping();

        elog(EL_LOG, ("SM vmm region values determined"));

        if (uGetEnvironmentVariable(SM_BACKGROUND_MODE, buf, 1024, __sys_call_error) == 0)
        {
            // we were started by command "se_sm -background-mode off" from "se_sm -background-mode on"
#ifdef _WIN32
#else
            // perform standard routines to run the process in the background mode
            setsid();
            //chdir(SEDNA_DATA);
            //umask(0);
            elog(EL_LOG, ("SM standard routines to run the process in the background mode (setsid) done"));
#endif
        }

        setup_sm_globals((gov_config_struct *)gov_shm_pointer);//setup default values from config file


        recover_database_by_physical_and_logical_log(db_id);


        /////////////// BACKGROUND MODE ////////////////////////////////////////
        char *command_line_str = NULL;
        if (background_mode == 1)
        {
        try {
            string command_line = argv[0];
            command_line += " -background-mode off ";
            command_line += " -bufs-num " + int2string(__bufs_num__);
            command_line += " -max-trs-num " + int2string(__max_trs_num__) + " ";

            char buf_uc[100];
            sprintf(buf_uc, "%.2f", __upd_crt__);

            command_line += string(" -upd-crt ") + buf_uc + " ";
            command_line += db_name;

            command_line_str = new char[command_line.length() + 1];
            strcpy(command_line_str, command_line.c_str());

            if (uSetEnvironmentVariable(SM_BACKGROUND_MODE, "1", __sys_call_error) != 0)
                throw USER_EXCEPTION2(SE4072, "SM_BACKGROUND_MODE");


            USemaphore started_sem;
            if (0 != USemaphoreCreate(&started_sem, 0, 1, CHARISMA_SM_IS_READY, NULL, __sys_call_error))
                throw USER_EXCEPTION(SE4205);
           
            if (uCreateProcess(command_line_str, false, NULL, U_DETACHED_PROCESS, NULL, NULL, NULL, NULL, NULL, __sys_call_error) != 0)
                throw USER_EXCEPTION(SE4205);

            int res;
            res = USemaphoreDownTimeout(started_sem, SM_BACKGROUND_MODE_TIMEOUT, __sys_call_error);

            USemaphoreRelease(started_sem, __sys_call_error);
            delete [] command_line_str;
          

            if (res != 0)
                throw USER_EXCEPTION(SE4205);

            ppc->shutdown();
            delete ppc;
            ppc = NULL;
            is_ppc_closed = true;
            if (uSocketCleanup(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION(SE3000);
           
            fprintf(res_os, "SM has been started in the background mode\n");
            fflush(res_os);
            return 0;

        } catch (SednaUserException &e) {
            fprintf(stderr, "%s\n", e.getMsg().c_str());
            if (!is_ppc_closed) { if (ppc) ppc->shutdown();}
            return 1;
        } catch (SednaException &e) {
            sedna_soft_fault(e, EL_SM);
        } catch (ANY_SE_EXCEPTION) {
            sedna_soft_fault(EL_SM);
        }
    	}
        /////////////// BACKGROUND MODE ////////////////////////////////////////

//        event_logger_init(EL_SM, db_name, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
//        elog(EL_LOG, ("SM event log is ready"));

        if (USemaphoreCreate(&wait_for_shutdown, 0, 1, CHARISMA_SM_WAIT_FOR_SHUTDOWN, NULL, __sys_call_error) != 0)
            throw USER_EXCEPTION(SE4206);
/*
        if ( __bufs_num__ > 0 )
           bufs_num = __bufs_num__;

        if ( __max_trs_num__ > 0)
           max_trs_num = __max_trs_num__;
*/         

        //init transacion ids table
        init_transaction_ids_table();
        elog(EL_LOG, ("SM init_transaction_ids_table done"));

        //init checkpoint resources
        init_checkpoint_sems();
        elog(EL_LOG, ("SM init_checkpoint_sems done"));

        //create checkpoint thread
        start_chekpoint_thread();
        elog(EL_LOG, ("SM start_chekpoint_thread done"));

        //start up logical log
		if (!ll_logical_log_startup(sedna_db_version)) throw SYSTEM_EXCEPTION("Inconsistent database state");
        elog(EL_LOG, ("SM ll_logical_log_startup done"));

        //enable checkpoints
        ll_log_set_checkpoint_flag(true);
        elog(EL_LOG, ("SM ll_logical_log_startup done"));


#ifdef LOCK_MGR_ON
        lm_table.init_lock_table();
        elog(EL_LOG, ("SM init_lock_table done"));
#endif

        //start buffer manager
        bm_startup();
        elog(EL_LOG, ("SM buffer manager started"));


#ifdef _WIN32
        BOOL fSuccess; 
        fSuccess = SetConsoleCtrlHandler((PHANDLER_ROUTINE) SMCtrlHandler, TRUE);                           // add to list 
        if (!fSuccess) throw USER_EXCEPTION(SE4207);
#else
		// For Control-C or Delete
        if ((int)signal(SIGINT, SMCtrlHandler) == -1) throw USER_EXCEPTION(SE4207);
		// For Control-backslash
        if ((int)signal(SIGQUIT, SMCtrlHandler) == -1) throw USER_EXCEPTION(SE4207);
		//For reboot or halt
        if ((int)signal(SIGTERM, SMCtrlHandler) == -1) throw USER_EXCEPTION(SE4207);
#endif
        try {
            // Starting SSMMsg server
            d_printf1("Starting SSMMsg...");

			//((gov_config_struct*)gov_shm_pointer)->gov_vars.os_primitives_id_min_bound
            ssmmsg = new SSMMsg(SSMMsg::Server, 
                                sizeof (sm_msg_struct), 
                                CHARISMA_SSMMSG_SM_ID(db_id, buf, 1024),
                                SM_NUMBER_OF_SERVER_THREADS,
                                U_INFINITE);
            if (ssmmsg->init() != 0)
                throw USER_EXCEPTION(SE3030);

            if (ssmmsg->serve_clients(sm_server_handler) != 0)
                throw USER_EXCEPTION(SE3031);

            WuSetTimestamp(ll_returnTimestampOfPersSnapshot() + 1);
            WuInitExn(0,0, ll_returnTimestampOfPersSnapshot());
            elog(EL_LOG, ("SM : Wu is initialized"));

#ifdef RCV_TEST_CRASH
            read_test_cfg();
#endif
            d_printf1("OK\n");

                            sm_blk_stat stat;
                            bm_block_statistics(&stat);
                            d_printf1("Block statistics:\n");
                            d_printf2("free_data_blocks_num = %d\n", stat.free_data_blocks_num);
                            d_printf2("free_tmp_blocks_num  = %d\n", stat.free_tmp_blocks_num);
                            d_printf2("used_data_blocks_num = %d\n", stat.used_data_blocks_num);
                            d_printf2("used_tmp_blocks_num  = %d\n", stat.used_tmp_blocks_num);




            ///////// NOTIFY THAT SERVER IS READY //////////////////////////////////
            USemaphore started_sem;
            if (0 == USemaphoreOpen(&started_sem, CHARISMA_SM_IS_READY, __sys_call_error))
            {
                USemaphoreUp(started_sem, __sys_call_error);
                USemaphoreClose(started_sem, __sys_call_error);
            }
            ///////// NOTIFY THAT SERVER IS READY //////////////////////////////////
            register_sm_on_gov();

      
            elog(EL_LOG, ("SM is ready"));
            fprintf(res_os, "\nSM has been started\n");
            fflush(res_os);

			USemaphoreDown(wait_for_shutdown, __sys_call_error);

            //to this point all sessions are closed by governor

            if (ssmmsg->stop_serve_clients() != 0)
                throw USER_EXCEPTION(SE3032);

            if (ssmmsg->shutdown() != 0)
                throw USER_EXCEPTION(SE3033);

            USemaphoreRelease(wait_for_shutdown, __sys_call_error);

        } catch(ANY_SE_EXCEPTION) {
            ssmmsg->stop_serve_clients();
            ssmmsg->shutdown();
            throw;
        }

        delete ssmmsg;

//		WuAdvanceSnapshotsExn();

        //shutdown checkpoint thread (it also makes checkpoint)
        shutdown_chekpoint_thread();

        WuReleaseExn();
        elog(EL_LOG, ("SM : Wu is released"));

        // shutdown bm
        bm_shutdown();

        //shutdown logical log
        ll_logical_log_shutdown();

        //release checkpoint resources
        release_checkpoint_sems();

        release_transaction_ids_table();

#ifdef LOCK_MGR_ON
        lm_table.release_lock_table();
#endif

        event_logger_release();
   
        ppc->shutdown();
        delete ppc;
        is_ppc_closed = true;
        ppc = NULL;

        close_gov_shm(gov_mem_dsc, gov_shm_pointer);

        return 0;
 
    } catch (SednaUserException &e) {
        fprintf(stderr, "%s\n", e.getMsg().c_str());
        event_logger_release();
        if (!is_ppc_closed) { if (ppc) ppc->shutdown();}
        close_gov_shm(gov_mem_dsc, gov_shm_pointer);
        return 1;
    } catch (SednaException &e) {
        sedna_soft_fault(e, EL_SM);
    } catch (ANY_SE_EXCEPTION) {
        sedna_soft_fault(EL_SM);
    }

    return 0;
}


void recover_database_by_physical_and_logical_log(int db_id)
{
  try{
    char buf[1024];
    if (uGetEnvironmentVariable(SM_BACKGROUND_MODE, buf, 1024, __sys_call_error) != 0)
    {//I am in running sm process

       is_recovery_mode = true;

       event_logger_init(EL_SM, db_name, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
       elog(EL_LOG, ("SM event log in recovery procedure is ready"));

       //init transacion ids table
       init_transaction_ids_table();
       elog(EL_LOG, ("SM : init_transaction_ids_table done"));

       //init checkpoint resources
       init_checkpoint_sems();
       elog(EL_LOG, ("SM : init_checkpoint_sems done"));

       bool is_stopped_correctly = ll_logical_log_startup(sedna_db_version/*out parameter*/);
       elog(EL_LOG, ("SM : ll_logical_log_startup done"));
       if (sedna_db_version != SEDNA_DATA_STRUCTURES_VER)
       {
          release_checkpoint_sems();
          release_transaction_ids_table();
          if (sedna_db_version != SEDNA_DATA_STRUCTURES_VER)
             throw USER_EXCEPTION2(SE4212, "See file FAQ shipped with the distribution");
       }

       d_printf1("logical log startup call finished successfully\n");

       // recover persistent heap
       if (!is_stopped_correctly) ll_recover_pers_heap();
       elog(EL_LOG, ("SM : ll_recover_pers_heap done"));

       //create checkpoint thread
       start_chekpoint_thread();
       elog(EL_LOG, ("SM : start_chekpoint_thread done"));

       //start buffer manager
       bm_startup();
       elog(EL_LOG, ("SM : bm_startup done"));

       //recover data base by physical log
       LONG_LSN last_checkpoint_lsn = NULL_LSN;
       if (!is_stopped_correctly) last_checkpoint_lsn = ll_recover_db_by_phys_records();

       d_printf1("db recovered by phys logs successfully\n");
       elog(EL_LOG, ("SM : db recovered by phys log successfully"));

       //disable checkpoints
       ll_log_set_checkpoint_flag(false);
       elog(EL_LOG, ("SM : ll_log_set_checkpoint_flag done"));
        
#ifdef LOCK_MGR_ON
       lm_table.init_lock_table();
       elog(EL_LOG, ("SM : lm_table.init_lock_table done"));
#endif


       // Starting SSMMsg server
       d_printf1("Starting SSMMsg...");

	   //((gov_config_struct*)gov_shm_pointer)->gov_vars.os_primitives_id_min_bound
       ssmmsg = new SSMMsg(SSMMsg::Server, 
                           sizeof (sm_msg_struct), 
                           CHARISMA_SSMMSG_SM_ID(db_id, buf, 1024),
                           SM_NUMBER_OF_SERVER_THREADS,
                           U_INFINITE);
       if (ssmmsg->init() != 0)
          throw USER_EXCEPTION(SE3030);

       if (ssmmsg->serve_clients(sm_server_handler) != 0)
          throw USER_EXCEPTION(SE3031);

       d_printf1("OK\n");

       WuSetTimestamp(ll_returnTimestampOfPersSnapshot() + 1);
//       WuInitExn(1,0,ll_returnTimestampOfPersSnapshot());
       WuInitExn(0,0,ll_returnTimestampOfPersSnapshot()); // turn on versioning mechanism on recovery
       elog(EL_LOG, ("SM : Wu is initialized"));

       //recover database by logical log
       if (!is_stopped_correctly) execute_recovery_by_logical_log_process(last_checkpoint_lsn);
       elog(EL_LOG, ("SM : db recovered by logical log successfully"));

       //enable checkpoints
	   ll_log_set_checkpoint_flag(true);
       elog(EL_LOG, ("SM : checkpoints enabled"));

       if (ssmmsg->stop_serve_clients() != 0)
          throw USER_EXCEPTION(SE3032);

       if (ssmmsg->shutdown() != 0)
          throw USER_EXCEPTION(SE3033);

       //shutdown checkpoint thread (it also makes checkpoint)
       shutdown_chekpoint_thread();
       elog(EL_LOG, ("SM : shutdown checkpoint thread done"));

       WuReleaseExn();
       elog(EL_LOG, ("SM : Wu is released"));

	   // shutdown bm
       bm_shutdown();
       elog(EL_LOG, ("SM : bm_shutdown done"));

       //shutdown logical log
       ll_logical_log_shutdown();
       elog(EL_LOG, ("SM : ll_logical_log_shutdown done"));

       //release checkpoint resources
       release_checkpoint_sems();
       elog(EL_LOG, ("SM : release_checkpoint_sems done"));

       release_transaction_ids_table();
       elog(EL_LOG, ("SM : release_transaction_ids_table done"));

#ifdef LOCK_MGR_ON
       lm_table.release_lock_table();
       elog(EL_LOG, ("SM : lm_table.release_lock_table done"));
#endif
       elog(EL_LOG, ("SM recovery procedure is finished successfully"));
       event_logger_release();
    
       is_recovery_mode = false;
    }
  } catch (SednaUserException &e) {
       fprintf(stderr, "%s\n", e.getMsg().c_str());
       throw USER_EXCEPTION(SE4205);       
  } catch (SednaException &e) {
        throw;
  } catch (ANY_SE_EXCEPTION) {
        throw;
  }

}
