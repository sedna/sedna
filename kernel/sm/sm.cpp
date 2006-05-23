/*
 * File:  sm.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include <iostream>
#include "sm_globals.h"
#include "sm_functions.h"
#include "bm_functions.h"
#include "bm_core.h"
#include "plmgr.h"
#include "llmgr.h"
#include "usem.h"
#include "SSMMsg.h"
#include "d_printf.h"
#include "trmgr.h"
#include "pping.h"
#include "version.h"
#include "lm_base.h"
#include "lm_globals.h"
#include "gmm.h"
#include "memutils.h"

using namespace std;

SSMMsg *ssmmsg;

USemaphore wait_for_shutdown;


#define SM_BACKGROUND_MODE_TIMEOUT		15000
#define SM_BACKGROUND_MODE				"SEDNA_SM_BACKGROUND_MODE"


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

    try {
        switch (msg->cmd)
        {   
            case 1:  {//get identifier for transaction
                         msg->trid = get_transaction_id();
                         break;
                     }
            case 2:  {//give identifier for transaction
                         give_transaction_id(msg->trid);
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
                         else kind = LM_DATABASE;
  
                         lock_reply r = lm_table.lock(msg->trid, msg->sid, resource_id(string((msg->data.data)+2), kind), mode, LOCK_LONG, 0/*timeout is not important by now*/);

                         if (r == LOCK_OK) msg->data.data[0] = '1';
                         else if (r == LOCK_NOT_LOCKED && !lm_table.deadlock(true)) msg->data.data[0] = '0';
                         else msg->data.data[0] = '2';
                          
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
                         bm_register_session(msg->sid, &pdb);
                         msg->data.num = bufs_num;
                         msg->data.mptr = pdb;
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
                         bm_allocate_data_block(msg->sid, 
                                                (xptr*)(&(msg->data.swap_data.ptr)), 
                                                (ramoffs*)(&(msg->data.swap_data.offs)), 
                                                (xptr*)(&(msg->data.swap_data.swapped)));
                         msg->cmd = 0;
                         break;
                     }
            case 24: {
                         //d_printf1("query 24: bm_allocate_tmp_block\n");
                         bm_allocate_tmp_block(msg->sid, 
                                               (xptr*)(&(msg->data.swap_data.ptr)), 
                                               (ramoffs*)(&(msg->data.swap_data.offs)), 
                                               (xptr*)(&(msg->data.swap_data.swapped)));
                         msg->cmd = 0;
                         break;
                     }
            case 25: {
                         //d_printf1("query 25: bm_delete_block\n");
                         bm_delete_block(msg->sid, *(xptr*)(&(msg->data.ptr)));
                         msg->cmd = 0;
                         break;
                     }
            case 26: {
                         //d_printf1("query 26: bm_get_block\n");
                         bm_get_block(msg->sid, 
                                      *(xptr*)(&(msg->data.swap_data.ptr)), 
                                      (ramoffs*)(&(msg->data.swap_data.offs)), 
                                      (xptr*)(&(msg->data.swap_data.swapped)));
                         msg->cmd = 0;
                         break;
                     }
            case 27: {
                         //d_printf1("query 27: bm_enter_exclusive_mode\n");
                         bm_enter_exclusive_mode(msg->sid, &(msg->data.num));
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
                         bm_pseudo_allocate_data_block(msg->sid, (xptr*)(&(msg->data.ptr)));
                         msg->cmd = 0;
                         break;
                     }
            case 33: {
                         //d_printf1("query 33: bm_pseudo_delete_data_block\n");
                         bm_pseudo_delete_data_block(msg->sid, *(xptr*)(&(msg->data.ptr)));
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
                         bm_register_transaction(msg->sid, msg->trid);
                         msg->cmd = 0;
                         break;
                     }
            case 36: {
                         //d_printf1("query 36: bm_unregister_transaction\n");
                         bm_unregister_transaction(msg->sid, msg->trid);
                         msg->cmd = 0;
                         break;
                     }
            default: {
                         //d_printf2("query unknown (%d)\n", msg->cmd);
                         msg->cmd = 1;
                         break;
                     }
        }

    } catch (SednaUserException &e) {
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
            default    :  sedna_soft_fault(e);
        }
    } catch (SednaException &e) {
        sedna_soft_fault(e);
    } catch (...) {
        sedna_soft_fault();
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
    pping_client ppc(5151);
    bool is_ppc_closed = true;
    char buf[1024];

    try {

        SafeMemoryContextInit();

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


        set_global_names();
        set_global_names(db_name, true);

#ifdef REQUIRE_ROOT
        if (!uIsAdmin(__sys_call_error)) throw USER_EXCEPTION(SE3064);
#endif

        if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION(SE3001);
        
        SednaUserException e = USER_EXCEPTION(SE4400);
        ppc.startup(e);
        is_ppc_closed = false;


        open_global_memory_mapping(SE4400);
        get_vmm_region_values();
        close_global_memory_mapping();


        if (uGetEnvironmentVariable(SM_BACKGROUND_MODE, buf, 1024, __sys_call_error) == 0)
        {
            // we were started by command "se_sm -background-mode off" from "se_sm -background-mode on"
#ifdef _WIN32
#else
            // perform standard routines to run the process in the background mode
            setsid();
            chdir(SEDNA_DATA);
            //umask(0);
#endif
        }


        /////////////// BACKGROUND MODE ////////////////////////////////////////
        char *command_line_str = NULL;
        if (background_mode == 1)
        try {
            string command_line = argv[0];
            command_line += " -background-mode off ";
            command_line += db_name;

            command_line_str = new char[command_line.length() + 1];
            strcpy(command_line_str, command_line.c_str());

            if (uSetEnvironmentVariable(SM_BACKGROUND_MODE, "1", __sys_call_error) != 0)
                throw USER_EXCEPTION2(SE4072, "SM_BACKGROUND_MODE");


            USemaphore started_sem;
            if (0 != USemaphoreCreate(&started_sem, 0, 1, CHARISMA_SM_IS_READY(db_name, buf, 1024), NULL, __sys_call_error))
                throw USER_EXCEPTION(SE4205);
           
            if (uCreateProcess(command_line_str, false, NULL, U_DETACHED_PROCESS, NULL, NULL, NULL, NULL, NULL, __sys_call_error) != 0)
                throw USER_EXCEPTION(SE4205);

            int res;
            res = USemaphoreDownTimeout(started_sem, SM_BACKGROUND_MODE_TIMEOUT, __sys_call_error);

            USemaphoreRelease(started_sem, __sys_call_error);
            delete [] command_line_str;
          

            if (res != 0)
                throw USER_EXCEPTION(SE4205);

            ppc.shutdown();
            if (uSocketCleanup(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION(SE3000);
           
            fprintf(res_os, "SM has been started in the background mode\n"); 
            fflush(res_os);
            return 0;

        } catch (SednaUserException &e) {
            fprintf(stderr, "%s\n", e.getMsg().c_str());
            ppc.shutdown();
            return 1;
        } catch (SednaException &e) {
            sedna_soft_fault(e);
        } catch (...) {
            sedna_soft_fault();
        }
        /////////////// BACKGROUND MODE ////////////////////////////////////////

        event_logger_init(EL_SM, db_name, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
        elog(EL_LOG, ("SM event log is ready"));


        setup_sm_globals();//setup default values from config file


        if (USemaphoreCreate(&wait_for_shutdown, 0, 1, CHARISMA_SM_WAIT_FOR_SHUTDOWN, NULL, __sys_call_error) != 0)
            throw USER_EXCEPTION(SE4206);

         

        if ( __bufs_num__ > 0 )
           bufs_num = __bufs_num__;

        if ( __max_trs_num__ > 0)
           max_trs_num = __max_trs_num__;

        //init transacion ids table
        init_transaction_ids_table();

        //init checkpoint resources
        init_checkpoint_sems();

        //create checkpoint thread
        start_chekpoint_thread();

        //start phys log
        
        bool is_stopped_correctly = ll_phys_log_startup();
        d_printf1("phys log startup call finished successfully\n");

        //disable write to phys log
        ll_phys_log_set_phys_log_flag(false);
        //recover data base by phisical log

        if (!is_stopped_correctly) ll_phys_log_recover_db();
        d_printf1("db recovered by phys log successfully\n");

        ll_phys_log_set_phys_log_flag(true);

        //start up shared memory for phys log
        ll_phys_log_startup_shared_mem();

        //disable checkpoints
        ll_phys_log_set_checkpoint_flag(false);
        
        //start up logical log
        ll_logical_log_startup();

#ifdef LOCK_MGR_ON
        lm_table.init_lock_table();
#endif

        //start buffer manager
        bm_startup();


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

        d_printf2("ext_portion=%d\n", phys_log_ext_portion);
       
        try {
            // Starting SSMMsg server
            d_printf1("Starting SSMMsg...");

            ssmmsg = new SSMMsg(SSMMsg::Server, 
                                sizeof (sm_msg_struct), 
                                CHARISMA_SSMMSG_SM_ID(db_name, buf, 1024), 
                                SM_NUMBER_OF_SERVER_THREADS,
                                U_INFINITE);
            if (ssmmsg->init() != 0)
                throw USER_EXCEPTION(SE3030);

            if (ssmmsg->serve_clients(sm_server_handler) != 0)
                throw USER_EXCEPTION(SE3031);

            d_printf1("OK\n");

                            sm_blk_stat stat;
                            bm_block_statistics(&stat);
                            d_printf1("Block statistics:\n");
                            d_printf2("free_data_blocks_num = %d\n", stat.free_data_blocks_num);
                            d_printf2("free_tmp_blocks_num  = %d\n", stat.free_tmp_blocks_num);
                            d_printf2("used_data_blocks_num = %d\n", stat.used_data_blocks_num);
                            d_printf2("used_tmp_blocks_num  = %d\n", stat.used_tmp_blocks_num);


            //recover database by logical log


//FOR DEBUG I comment this call
            if (!is_stopped_correctly) execute_recovery_by_logical_log_process();



            //enable checkpoints
            ll_phys_log_set_checkpoint_flag(true);


            ///////// NOTIFY THAT SERVER IS READY //////////////////////////////////
            USemaphore started_sem;
            if (0 == USemaphoreOpen(&started_sem, CHARISMA_SM_IS_READY(db_name, buf, 1024), __sys_call_error))
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

        } catch(...) {
            ssmmsg->stop_serve_clients();
            ssmmsg->shutdown();
            throw;
        }

        delete ssmmsg;


        //shutdown checkpoint thread (it also makes checkpoint)
        shutdown_chekpoint_thread();

        // shutdown bm
        bm_shutdown();


        //shutdown phys log
        ll_phys_log_shutdown();

        //shutdown logical log
        ll_logical_log_shutdown();

        //release checkpoint resources
        release_checkpoint_sems();

        release_transaction_ids_table();

#ifdef LOCK_MGR_ON
        lm_table.release_lock_table();
#endif

        event_logger_release();
   
        ppc.shutdown();
        is_ppc_closed = true;

        return 0;
 
    } catch (SednaUserException &e) {
        fprintf(stderr, "%s\n", e.getMsg().c_str());
        event_logger_release();
        if (!is_ppc_closed) ppc.shutdown();
        return 1;
    } catch (SednaException &e) {
        sedna_soft_fault(e);
    } catch(...) {
        sedna_soft_fault();
    }

    return 0;
}
