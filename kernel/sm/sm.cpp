/*
* File:  sm.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include <iostream>

#include "common/sedna.h"
#include "common/u/usem.h"
#include "common/u/uevent.h"
#include "common/SSMMsg.h"
#include "common/errdbg/d_printf.h"
#include "common/rcv_test.h"
// #include "common/pping.h"
#include "common/lm_base.h"
#include "common/gmm.h"
#include "common/mmgr/memutils.h"
#include "common/config.h"
#include "common/ipc_ops.h"
#include "common/sm_vmm_data.h"
#include "common/llcommon/llMain.h"

#include "sm/sm_globals.h"
#include "sm/sm_functions.h"
#include "sm/bufmgr/bm_functions.h"
#include "sm/bufmgr/bm_core.h"
#include "sm/trmgr.h"
#include "sm/lm/lm_globals.h"
#include "sm/wu/wu.h"
#include "sm/llsm/physrcv.h"
#include "sm/hb_utils.h"

#include "common/u/usocket.h"
#include "common/structures/cdb_structures.h"
#include "cdb_utils.h"

using namespace std;
using namespace sm_globals;

static SSMMsg *ssmmsg;
static USemaphore wait_for_shutdown;

#define SM_BACKGROUND_MODE				"SEDNA_SM_BACKGROUND_MODE"
#define SM_BACKGROUND_MODE_TIMEOUT		60000

#ifdef _WIN32
BOOL SMCtrlHandler(DWORD fdwCtrlType)
{
    switch (fdwCtrlType)
    {
    case CTRL_C_EVENT		: /* Handle the CTRL+C signal. */
    case CTRL_CLOSE_EVENT	: /* CTRL+CLOSE: confirm that the user wants to exit. */
    case CTRL_BREAK_EVENT	:
    case CTRL_LOGOFF_EVENT	:
    case CTRL_SHUTDOWN_EVENT:
        Beep(1000, 1000);

        send_stop_sm_msg();

        return TRUE;
    default					: return FALSE;
    }
}
#else  /* !_WIN32 */
#include <signal.h>

void SMCtrlHandler(int signo)
{
    if (   signo == SIGINT
        || signo == SIGQUIT
        || signo == SIGTERM) {
            send_stop_sm_msg();
    }
}
#endif /* _WIN32 */


int sm_server_handler(void *arg)
{
    sm_msg_struct *msg = (sm_msg_struct*)arg;
    bool isGiantLockObtained = false;

    try {
        ObtainGiantLock(); isGiantLockObtained = true;
        switch (msg->cmd)
        {
            case 1:  {//get identifier for transaction
                         bool isRO = (msg->data.data[0]) != 0;
                         bool isExcl = (msg->data.data[1]) != 0;

                         // even in an exclusive mode we don't block ro-transactions
                         if (xmGetExclusiveModeId() != -1 && !isRO)
                         {
                             xmBlockSession(msg->sid);

                             msg->cmd = 1; // come again later
                         }
                         else
                         {
                             msg->trid = get_transaction_id(isRO);

                             if (msg->trid != -1)
                             {
                                 if (isExcl)
                                 {
                                     xmEnterExclusiveMode(msg->sid);
                                 }
                             }

                             msg->cmd = 0; // all ok
                         }

                         break;
                     }
            case 2:  {//give identifier for transaction

                         session_id excl_sid = xmGetExclusiveModeId();
                         bool isRO = (msg->data.data[0]) != 0;

                         give_transaction_id(msg->trid, isRO);

                         if (isRO) // query has just finished; snapshot advancement might be possible
  						 {
                             if (UEventSet(&end_of_rotr_event, __sys_call_error) != 0)
                                 throw SYSTEM_EXCEPTION("Event signaling for possibility of snapshot advancement failed");
						 }
						 else // updater has just ended; check for need to advance snapshots or truncate the log
						 {
                             if (llNeedCheckpoint())
                             {
                                 llActivateCheckpoint(); // maintenance checkpoint
                             }
                             else
                             {
                                 if (UEventSet(&start_checkpoint_snapshot,  __sys_call_error) != 0)
                                      throw SYSTEM_EXCEPTION("Event signaling for checking of snapshot advancement failed");
                             }

                             if (msg->sid == excl_sid)
                             {
                                 xmExitExclusiveMode(); // exclusive tr ended
                             }
                             else if (excl_sid != -1)
                             {
                                 xmTryToStartExclusive(); // updater ended and exclusive tr is waiting: try to start it
                             }
                         }

                         break;
                     }
            case 3:  {//obtain lock on database entity
                         lock_mode mode;
                         resource_kind kind;

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
                         else if (r == LOCK_NOT_LOCKED && !lm_table.deadlock(msg->trid, true)) msg->data.data[0] = '0';
                         else
                         {
                             msg->data.data[0] = '2';
                             tr_lock_head* tr_head = tr_table.find_tr_lock_head(msg->trid);
                             if (tr_head == NULL) throw SYSTEM_EXCEPTION("Incorrect logic in SM's lock manager");
                             tr_head->tran->status = ROLLING_BACK_AFTER_DEADLOCK;
                         }

                         break;
                     }

            case 4:  {//release all transaction's locks
                         lm_table.release_tr_locks(msg->trid);

                         break;
                     }
            case 5:  {
                         resource_kind kind;

                         if (msg->data.data[1] == 'd') kind = LM_DOCUMENT;
                         else if (msg->data.data[1] == 'c') kind = LM_COLLECTION;
                         else if (msg->data.data[1] == 'i') kind = LM_INDEX;
                         else if (msg->data.data[1] == 't') kind = LM_TRIGGER;
                         else kind = LM_DATABASE;

                         lm_table.unlock(msg->trid, resource_id(string((msg->data.data)+2), kind));

                         break;

                     }

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
                         bm_reset_io_statistics();
                         bm_register_session(msg->sid, msg->data.reg.num);
                         msg->data.reg.num = bufs_num;
                         msg->data.reg.mptr = mb->catalog_masterdata_block;
                         msg->data.reg.transaction_flags = mb->transaction_flags;
                         msg->data.reg.layer_size = mb->layer_size;
                         msg->cmd = 0;
                         break;
                     }
            case 22: {
                         //d_printf1("query 22: bm_unregister_session\n");
                         bm_unregister_session(msg->sid);
                         msg->cmd = 0;
                         bm_log_out_io_statistics();
                         break;
                     }
            case 23: {
                         //d_printf1("query 23: bm_allocate_data_block\n");
                         WuAllocateDataBlockExn(msg->sid,
                                                &(msg->data.swap_data.ptr),
                                                &(msg->data.swap_data.offs),
                                                &(msg->data.swap_data.swapped));
                         msg->cmd = 0;
                         break;
                     }
            case 24: {
                         //d_printf1("query 24: bm_allocate_tmp_block\n");
                         WuAllocateTempBlockExn(msg->sid,
                                               &(msg->data.swap_data.ptr),
                                               &(msg->data.swap_data.offs),
                                               &(msg->data.swap_data.swapped));
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
                                      msg->data.swap_data.ptr,
                                      &(msg->data.swap_data.offs),
                                      &(msg->data.swap_data.swapped));
                         msg->cmd = 0;
                         break;
                     }
            case 27: {
                         //d_printf1("query 27: bm_enter_exclusive_mode\n");
                         bm_enter_exclusive_mode(msg->sid, &(msg->data.reg.num), sm_globals::bufs_num);
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
                         bm_memlock_block(msg->sid, *(xptr*)(&(msg->data.ptr)), sm_globals::bufs_num);
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
						 bool isUsingSnapshot = (msg->data.data[0]) != 0;

                         bm_register_transaction(msg->sid, msg->trid);
						 try
						 {
							 WuOnRegisterTransactionExn(msg->sid, isUsingSnapshot, (TIMESTAMP*) &msg->data.snp_ts);
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
                         msg->cmd = 0;

						 WuOnUnregisterTransactionExn(msg->sid);
                         bm_unregister_transaction(msg->sid, msg->trid);

                         if (mb->catalog_masterdata_block != msg->data.ptr) {
                             mb->catalog_masterdata_block = msg->data.ptr;
                             flush_master_block();
                         }

                         msg->cmd = 0;
                         break;
                     }
			case 37:
                     {
						 /* create version for the block */
                         WuCreateBlockVersionExn(msg->sid,
                                      msg->data.swap_data.ptr,
                                      &(msg->data.swap_data.offs),
                                      &(msg->data.swap_data.swapped));
                         msg->cmd = 0;
                         break;
                     }
			case 38:
                     {
						 /* rollback or commit notification */
						 bool isRollback = (msg->data.data[0] != 0);
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

						 if (msg->data.hb_struct.state == HB_START)
							msg->data.hb_struct.state =	hbProcessStartRequest(msg->data.hb_struct.state,
																			  msg->data.hb_struct.is_checkp,
																			  msg->data.hb_struct.incr_state);

						 else if (msg->data.hb_struct.state == HB_ARCHIVELOG)
						 	msg->data.hb_struct.state =	hbProcessLogArchRequest(&(msg->data.hb_struct.lnumber));

						 else if (msg->data.hb_struct.state == HB_GETPREVLOG)
						 	msg->data.hb_struct.state =	hbProcessGetPrevLogRequest(&(msg->data.hb_struct.lnumber));

						 else if (msg->data.hb_struct.state == HB_END)
						 	msg->data.hb_struct.state =	hbProcessEndRequest();

						 else if (msg->data.hb_struct.state == HB_ERR)
						 	msg->data.hb_struct.state =	hbProcessErrorRequest();

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


int main(int argc, char **argv)
{
    program_name_argv_0 = argv[0];
//     pping_client *ppc = NULL;
    char buf[1024];
    SednaUserException ppc_ex = USER_EXCEPTION(SE4400); /* used below in ppc->startup() */
    int sedna_db_version = 0;

    /*Under Solaris there is no SO_NOSIGPIPE/MSG_NOSIGNAL/SO_NOSIGNAL,
    so we must block SIGPIPE with sigignore.*/
#if defined(SunOS)
    sigignore(SIGPIPE);
#endif

    try {

#ifdef SE_MEMORY_MNG
        SafeMemoryContextInit();
#endif

        parse_sm_command_line(argc, argv);

        //!TODO: it seems that this place may be a cause of some problems in future. I need to think about this more carefully, not in the train :)
        gov_header_struct cfg;
        get_sednaconf_values(&cfg);

                
        InitGlobalNames(cfg.os_primitives_id_min_bound, INT_MAX);
        SetGlobalNames();
        
        SetGlobalNamesDB(db_id);

        /* event_logger_init must be after set_global_names */
        event_logger_init(EL_SM, db_name, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);

        elog(EL_LOG, ("Event log is ready"));

#ifdef REQUIRE_ROOT
        if (!uIsAdmin(__sys_call_error)) throw USER_EXCEPTION(SE3064);
#endif

        if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION(SE3001);
        
        USOCKET s;
        s = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);
        if(s == U_SOCKET_ERROR)
            throw USER_EXCEPTION (SE3001);

        if(uconnect_tcp(s, sm_globals::port_number, sm_globals::gov_address, __sys_call_error)!=0)
        {
            ushutdown_close_socket(s, __sys_call_error);
            throw USER_EXCEPTION (SE3003);
        }
        
        MessageExchanger * communicator = new MessageExchanger(s);
        
        elog(EL_LOG, ("SM has connected to GOV successfully"));
        
        CdbParameters * cdb_params = new CdbParameters();        
        /* Database creation section */
        if (sm_globals::cdb_mode == 1) {
          register_cdb_on_gov(communicator);
          elog(EL_LOG, ("CDB has registered on GOV successfully"));
          cdb_params->readCdbParams(communicator);
          cdb_params->setPaths(sm_globals::sedna_data);
          elog(EL_LOG, ("CDB has received parameters"));
          createDb(cdb_params, &cfg);
          elog(EL_LOG, ("CDB almost finished"));
          
        }
        
        
        


        InitGiantLock(); atexit(DestroyGiantLock);

//         ppc = new pping_client(GOV_HEADER_GLOBAL_PTR -> ping_port_number, EL_SM);
//         ppc->startup(ppc_ex);

        elog(EL_LOG, ("Ping client has been started"));


        /* Setup default values from config file */
        
//         !TODO do not to forget that these globals should be passed through command line
//         !TODO transmit it using cdbconfig in cdb_mode
        setup_sm_globals();

        recover_database_by_physical_and_logical_log(db_id);

        if (USemaphoreCreate(&wait_for_shutdown, 0, 1, SEDNA_SM_WAIT_FOR_SHUTDOWN, NULL, __sys_call_error) != 0)
            throw USER_EXCEPTION(SE4206);

        //init transacion ids table
        init_transaction_ids_table();
        elog(EL_DBG, ("init_transaction_ids_table done"));

        //init checkpoint resources
        init_checkpoint_sems();
        elog(EL_DBG, ("init_checkpoint_sems done"));

        //create checkpoint thread
        start_chekpoint_thread();
        elog(EL_DBG, ("start_chekpoint_thread done"));

        //start up logical log
        bool is_stopped_correctly;
        llInit(db_files_path, db_name, max_log_files, &sedna_db_version, &is_stopped_correctly, false);

        elog(EL_LOG, ("Logical log has been started"));

        //enable checkpoints
        llEnableCheckpoints();

        //cleanup temporary files
        if(uCleanupUniqueFileStructs(db_files_path, __sys_call_error) == 1)
            elog(EL_LOG,  ("Temporary files have been deleted"));
        else
            elog(EL_WARN, ("Temporary files haven't been (or partially) deleted"));

        lm_table.init_lock_table();
        elog(EL_DBG, ("init_lock_table done"));

        //start buffer manager
        bm_startup(sm_globals::bufs_num, string(sm_globals::db_files_path), string(sm_globals::db_name));
        elog(EL_LOG, ("Buffer manager started"));

#ifdef _WIN32
        BOOL fSuccess;
        fSuccess = SetConsoleCtrlHandler((PHANDLER_ROUTINE) SMCtrlHandler, TRUE);
        if (!fSuccess) throw USER_EXCEPTION(SE4207);
#else
        // For Control-C or Delete
        if (signal(SIGINT, SMCtrlHandler) == SIG_ERR) throw USER_EXCEPTION(SE4207);
        // For Control-backslash
        if (signal(SIGQUIT, SMCtrlHandler) == SIG_ERR) throw USER_EXCEPTION(SE4207);
        //For reboot or halt
        if (signal(SIGTERM, SMCtrlHandler) == SIG_ERR) throw USER_EXCEPTION(SE4207);
#endif

        try {
            // Starting SSMMsg server
            d_printf1("Starting SSMMsg...");

            ssmmsg = new SSMMsg(SSMMsg::Server,
                sizeof (sm_msg_struct),
                SEDNA_SSMMSG_SM_ID(sm_globals::db_id, buf, 1024),
                SM_NUMBER_OF_SERVER_THREADS,
                U_INFINITE);
            if (ssmmsg->init() != 0)
                throw USER_EXCEPTION(SE3030);

            if (ssmmsg->serve_clients(sm_server_handler) != 0)
                throw USER_EXCEPTION(SE3031);

            WuSetTimestamp(llGetPersTimestamp() + 1);
            WuInitExn(0,0, llGetPersTimestamp());
            elog(EL_LOG, ("Wu is initialized"));

#ifdef RCV_TEST_CRASH
            rcvReadTestCfg(); // prepare recovery tester
#endif
            d_printf1("OK\n");
            sm_blk_stat stat;
            bm_block_statistics(&stat);
            d_printf1("Block statistics:\n");
            d_printf2("free_data_blocks_num = %d\n", stat.free_data_blocks_num);
            d_printf2("free_tmp_blocks_num  = %d\n", stat.free_tmp_blocks_num);
            d_printf2("used_data_blocks_num = %d\n", stat.used_data_blocks_num);
            d_printf2("used_tmp_blocks_num  = %d\n", stat.used_tmp_blocks_num);

            if (sm_globals::cdb_mode == 1) {
              load_metadata(cdb_params, &cfg);
            }
            
            delete cdb_params;
            
            
            register_sm_on_gov(communicator);

            elog(EL_LOG, ("SM has been started"));
            fprintf(res_os, "\nSM has been started\n");
            fflush(res_os);

            
            USemaphoreDown(wait_for_shutdown, __sys_call_error);
            
            unregister_sm_on_gov(communicator);
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
        ssmmsg = NULL;

        //shutdown checkpoint thread (it also makes checkpoint)
        shutdown_chekpoint_thread();

        WuReleaseExn();
        elog(EL_LOG, ("Wu is released"));

        // shutdown bm
        bm_shutdown(sm_globals::bufs_num);

        //shutdown logical log
        llRelease();

        //release checkpoint resources
        release_checkpoint_sems();

        release_transaction_ids_table();

        lm_table.release_lock_table();

        event_logger_release();

//         ppc->shutdown();
//         delete ppc;
//         ppc = NULL;

        close_gov_shm();
        
        delete [] communicator;
        ushutdown_close_socket(s, __sys_call_error);
        
        return 0;

    } catch (SednaUserException &e) {
        fprintf(stderr, "%s\n", e.getMsg().c_str());
        event_logger_release();
        
//         if (ppc) { ppc->shutdown(); delete ppc; ppc = NULL; }
        close_gov_shm();
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
        bool is_stopped_correctly;
        int sedna_db_version = 0;

        if (uGetEnvironmentVariable(SM_BACKGROUND_MODE, buf, 1024, __sys_call_error) != 0)
        {//I am in running sm process

            is_recovery_mode = true;

            event_logger_init(EL_SM, db_name, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
            elog(EL_LOG, ("Event log in recovery procedure is ready"));

            //init transacion ids table
            init_transaction_ids_table();
            elog(EL_DBG, ("init_transaction_ids_table done"));

            //init checkpoint resources
            init_checkpoint_sems();
            elog(EL_DBG, ("init_checkpoint_sems done"));

            elog(EL_LOG, ("Starting database recovery or hot-backup restoration..."));
            fprintf(res_os, "Starting database recovery or hot-backup restoration...\n");
            llInit(db_files_path, db_name, max_log_files, &sedna_db_version, &is_stopped_correctly, true);
            elog(EL_DBG, ("logical log is started"));

            if (sedna_db_version != SEDNA_DATA_STRUCTURES_VER)
            {
                release_checkpoint_sems();
                release_transaction_ids_table();
                if (sedna_db_version != SEDNA_DATA_STRUCTURES_VER)
                    throw USER_EXCEPTION2(SE4212, "Possibly your Sedna installation is newer than database files. You should use export utility (se_exp) to convert database into the latest format. See documentation for details.");
            }

            d_printf1("logical log has been started successfully\n");


            //create checkpoint thread
            start_chekpoint_thread();
            elog(EL_DBG, ("start_chekpoint_thread done"));

            // check for tmp file (may be absent in hot-backup copy)
            string tmp_file_name = string(db_files_path) + string(db_name) + ".setmp";
            if (!uIsFileExist(tmp_file_name.c_str(), __sys_call_error))
            {
                USECURITY_ATTRIBUTES *sa;
                UFile tmp_file_handle;

                if (uCreateSA(&sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error) !=0 ) throw USER_EXCEPTION(SE3060);

                if ((tmp_file_handle = uCreateFile(tmp_file_name.c_str(), U_SHARE_READ, U_READ_WRITE, U_NO_BUFFERING, sa, __sys_call_error)) == U_INVALID_FD)
                    throw USER_EXCEPTION(SE4301);

                if (uCloseFile(tmp_file_handle, __sys_call_error) == 0)
                    throw USER_EXCEPTION(SE4305);

                if (uReleaseSA(sa, __sys_call_error) !=0 ) throw USER_EXCEPTION(SE3063);
            }

            //start buffer manager
            bm_startup(sm_globals::bufs_num, sm_globals::db_files_path, sm_globals::db_name);
            elog(EL_LOG, ("Buffer manager is started"));

            //recover data base by physical log
            LSN last_checkpoint_lsn = LFS_INVALID_LSN;
            if (!is_stopped_correctly)
            {
                last_checkpoint_lsn = llRecoverPhysicalState();
                elog(EL_LOG, ("Database has been recovered by physical log successfully"));
            }

            /*
             * recover tmp file
             * we recreate it on usual start also since we want always to reset
             * its initial size
             */
            recreate_tmp_file();

            //disable checkpoints
            llDisableCheckpoints();
            elog(EL_LOG, ("Checkpoints are disabled"));

            lm_table.init_lock_table();
            elog(EL_DBG, ("lm_table.init_lock_table done"));

            // Starting SSMMsg server
            d_printf1("Starting SSMMsg...");

            ssmmsg = new SSMMsg(SSMMsg::Server,
                sizeof (sm_msg_struct),
                SEDNA_SSMMSG_SM_ID(db_id, buf, 1024),
                SM_NUMBER_OF_SERVER_THREADS,
                U_INFINITE);
            if (ssmmsg->init() != 0)
                throw USER_EXCEPTION(SE3030);

            if (ssmmsg->serve_clients(sm_server_handler) != 0)
                throw USER_EXCEPTION(SE3031);

            d_printf1("OK\n");

            WuSetTimestamp(llGetPersTimestamp() + 1);
            WuInitExn(0,0,llGetPersTimestamp()); // turn on versioning mechanism on recovery
            elog(EL_LOG, ("Wu is initialized"));

            //recover database by logical log
            if (!is_stopped_correctly)
            {
                execute_recovery_by_logical_log_process(last_checkpoint_lsn);
                elog(EL_LOG, ("Database has been recovered by logical log successfully"));
            }

            //enable checkpoints
            llEnableCheckpoints();
            elog(EL_LOG, ("Checkpoints are enabled"));

            if (ssmmsg->stop_serve_clients() != 0)
                throw USER_EXCEPTION(SE3032);

            if (ssmmsg->shutdown() != 0)
                throw USER_EXCEPTION(SE3033);

            //shutdown checkpoint thread (it also makes checkpoint)
            shutdown_chekpoint_thread(); // checkpont is created here!
            elog(EL_LOG, ("Shutdown checkpoint thread done"));

            WuReleaseExn();
            elog(EL_LOG, ("Wu is released"));

            // shutdown bm
            bm_shutdown(sm_globals::bufs_num);
            elog(EL_LOG, ("Buffer manager is stopped"));

            //shutdown logical log
            llRelease();
            elog(EL_DBG, ("Logical log is stopped"));

            //release checkpoint resources
            release_checkpoint_sems();
            elog(EL_DBG, ("release_checkpoint_sems done"));

            release_transaction_ids_table();
            elog(EL_DBG, ("release_transaction_ids_table done"));

            lm_table.release_lock_table();
            elog(EL_DBG, ("lm_table.release_lock_table done"));

            elog(EL_LOG, ("Recovery procedure has been finished successfully"));
            event_logger_release();

            is_recovery_mode = false;
        }
    } catch (SednaUserException &e) {
        fprintf(stderr, "%s\n", e.what());
        throw USER_EXCEPTION(SE4205);
    } catch (SednaException&) {
        throw;
    } catch (ANY_SE_EXCEPTION) {
        throw;
    }

}
