/*
* File:  tr.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include <string>

#include "common/sedna.h"
#include "common/base.h"
#include "common/utils.h"
#include "common/SSMMsg.h"
#include "common/errdbg/d_printf.h"
#include "common/pping.h"
#include "common/mmgr/memutils.h"
#include "common/ipc_ops.h"

#include "tr/tr_globals.h"
#include "tr/tr_functions.h"
#include "tr/tr_common_funcs.h"
#include "tr/tr_utils.h"
#include "tr/cl_client.h"
#include "tr/socket_client.h"
#include "tr/auth/auc.h"
#include "tr/rcv/rcv_test_tr.h"

// only for MSDEV 6.0
#if (_MSC_VER == 1200) && (WINVER < 0x0500)
extern "C" long _ftol(double);  //defined by VC6 C libs
extern "C" long _ftol2(double dblSource)
{
    return _ftol(dblSource);
}
#endif


using namespace std;
using namespace tr_globals;


DECLARE_TIME_VARS
// variables for time measurement
u_timeb t_total1, t_total2, t_qep1, t_qep2, t_total, t_qep;


string total_time = "0.000 secs", qep_time = "0.000 secs";

#ifdef _WIN32
BOOL TrnCtrlHandler(DWORD fdwCtrlType)
{
    switch (fdwCtrlType)
    {
    case CTRL_C_EVENT:         // Handle the CTRL+C signal.
    case CTRL_CLOSE_EVENT:     // CTRL+CLOSE: confirm that the user wants to exit.
    case CTRL_BREAK_EVENT:
    case CTRL_LOGOFF_EVENT:
    case CTRL_SHUTDOWN_EVENT:
        return TRUE;
    default:
        return FALSE;
    }
}
#else
#include <signal.h>

void TrnCtrlHandler(int signo)
{
    if (signo == SIGINT || signo == SIGQUIT || signo == SIGTERM)
    {
        //beep();
    }
}
#endif


int TRmain(int argc, char *argv[])
{
    int ret_code = 0;

    //DebugBreak();
    /*
    if (AllocConsole())
    {
    freopen("CON","wt",stderr);
    freopen("CON","wt",stdout);
    }
    */

#ifdef SE_MEMORY_TRACK
    {
#endif

        program_name_argv_0 = argv[0];
        tr_globals::ppc = NULL;
        char buf[1024];
        SSMMsg *sm_server = NULL;
        int determine_vmm_region = 0;
        bool sedna_server_is_running = false;
        int os_primitives_id_min_bound;
        SednaUserException e = USER_EXCEPTION(SE4400);

        try
        {
            if (uGetEnvironmentVariable(SEDNA_OS_PRIMITIVES_ID_MIN_BOUND, buf, 1024, NULL) != 0)
                os_primitives_id_min_bound = 1500; //default value for command line only
            else
                os_primitives_id_min_bound = atoi(buf);

            INIT_TOTAL_TIME_VARS u_ftime(&t_total1);

            if (uGetEnvironmentVariable(SEDNA_DETERMINE_VMM_REGION, buf, 1024, NULL) != 0)
                determine_vmm_region = 0;
            else
                determine_vmm_region = atoi(buf);

            /* Probably it is wrong since we get region info from global shared memory and if we
             * run tr from command line (no environment variable set) and non-default id_min_bound
             * is used we won't find the shmem. Also we can erroneously access shmem from the
             * wrong Sedna installation which is not good (different vmm region settings are
             * possible though the probability is low, however the worse thing is that we wreck isolation
             * of unrelated installations; we may even fail if the shmem is created by the
             * installation running as a different user). ZN */

            InitGlobalNames(os_primitives_id_min_bound, INT_MAX);
            SetGlobalNames();

            if (determine_vmm_region == 1)
            {
                vmm_determine_region();
                return 0;
            }
            else
            {
                try {
                    vmm_preliminary_call();
                    sedna_server_is_running = true;
                } catch (SednaUserException &e) {
                    if (e.get_code() != SE4400) throw;
                }
                OS_EXCEPTIONS_INSTALL_HANDLER
            }

            ReleaseGlobalNames();

#ifdef SE_MEMORY_MNG
            SafeMemoryContextInit();

            TransactionContext = AllocSetContextCreate(TopMemoryContext, "TransactionContext", ALLOCSET_DEFAULT_MINSIZE, ALLOCSET_DEFAULT_INITSIZE, ALLOCSET_DEFAULT_MAXSIZE);
            UserStatementContext = AllocSetContextCreate(TransactionContext, "UserStatementContext", ALLOCSET_DEFAULT_MINSIZE, ALLOCSET_DEFAULT_INITSIZE, ALLOCSET_DEFAULT_MAXSIZE);
            // KernelStatementContext = AllocSetContextCreate(UserStatementContext, "KernelStatementContext", ALLOCSET_DEFAULT_MINSIZE, ALLOCSET_DEFAULT_INITSIZE, ALLOCSET_DEFAULT_MAXSIZE);
            XQParserContext = AllocSetContextCreate(UserStatementContext, "XQParserContext", ALLOCSET_DEFAULT_MINSIZE, ALLOCSET_DEFAULT_INITSIZE, ALLOCSET_DEFAULT_MAXSIZE);
#endif

#ifdef REQUIRE_ROOT
            if (!uIsAdmin(__sys_call_error))
                throw USER_EXCEPTION(SE3064);
#endif

            if (uGetEnvironmentVariable(SEDNA_SERVER_MODE, buf, 1024, __sys_call_error) != 0)
                server_mode = 0;
            else
                server_mode = atoi(buf);


            if (server_mode == 1)
                client = se_new socket_client();
            else                    //server mode  = 0 (run from command line)
            {
                if (strcmp(ACTIVE_CONFIGURATION, "Release") == 0 &&
                    uGetEnvironmentVariable(SEDNA_LOAD_METADATA_TRANSACTION, buf, 1024, __sys_call_error) != 0)
                    throw USER_EXCEPTION(SE4613);

                if (uGetEnvironmentVariable(SEDNA_LOAD_METADATA_TRANSACTION, buf, 1024, __sys_call_error) != 0) {
                    first_transaction = 0;
                } else {
                    first_transaction = 1;
                }

                client = se_new command_line_client(argc, argv);
                if (!sedna_server_is_running) throw USER_EXCEPTION(SE4400);
            }

            if (uSocketInit(__sys_call_error) != 0)
                throw USER_EXCEPTION(SE3001);

            client->init();
            client->get_session_parameters();

            //init global names
            InitGlobalNames(client->get_os_primitives_id_min_bound(), INT_MAX);
            SetGlobalNames();

            open_gov_shm();

            //get global configuration
            socket_port     = GOV_HEADER_GLOBAL_PTR -> lstnr_port_number;
            SEDNA_DATA      = GOV_HEADER_GLOBAL_PTR -> SEDNA_DATA;
            max_stack_depth = GOV_HEADER_GLOBAL_PTR -> pp_stack_depth;

#ifdef SE_MEMORY_TRACK
            strcpy(MT_SEDNA_DATA, SEDNA_DATA);
#endif

            if (!check_database_existence(db_name)) //check database consistency (all files exists)
                throw USER_EXCEPTION2(SE4609, db_name);

            int db_id = get_db_id_by_name(GOV_CONFIG_GLOBAL_PTR, db_name);

            if (db_id == -1)//there is no such database
                throw USER_EXCEPTION2(SE4200, db_name);

            SetGlobalNamesDB(db_id);

            //register session on governer
            register_session_on_gov();

            tr_globals::ppc = se_new pping_client(GOV_HEADER_GLOBAL_PTR -> ping_port_number, EL_TRN, &tr_globals::is_timer_fired);
            tr_globals::ppc->startup(e);

            event_logger_init(EL_TRN, db_name, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
            event_logger_set_sid(sid);

            client->write_user_query_to_log();                                    /// it works only for command line client
            client->set_keep_alive_timeout(GOV_HEADER_GLOBAL_PTR -> ka_timeout);  /// it works only for socket client

#ifdef _WIN32
            BOOL fSuccess;
            fSuccess = SetConsoleCtrlHandler((PHANDLER_ROUTINE) TrnCtrlHandler, TRUE);      // add to list
            if (!fSuccess)
                throw USER_EXCEPTION(SE4207);
#else
            // For Control-C or Delete
            if ((int) signal(SIGINT, TrnCtrlHandler) == -1)
                throw USER_EXCEPTION(SE4207);
            // For Control-backslash
            if ((int) signal(SIGQUIT, TrnCtrlHandler) == -1)
                throw USER_EXCEPTION(SE4207);
            //For reboot or halt
            if ((int) signal(SIGTERM, TrnCtrlHandler) == -1)
                throw USER_EXCEPTION(SE4207);
#endif

            msg_struct client_msg;

            // transaction initialization
            on_session_begin(sm_server, db_id);
            elog(EL_LOG, ("Session is ready"));

            PPQueryEssence *qep_tree = NULL;        //qep of current stmnt
            StmntsArray *st = NULL;
            bool expect_another_transaction = true;

#ifdef RCV_TEST_CRASH
            rcvReadTestCfg(); // prepare recovery tester
#endif
            /////////////////////////////////////////////////////////////////////////////////
            /// CYCLE BY TRANSACTIONS
            /////////////////////////////////////////////////////////////////////////////////
            while (expect_another_transaction) //cycle by transactions
            {
#ifdef SE_MEMORY_MNG
                MemoryContextSwitchTo(TransactionContext);
#endif

                client->read_msg(&client_msg);
                if (client_msg.instruction == se_BeginTransaction)  //BeginTransaction
                {
                    try
                    {
                        on_transaction_begin(sm_server, tr_globals::ppc);
                        client->respond_to_client(se_BeginTransactionOk);

                        qep_tree = NULL;    //qep of current stmnt
                        st = NULL;
                        qepNextAnswer item_status = se_no_next_item;

                        d_printf1("============== Trn execution started ======================\n");


                        /////////////////////////////////////////////////////////////////////////////////
                        /// CYCLE BY COMMANDS OF ONE TRANSACTION
                        /////////////////////////////////////////////////////////////////////////////////
                        do          //cycle by commands of one transaction
                        {
                            client->read_msg(&client_msg);
                            switch (client_msg.instruction)
                            {
                            case se_Authenticate:  //authentication
                                {
#ifdef SE_MEMORY_MNG
                                    MemoryContextSwitchTo(UserStatementContext);
#endif
                                    do_authentication();
#ifdef SE_MEMORY_MNG
                                    MemoryContextReset(UserStatementContext);
                                    MemoryContextSwitchTo(TransactionContext);
#endif
                                    client->authentication_result(true, "");
                                    break;
                                }

                            case se_ExecuteSchemeProgram:  //Execute Scheme program
                                {
                                    d_printf2("Scheme program from file=%s is executed\n", client_msg.body);
                                    break;
                                }

                            case se_ExecuteLong:   //execute long query command
                            case se_Execute:       //execute query command
                                {
                                    u_ftime(&t_qep1);
#ifdef SE_MEMORY_MNG
                                    MemoryContextSwitchTo(UserStatementContext);
#endif

                                    //print for test system
                                    d_printf1("\n============== statement =================\n");

                                    // close previous statement
                                    on_user_statement_end(qep_tree, st);

                                    /* Adjust client for the new statement */
                                    client->set_result_type(&client_msg);
                                    client->user_statement_begin();

                                    on_user_statement_begin(client->get_query_type(),
                                                            client->get_query_string(&client_msg),
                                                            qep_tree, st);


                                    if (qep_tree->is_update())
                                    {
                                        GET_TIME(&t1_exec);

                                        execute(qep_tree);
                                        GET_TIME(&t2_exec);
                                        client->respond_to_client(se_UpdateSucceeded);
                                        on_user_statement_end(qep_tree, st);
                                    }
                                    else if (!(qep_tree->supports_next()))
                                    {
                                        client->respond_to_client(se_QuerySucceeded);
                                        GET_TIME(&t1_exec);
                                        item_status = execute(qep_tree);
                                        GET_TIME(&t2_exec);

                                        client->end_item(item_status);

                                        on_user_statement_end(qep_tree, st);
                                    }
                                    else
                                    {
                                        client->respond_to_client(se_QuerySucceeded);
                                        GET_TIME(&t1_exec);
                                        item_status = next(qep_tree);
                                        GET_TIME(&t2_exec);

                                        client->end_item(item_status);
                                        if (item_status != se_next_item_exists) {
                                            on_user_statement_end(qep_tree, st);
                                        }
                                    }

                                    u_ftime(&t_qep2);
                                    t_qep = (t_qep2 - t_qep1);
                                    qep_time = to_string(t_qep);

                                    ADD_TIME(t_total_exec, t1_exec, t2_exec);

                                    break;
                                }

                            case se_GetNextItem:   //next portion command
                                {
                                    u_ftime(&t_qep1);
                                    if (qep_tree) // if statement execution is in progress
                                    {
                                        if (item_status == se_next_item_exists)
                                        {
                                            GET_TIME(&t1_exec);
                                            item_status = next(qep_tree);

                                            GET_TIME(&t2_exec);
                                        }
                                        client->end_item(item_status);
                                        u_ftime(&t_qep2);

                                        t_qep = (t_qep2 - (t_qep1 - t_qep));

                                        qep_time = to_string(t_qep);
                                        if (item_status != se_next_item_exists)
                                        {
                                            on_user_statement_end(qep_tree, st);
                                        }
                                    }

                                    ADD_TIME(t_total_exec, t1_exec, t2_exec);

                                    break;
                                }

                            case se_CommitTransaction:     //commit command
                                {
                                    on_user_statement_end(qep_tree, st);
                                    on_transaction_end(sm_server, true /*COMMIT*/, tr_globals::ppc);
                                    ret_code = 0;

                                    client->respond_to_client(se_CommitTransactionOk);
                                    d_printf1("============== Trn execution finished ======================\n");
                                    break;
                                }

                            case se_RollbackTransaction:   //rollback command
                                {
                                    on_user_statement_end(qep_tree, st);
                                    on_transaction_end(sm_server, false /*ROLLBACK*/, tr_globals::ppc);
                                    ret_code = 0;

                                    client->respond_to_client(se_RollbackTransactionOk);
                                    d_printf1("============== Trn execution finished ======================\n");
                                    break;
                                }
                            case se_ShowTime:      //show time
                                {
                                    client->show_time(qep_time);
                                    break;
                                }
                            case se_CloseConnection:       //close connection
                                {
                                    on_user_statement_end(qep_tree, st);
                                    on_transaction_end(sm_server, false /*ROLLBACK*/, tr_globals::ppc);
                                    ret_code = 1;

                                    client->respond_to_client(se_TransactionRollbackBeforeClose);
                                    d_printf1("============== Trn execution finished ======================\n");
                                    break;
                                }
                            case se_SetSessionOptions:
                                {
                                    client->set_session_options(&client_msg);
                                    break;
                                }
                            case se_ResetSessionOptions:
                                {
                                    client->reset_session_options();
                                    break;
                                }
                            default:
                                {
                                    client->process_unknown_instruction(client_msg.instruction, true);
                                    break;
                                }
                            }       //end switch

                        }
                        while (client_msg.instruction != se_CommitTransaction && client_msg.instruction != se_RollbackTransaction && client_msg.instruction != se_CloseConnection);
                        /////////////////////////////////////////////////////////////////////////////////
                        /// END OF CYCLE BY COMMANDS OF ONE TRANSACTION
                        /////////////////////////////////////////////////////////////////////////////////

                    }
                    catch(SednaUserException & e)
                    {
                        on_user_statement_end(qep_tree, st);
                        on_transaction_end(sm_server, false /*ROLLBACK*/, tr_globals::ppc);
                        ret_code = 1;

                        d_printf1("\nTr is rolled back successfully\n");

                        /* Client session must be closed if we
                        * have one of the following errors
                        */
                        if (e.get_code() == SE3053 || e.get_code() == SE3006 || e.get_code() == SE3007 || e.get_code() == SE3009 || e.get_code() == SE3012)
                        {
                            ret_code = 1;
                            throw;
                        }

                        fprintf(stderr, "%s\n", e.what());
                        client->error(e.get_code(), e.getMsg());
                    }
                    catch(SednaException & e)
                    {
                        sedna_soft_fault(e, EL_TRN);
                    }
                    catch(ANY_SE_EXCEPTION)
                    {
                        sedna_soft_fault(EL_TRN);
                    }
                }
                else if (client_msg.instruction == se_CloseConnection)      // CloseConnection
                {
                    client->respond_to_client(se_CloseConnectionOk);
                    client->release();
                    delete client;
                    expect_another_transaction = false;
                }
                else if (client_msg.instruction == se_ShowTime)             // ShowTime
                {
                    client->show_time(qep_time);
                }
                else if (client_msg.instruction == se_SetSessionOptions)    // Set session options
                {
                    client->set_session_options(&client_msg);
                }
                else if (client_msg.instruction == se_ResetSessionOptions)  // Reset all session options to their default values
                {
                    client->reset_session_options();
                }
                else
                {
                    client->process_unknown_instruction(client_msg.instruction, false);
                }

#ifdef SE_MEMORY_MNG
                MemoryContextResetChildren(TransactionContext);
                MemoryContextReset(TransactionContext);
#endif

            } // end 'while' by transactions
            /////////////////////////////////////////////////////////////////////////////////
            /// END OF CYCLE BY TRANSACTIONS
            /////////////////////////////////////////////////////////////////////////////////


            on_session_end(sm_server);
            elog(EL_LOG, ("Session is closed"));


            u_ftime(&t_total2);
            total_time = to_string(t_total2 - t_total1);

            //   PRINT_DEBUG_TIME_RESULTS

            if (show_time == 1)
            {
                cerr << "\nStatistics: total time: " << total_time.c_str() << "\n";
                cerr << "<step>\t\t\t<time>\n";
                //cerr << "query execution\t\t" << qep_time.c_str() << "\n";
                cerr << endl;
#ifdef VMM_GATHER_STATISTICS
                cerr << "vmm_different_blocks_touched : " << vmm_different_blocks_touched() << endl;
                cerr << "vmm_blocks_touched           : " << vmm_blocks_touched() << endl;
                cerr << "vmm_different_blocks_modified: " << vmm_different_blocks_modified() << endl;
                cerr << "vmm_blocks_modified          : " << vmm_blocks_modified() << endl;
                cerr << "vmm_data_blocks_allocated    : " << vmm_data_blocks_allocated() << endl;
                cerr << "vmm_tmp_blocks_allocated     : " << vmm_tmp_blocks_allocated() << endl;
                cerr << "vmm_number_of_checkp_calls   : " << vmm_number_of_checkp_calls() << endl;
                cerr << "vmm_number_of_sm_callbacks   : " << vmm_number_of_sm_callbacks() << endl;
                cerr << endl;
#endif
                PRINT_DEBUG_TIME_RESULTS}

            event_logger_release();
            tr_globals::ppc->shutdown();
            delete tr_globals::ppc;
            tr_globals::ppc = NULL;
            set_session_finished();
            event_logger_set_sid(-1);

            close_gov_shm();

            uSocketCleanup(__sys_call_error);

            d_printf1("Transaction has been closed\n\n");

        }
        catch(SednaUserException & e)
        {
            fprintf(stderr, "%s\n", e.what());

            on_session_end(sm_server);
            elog(EL_LOG, ("Session is closed"));
            try
            {
                if (client != NULL)
                {
                    if (e.get_code() == SE3053)
                        client->authentication_result(false, e.getMsg());
                    else
                        client->error(e.get_code(), e.getMsg());

                    client->release();
                    delete client;
                }
            }
            catch(ANY_SE_EXCEPTION)
            {
                d_printf1("Connection with client has been broken\n");
            }
            event_logger_release();
            if (tr_globals::ppc)
            {
                tr_globals::ppc->shutdown();
                delete tr_globals::ppc;
                tr_globals::ppc = NULL;
            }
            set_session_finished();
            close_gov_shm();
            uSocketCleanup(__sys_call_error);
            ret_code = 1;
        }
        catch(SednaException & e)
        {
            sedna_soft_fault(e, EL_TRN);
        }
        catch(ANY_SE_EXCEPTION)
        {
            sedna_soft_fault(EL_TRN);
        }

#ifdef SE_MEMORY_TRACK
    }
    DumpUnfreed(EL_TRN);
#endif

    return ret_code;
}
