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
// #include "common/pping.h"
#include "common/ipc_ops.h"

#include "tr/tr_globals.h"
#include "tr/tr_functions.h"
#include "tr/tr_common_funcs.h"
#include "tr/tr_utils.h"
#include "tr/cl_client.h"
#include "tr/socket_client.h"
#include "tr/auth/auc.h"
#include "tr/rcv/rcv_test_tr.h"
#include "tr/rcv/rcv_funcs.h"
#include "common/gmm.h"

using namespace std;
using namespace tr_globals;

/* variables for time measurement */
DECLARE_TIME_VARS
u_timeb t_total1, t_total2, t_qep1, t_qep2, t_qep;

int TRmain(int argc, char *argv[])
{
    /* volatile to prevent clobbing by setjmp/longjmp */
    volatile int ret_code = 0;
    volatile bool sedna_server_is_running = false;
    program_name_argv_0 = argv[0]; /* we use it to get full binary path */
//     tr_globals::ppc = NULL; /* pping client */
    char buf[1024]; /* buffer enough to get environment variables */
    SSMMsg *sm_server = NULL; /* shared memory messenger to communicate with SM */
    int determine_vmm_region = -1;
//     int os_primitives_id_min_bound;
    SednaUserException e = USER_EXCEPTION(SE4400);

    try
    {
//         if (uGetEnvironmentVariable(SEDNA_OS_PRIMITIVES_ID_MIN_BOUND, buf, 1024, NULL) != 0)
//             /* Default value for command line only */
//             os_primitives_id_min_bound = 1500;
//         else
//             os_primitives_id_min_bound = atoi(buf);
        
//         uSleep(10, __sys_call_error);
        
        parse_trn_command_line(argc, argv);
        
        INIT_TOTAL_TIME_VARS u_ftime(&t_total1);

        /*
         * determine_vmm_region specifies db_id for which to search layer_size
         * db_id is needed since we must report back to the proper cdb
         */
        if (uGetEnvironmentVariable(SEDNA_DETERMINE_VMM_REGION, buf, 1024, NULL) != 0)
            determine_vmm_region = -1;
        else
            determine_vmm_region = atoi(buf);

        /* Probably it is wrong since we get region info from global shared memory and if we
         * run tr from command line (no environment variable set) and non-default id_min_bound
         * is used we won't find the shmem. Also we can erroneously access shmem from the
         * wrong Sedna installation which is not good (different vmm region settings are
         * possible though the probability is low, however the worse thing is that we wreck isolation
         * of unrelated installations; we may even fail if the shmem is created by the
         * installation running as a different user). */

        InitGlobalNames(tr_globals::os_primitives_min_bound, INT_MAX);
        SetGlobalNames();

        if (determine_vmm_region != -1)
        {
            SetGlobalNamesDB(determine_vmm_region);
            vmm_determine_region();
            ReleaseGlobalNames();
            return 0;
        }
        else
        {
            OS_EXCEPTIONS_INSTALL_HANDLER
        }

        ReleaseGlobalNames();

#ifdef REQUIRE_ROOT
        if (!uIsAdmin(__sys_call_error)) throw USER_EXCEPTION(SE3064);
#endif /* REQUIRE_ROOT */

        /* Determine if we run via GOV or via command line */
        bool server_mode = false;

        if (uGetEnvironmentVariable(SEDNA_SERVER_MODE, buf, 1024, __sys_call_error) == 0)
            server_mode = (atoi(buf) == 1);

        /* We got load metadata transaction started by CDB*/
        first_transaction = (uGetEnvironmentVariable(SEDNA_LOAD_METADATA_TRANSACTION, buf, 1024, __sys_call_error) == 0);

        /* We got recovery transaction started ny SM */
        run_recovery = (uGetEnvironmentVariable(SEDNA_RUN_RECOVERY_TRANSACTION, buf, 1024, __sys_call_error) == 0);
        
        if (uSocketInit(__sys_call_error) != 0) throw USER_EXCEPTION(SE3001);
        
        if (first_transaction || run_recovery || !server_mode) {
            
            /* We don't allow running se_trn directly */
            if (strcmp(ACTIVE_CONFIGURATION, "Release") == 0 && !first_transaction && !run_recovery)
                throw USER_EXCEPTION(SE4613);

            client = new command_line_client(argc, argv);
            if (!sedna_server_is_running) throw USER_EXCEPTION(SE4400);
        }
        else
        {
            client = new socket_client();
        }

        client->init();
        
        /* init global names */
        InitGlobalNames(client->get_os_primitives_id_min_bound(), INT_MAX);
        SetGlobalNames();

        fflush(stdout);
        
        open_gov_shm();

        SEDNA_DATA = tr_globals::sedna_data;
        SetGlobalNamesDB(db_id);

        if (!run_recovery) {
            /* register session on governor */
            client->register_session_on_gov();
        }
        else {
            /* cannot register for recovery since the
             * database has status 'not started' */
            sid = 0;
        }

        client->get_session_parameters();
        
//         tr_globals::ppc = new pping_client(GOV_HEADER_GLOBAL_PTR -> ping_port_number,
//                                            run_recovery ? EL_RCV : EL_TRN,
//                                            run_recovery ? NULL : &tr_globals::is_timer_fired);
//         tr_globals::ppc->startup(e);

        event_logger_init((run_recovery) ? EL_RCV : EL_TRN, db_name, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
        event_logger_set_sid(sid);

        if (!run_recovery && !first_transaction)
        {
            /* doing somehing only for command line client */
            client->write_user_query_to_log();
            /* doing something only for command line client */
//             client->set_keep_alive_timeout(GOV_HEADER_GLOBAL_PTR -> ka_timeout);
            client->set_keep_alive_timeout(tr_globals::ka_timeout);
            /* set keyboard handlers */
            set_trn_ctrl_handler();
        }

        msg_struct client_msg;

        /* transaction initialization */
        on_session_begin(sm_server, db_id, run_recovery);
        elog(EL_LOG, ("Session is ready"));

        /* stores QEP of the current statement */
        PPQueryEssence *qep_tree = NULL;
        StmntsArray *st = NULL;
        bool expect_another_transaction = !run_recovery;

#ifdef RCV_TEST_CRASH
        rcvReadTestCfg(); // prepare recovery tester
#endif

        /* recovery routine is run instead of transaction mix */
        if (run_recovery)
        {
            on_transaction_begin(sm_server, /*tr_globals::ppc, */true); // true means recovery is active
            on_kernel_recovery_statement_begin();

            recover_db_by_logical_log();

            on_kernel_recovery_statement_end();
            on_transaction_end(sm_server, true, /*tr_globals::ppc,*/ true);
        }

        /////////////////////////////////////////////////////////////////////////////////
        /// CYCLE BY TRANSACTIONS
        /////////////////////////////////////////////////////////////////////////////////
        while (expect_another_transaction)
        {
            client->read_msg(&client_msg);
            if (client_msg.instruction == se_BeginTransaction)  //BeginTransaction
            
            {
                try
                {
                    on_transaction_begin(sm_server/*, tr_globals::ppc*/);
                    client->respond_to_client(se_BeginTransactionOk);

                    qep_tree = NULL; //qep of current stmnt
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
                                do_authentication();
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
                                //print for test system
                                d_printf1("\n============== statement =================\n");

                                // close previous statement
                                on_user_statement_end(qep_tree, st);

                                /* Adjust client for the new statement */
                                client->set_result_type((enum se_output_method) (client_msg.body[0]));
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
                                on_transaction_end(sm_server, true /*COMMIT*//*, tr_globals::ppc*/);
                                ret_code = 0;

                                client->respond_to_client(se_CommitTransactionOk);
                                d_printf1("============== Trn execution finished ======================\n");
                                break;
                            }

                        case se_RollbackTransaction:   //rollback command
                            {
                                on_user_statement_end(qep_tree, st);
                                on_transaction_end(sm_server, false /*ROLLBACK*//*, tr_globals::ppc*/);
                                ret_code = 0;

                                client->respond_to_client(se_RollbackTransactionOk);
                                d_printf1("============== Trn execution finished ======================\n");
                                break;
                            }
                        case se_ShowTime:      //show time
                            {
                                client->show_time(t_qep);
                                break;
                            }
                        case se_CloseConnection:       //close connection
                            {
                                on_user_statement_end(qep_tree, st);
                                on_transaction_end(sm_server, false /*ROLLBACK*//*, tr_globals::ppc*/);
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
                    on_transaction_end(sm_server, false /*ROLLBACK*//*, tr_globals::ppc*/);
                    ret_code = 1;

                    d_printf1("\nTr is rolled back successfully\n");

                    /* Client session must be closed if we
                    * have one of the following errors */
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
                if (!run_recovery)
                {   
                  client->unregister_session_on_gov();
                }
                client->release();
                delete client;
                expect_another_transaction = false;
            }
            else if (client_msg.instruction == se_ShowTime)             // ShowTime
            {
                client->show_time(t_qep);
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
        }
        /////////////////////////////////////////////////////////////////////////////////
        /// END OF CYCLE BY TRANSACTIONS
        /////////////////////////////////////////////////////////////////////////////////


        on_session_end(sm_server);

        if (run_recovery)
            elog(EL_LOG, ("recovery process by logical log finished"));
        else
            elog(EL_LOG, ("Session is closed"));


        if (show_time == 1)
        {
            u_ftime(&t_total2);
            string total_time = to_string(t_total2 - t_total1);
            cerr << "\nStatistics: total time: " << total_time.c_str() << " secs\n";
            cerr << "<step>\t\t\t<time>\n";
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
//         tr_globals::ppc->shutdown();
//         delete tr_globals::ppc;
//         tr_globals::ppc = NULL;

        if (!run_recovery)
        {   
            set_session_finished();
        }

        event_logger_set_sid(-1);

        close_gov_shm();

        uSocketCleanup(__sys_call_error);

        d_printf1("Transaction has been closed\n\n");

        /* tell SM that we are done */
        if (run_recovery)
        {
            USemaphore rcv_signal_end;

            /* signal to SM that we are finished */
            if (0 != USemaphoreOpen(&rcv_signal_end, SEDNA_DB_RECOVERED_BY_LOGICAL_LOG, __sys_call_error))
                throw SYSTEM_EXCEPTION("Cannot open SEDNA_DB_RECOVERED_BY_LOGICAL_LOG!");

            if (0 != USemaphoreUp(rcv_signal_end, __sys_call_error))
                throw SYSTEM_EXCEPTION("Cannot up SEDNA_DB_RECOVERED_BY_LOGICAL_LOG!");

            if (0 != USemaphoreClose(rcv_signal_end, __sys_call_error))
                throw SYSTEM_EXCEPTION("Cannot close SEDNA_DB_RECOVERED_BY_LOGICAL_LOG!");
        }
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
//         if (tr_globals::ppc)
//         {
//             tr_globals::ppc->shutdown();
//             delete tr_globals::ppc;
//             tr_globals::ppc = NULL;
//         }
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

    return ret_code;
}
