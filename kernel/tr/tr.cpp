/*
 * File:  tr.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include <iostream>
#include <string>
#include <string.h>

#include "common/base.h"
#include "common/utils.h"
#include "common/SSMMsg.h"
#include "tr/tr_globals.h"
#include "tr/tr_functions.h"
#include "tr/pq/pq.h"
#include "tr/crmutils/exec_output.h"
#include "tr/executor/por2qep/por2qep.h"
#include "common/errdbg/d_printf.h"
#include "common/pping.h"
#include "tr/cl_client.h"
#include "tr/socket_client.h"
#include "tr/tr_utils.h"
#include "tr/auth/auc.h"
#include "common/mmgr/memutils.h"
#include "tr/tr_common_funcs.h"

// only for MSDEV 6.0
#if (_MSC_VER == 1200) && (WINVER < 0x0500)
extern "C" long _ftol(double);  //defined by VC6 C libs
extern "C" long _ftol2(double dblSource)
{
    return _ftol(dblSource);
}
#endif


using namespace std;

// should be removed later !!!
char db_files_path[U_MAX_PATH + 1];

DECLARE_TIME_VARS
// variables for time measurement
u_timeb t_total1, t_total2, t_qep1, t_qep2, t_total, t_qep;


//u_timeb t_test1, t_test2, t_test3, t_test4;
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


//u_timeb ttt1, ttt2;


int main(int argc, char *argv[])
{
    int ret_code = 0;
	//DebugBreak();
	if (AllocConsole())
	{
		freopen("CON","wt",stderr);
		freopen("CON","wt",stdout);
	}
	
//	DebugBreak();
#ifdef SE_MEMORY_TRACK
    {
#endif

    program_name_argv_0 = argv[0];
    pping_client *ppc = NULL;
    char buf[1024];
    SSMMsg *sm_server = NULL;
    int determine_vmm_region = 0;
    bool sedna_server_is_running = false;
    int os_primitives_id_min_bound;
    try
    {
        if (uGetEnvironmentVariable(SEDNA_OS_PRIMITIVES_ID_MIN_BOUND, buf, 1024, NULL) != 0)
           os_primitives_id_min_bound = 1500; //default value for command line only
        else
           os_primitives_id_min_bound = atoi(buf);

        set_global_names(os_primitives_id_min_bound);

        INIT_TOTAL_TIME_VARS u_ftime(&t_total1);

        if (uGetEnvironmentVariable(SEDNA_DETERMINE_VMM_REGION, buf, 1024, NULL) != 0)
            determine_vmm_region = 0;
        else
            determine_vmm_region = atoi(buf);

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

            client = se_new command_line_client(argc, argv);
            if (!sedna_server_is_running) throw USER_EXCEPTION(SE4400);
        }

        if (uSocketInit(__sys_call_error) != 0)
            throw USER_EXCEPTION(SE3001);

        // FIXME: I think, it's possible to combine init and get_session_parameters into one functions (AF)
        //  u_ftime(&ttt1);
        client->init();

//  u_ftime(&ttt2);
//  cerr << "TEST!!!!!!!!!!!!!: " << to_string(ttt2 - ttt1).c_str() << endl;
        //get session parameters (from socket or from cmd line)
        client->get_session_parameters();

        //init global names
        set_global_names(client->get_os_primitives_id_min_bound());
        gov_shm_pointer = open_gov_shm(&gov_shm_dsc);
        is_init_gov_shm = true;
        socket_port = ((gov_config_struct *) gov_shm_pointer)->gov_vars.lstnr_port_number;
        SEDNA_DATA = ((gov_config_struct *) gov_shm_pointer)->gov_vars.SEDNA_DATA;

#ifdef SE_MEMORY_TRACK
        strcpy(MT_SEDNA_DATA, SEDNA_DATA);
#endif

        if (!check_database_existence(db_name)) //check database consistency (all files exists)
            throw USER_EXCEPTION2(SE4609, db_name);


        db_id = get_db_id_by_name((gov_config_struct*)gov_shm_pointer, db_name);

        if (db_id == -1)//there is no such database
           throw USER_EXCEPTION2(SE4200, db_name);

        set_global_names(client->get_os_primitives_id_min_bound(), db_id);

        //register session on governer
        register_session_on_gov();

        SednaUserException e = USER_EXCEPTION(SE4400);
        ppc = se_new pping_client(((gov_config_struct*)gov_shm_pointer)->gov_vars.ping_port_number, EL_TRN);
        ppc->startup(e);

        // sid is known
        event_logger_init(EL_TRN, db_name, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
        event_logger_set_sid(sid);
        client->write_user_query_to_log();

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

//u_ftime(&t_test3);
        // transaction initialization
//u_ftime(&t_test1);
        on_session_begin(sm_server);
//u_ftime(&t_test2);
//d_printf2("TEST1: %s\n", to_string(t_test2 - t_test1).c_str());
        elog(EL_LOG, ("Session is ready"));

#ifdef SE_ENABLE_SECURITY          //if security is on
        if (entry_point->is_first_transaction())
        {
            entry_point->clear_first_transaction_flag();
            auth = 0;
        }
#endif

        PPQueryEssence *qep_tree = NULL;        //qep of current stmnt
        StmntsArray *st = NULL;
        bool expect_another_transaction = true;

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
                    on_transaction_begin(sm_server);
                    client->respond_to_client(se_BeginTransactionOk);

                    qep_tree = NULL;    //qep of current stmnt
                    st = NULL;
                    bool has_next_item = false;

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
                                authentication();
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

                                on_user_statement_begin(client->get_query_type(), client->get_result_type(&client_msg), client->get_se_ostream(), client->get_query_string(&client_msg), qep_tree, st);


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
                                    client->begin_item();
                                    GET_TIME(&t1_exec);
                                    execute(qep_tree);
                                    GET_TIME(&t2_exec);

                                    client->end_of_item(false);

                                    on_user_statement_end(qep_tree, st);
                                }
                                else
                                {
                                    client->begin_item();
                                    GET_TIME(&t1_exec);
                                    has_next_item = next(qep_tree);
                                    GET_TIME(&t2_exec);

                                    client->end_of_item(has_next_item);
                                    if (!has_next_item)
                                    {
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
                                    if (has_next_item)
                                    {
                                        GET_TIME(&t1_exec);
                                        has_next_item = next(qep_tree);
                                        
                                        GET_TIME(&t2_exec);
                                    }
                                    client->end_of_item(has_next_item);
                                    u_ftime(&t_qep2);
                                    
                                    t_qep = (t_qep2 - (t_qep1 - t_qep));
                                    
                                    qep_time = to_string(t_qep);
                                    if (!has_next_item)
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
                                on_transaction_end(sm_server, true /*COMMIT*/);
                                ret_code = 0;

                                client->respond_to_client(se_CommitTransactionOk);
                                d_printf1("============== Trn execution finished ======================\n");
                                break;
                            }

                        case se_RollbackTransaction:   //rollback command
                            {
                                on_user_statement_end(qep_tree, st);
                                on_transaction_end(sm_server, false /*ROLLBACK*/);
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
                                on_transaction_end(sm_server, false /*ROLLBACK*/);
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
                    on_transaction_end(sm_server, false /*ROLLBACK*/);
                    ret_code = 1;

                    d_printf1("\nTr is rolled back successfully\n");

                    if (e.get_code() == SE3053 || e.get_code() == SE3006 || e.get_code() == SE3007 || e.get_code() == SE3009 || e.get_code() == SE3012)   //session must be closed
                    {
                        ret_code = 1;
                        throw;
                    }

                    fprintf(stderr, "%s\n", e.getMsg().c_str());
                    client->error(e.get_code(), e.getMsg());
                }
                catch(SednaException & e)
                {
                    sedna_soft_fault(e, EL_TRN);
                }
                catch(...)
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
        //qep_time = to_string(t_qep2 - t_qep1);

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
        ppc->shutdown();
        delete ppc;
        ppc = NULL;
        set_session_finished();
        event_logger_set_sid(-1);

        close_gov_shm(gov_shm_dsc, gov_shm_pointer);

        uSocketCleanup(__sys_call_error);

        d_printf1("Transaction has been closed\n\n");

    }
    catch(SednaUserException & e)
    {
        fprintf(stderr, "%s\n", e.getMsg().c_str());

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
        catch(...)
        {
            d_printf1("Connection with client has been broken\n");
        }
        event_logger_release();
        if (ppc) ppc->shutdown();
        set_session_finished();
        if (is_init_gov_shm)
            close_gov_shm(gov_shm_dsc, gov_shm_pointer);
        uSocketCleanup(__sys_call_error);
        ret_code = 1;
    }
    catch(SednaException & e)
    {
        sedna_soft_fault(e, EL_TRN);
    }
    catch(...)
    {
        sedna_soft_fault(EL_TRN);
    }

#ifdef SE_MEMORY_TRACK
    }
    DumpUnfreed(EL_TRN);
#endif

    return ret_code;
}
