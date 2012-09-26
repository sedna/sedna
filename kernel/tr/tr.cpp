/*
* File:  tr.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "common/base.h"
#include "common/ssmmsg/SSMMsg.h"
#include "common/errdbg/errors.h"
#include "u/uprocess.h"
#include "common/globalobjects/globalnames.h"

#include "tr/vmm/vmm.h"


// #include <string>
// 
// #include "common/sedna.h"
// #include "common/base.h"
// #include "auxiliary/utils.h"
// #include "common/ssmmsg/SSMMsg.h"
// #include "common/errdbg/d_printf.h"
// #include "common/errdbg/exceptions.h"
// #include "tr/vmm/os_exceptions.h"

using namespace std;

class ClientRequestNotSupportedException : public SednaUserSoftException
{
public:
    ClientRequestNotSupportedException(const char* _file_, const char* _function_, int _line_, const char* _err_msg_) :
        SednaUserSoftException(_file_, _function_, _line_, _err_msg_) { };
};

// class TransactionProcessor
// {
//     client_core * client;
//     SSMMsg * smServer;
//     opt::IStatement * currentStatement;
// public:
//     int lastInstruction;
// 
//     TransactionProcessor(client_core * _client, SSMMsg * sm_server)
//         : client(_client), smServer(sm_server), currentStatement(NULL), lastInstruction(0)
//     {
//     };
// 
//     ~TransactionProcessor()
//     {
//         delete currentStatement;
//     };
// 
//     void run()
//     {
//         msg_struct client_msg;
//         bool transactionAlive = true;
//         uint64_t total_time = 0;
// 
//         on_transaction_begin(smServer, tr_globals::ppc);
//         client->respond_to_client(se_BeginTransactionOk);
// 
//         try {
//             do {
//                 client->read_msg(&client_msg);
//                 lastInstruction = client_msg.instruction;
// 
//                 switch (lastInstruction) {
//                 case se_Authenticate:
//                 {
//                     do_authentication();
//                     client->authentication_result(true, "");
//                 } break;
// 
//                 case se_ExecuteSchemeProgram:
//                 {
//                     /* Backward compatibility, not currently supported */
//                     throw ClientRequestNotSupportedException(EXCEPTION_PARAMETERS, "Scheme program execution is depricated");
//                     break;
//                 }
// 
//                 case se_ExecuteLong:
//                 case se_Execute:
//                 {
//                     if (currentStatement != NULL) {
//                         delete currentStatement;
//                         currentStatement = NULL;
//                     }
// 
//                     /* Adjust client for the new statement */
//                     client->set_result_type((enum se_output_method) (client_msg.body[0]));
//                     client->user_statement_begin();
// 
//                     currentStatement = new opt::OptimizedStatement(client);
// 
//                     currentStatement->prepare(
//                         client->get_query_type(),
//                         client->get_query_string(&client_msg));
// 
//                     currentStatement->execute();
// 
//                     /* If no exception, consider query succeded */
//                     if (currentStatement->isUpdate()) {
//                         client->respond_to_client(se_UpdateSucceeded);
//                         break;
//                     } else {
//                         client->respond_to_client(se_QuerySucceeded);
//                     };
// 
//                     /* Get the first item, no break */
//                 };
//                 case se_GetNextItem:
//                 {
//                     if (currentStatement == NULL) {
//                         U_ASSERT(false);
//                         // Statement not prepared;
//                     };
// 
//                     currentStatement->next();
// 
//                     if (currentStatement->isFinished()) {
//                         client->end_item(se_no_next_item);
//                     }
// 
//                     /* TODO: implement time */
// //                        total_time += currentStatement->time;
//                 } break;
//                 case se_ShowTime:      //show time
//                 {
//                     client->show_time_ex(total_time);
//                     break;
//                 }
//                 case se_CommitTransaction:
//                 case se_RollbackTransaction:
//                 case se_CloseConnection:
//                 {
//                     if (currentStatement != NULL) {
//                         delete currentStatement;
//                         currentStatement = NULL;
//                     }
// 
//                     transactionAlive = false;
// 
//                     try {
//                         on_transaction_end(
//                           smServer,
//                           client_msg.instruction == se_CommitTransaction,
//                           tr_globals::ppc);
//                     } catch (std::exception & exception) {
//                         throw SYSTEM_EXCEPTION(exception.what());
//                     };
// 
//                     switch (client_msg.instruction) {
//                       case se_CommitTransaction:
//                         client->respond_to_client(se_CommitTransactionOk); break;
//                       case se_RollbackTransaction:
//                         client->respond_to_client(se_RollbackTransactionOk); break;
//                       case se_CloseConnection:
//                         client->respond_to_client(se_TransactionRollbackBeforeClose); break;
//                       default :
//                         U_ASSERT(false);
//                     };
//                 } break;
// 
//                 default:
//                   client->process_unknown_instruction(client_msg.instruction, true);
//                 }
//             } while (transactionAlive);
//         } catch (SednaUserEnvException & userException) {
//             throw;
//         } catch (SednaUserException & userException) {
//             /* Any exception thrown from this block is considered system */
// 
//             try {
//                 on_transaction_end(smServer, false, tr_globals::ppc);
//             } catch (std::exception & exception) {
//                 throw SYSTEM_EXCEPTION(exception.what());
//             };
// 
//             if (userException.getCode() == SE3053) {
//                 client->authentication_result(false, userException.what());
//             } else {
//                 client->error(userException.getCode(), userException.what());
//             }
//         } catch (std::exception & exception) {
//             throw;
//         }
//     };
// };

int TRmain(int argc, char *argv[])
{
    /* volatile to prevent clobbing by setjmp/longjmp */
    volatile int ret_code = 0;
    init_base_path(argv[0]);

    SSMMsg *sm_server = NULL; /* shared memory messenger to communicate with SM */
    int determine_vmm_region = -1;
    
    try
    {
        char sednaDataDirectory[SEDNA_DATA_VAR_SIZE] = {};
        CHECK_ENV(uGetEnvironmentVariable(SEDNA_DATA_ENVIRONMENT, sednaDataDirectory, 
                                          SEDNA_DATA_VAR_SIZE, __sys_call_error),
                                          SE4074, SEDNA_DATA_ENVIRONMENT);
        SEDNA_DATA = sednaDataDirectory;
        
        /* Set global settings */
        GlobalObjectsCollector globalObjectsCollector;
        uSetGlobalNameGeneratorBase(SEDNA_DATA);

        /* Init event log */
        CHECK_ENV(event_logger_init(EL_TRN, "", eventLogShmName, eventLogSemName),
            SE0001, "Failed to start event log");

        /* This should release event log on exception */
        struct EventLogWarden {
            ~EventLogWarden() { event_logger_release(); };
        };

        char envVariableDbId[64] = {};
        CHECK_ENV(uGetEnvironmentVariable(SEDNA_DB_ID_ENVIRONMENT, envVariableDbId, 
                                          64, __sys_call_error),
                                          SE4074, SEDNA_DB_ID_ENVIRONMENT);
        
        uSetGlobalNameInstanceId(GN_DATABASE, envVariableDbId);
        
        if (argc == 2 && 0 == strcmp(argv[1], "--vmm-region")) {
            VirtualMemoryManager vmm(false /* not recovery mode */);
            vmm.determineRegion();
            elog(EL_INFO, ("Vmm region completed successfully"));
            return 0;
        } else if (argc == 2 && 0 == strcmp(argv[1], "--load-metadata")) {
            elog(EL_INFO, ("Load-metadata call succeded"));
        } else {
            /*temporary; to detect failures*/
            elog(EL_FATAL, ("Command line for trn was incorrect"));
            return -1;
        }
    }
    

//     try
//     {
//         OS_EXCEPTIONS_INSTALL_HANDLER

        /* Determine if we run via GOV or via command line */
//         bool server_mode = false;

//         event_logger_set_sid(sid);

//         sm_msg_struct client_msg;

        /* transaction initialization */
//         on_session_begin(sm_server, run_recovery);
//         elog(EL_LOG, ("Session is ready"));
// 
//         bool expect_another_transaction = !run_recovery;
// 
//         if (run_recovery)
//         {
//             on_transaction_begin(sm_server, tr_globals::ppc, true); // true means recovery is active
//             on_kernel_recovery_statement_begin();
// 
//             recover_db_by_logical_log();
// 
//             on_kernel_recovery_statement_end();
//             on_transaction_end(sm_server, true, tr_globals::ppc, true);
//         }
// 
//         on_session_end(sm_server);
// 
//         if (run_recovery)
//             elog(EL_LOG, ("recovery process by logical log finished"));
//         else
//             elog(EL_LOG, ("Session is closed"));
// 
//         event_logger_release();
// 
//         if (!run_recovery)
//             set_session_finished();
// 
//         event_logger_set_sid(-1);
// 
//         uSocketCleanup(__sys_call_error);
// 
//         d_printf1("Transaction has been closed\n\n");
// 
//     }
//     }
//     catch(SednaUserException & e)
//     {
//         fprintf(stderr, "%s\n", e.what());
// 
//         on_session_end(sm_server);
//         elog(EL_LOG, ("Session is closed"));
//         try
//         {
//             if (client != NULL)
//             {
//                 if (e.getCode() == SE3053)
//                     client->authentication_result(false, e.what());
//                 else
//                     client->error(e.getCode(), e.what());
// 
//                 client->release();
//                 delete client;
//             }
//         }
//         catch(ANY_SE_EXCEPTION)
//         {
//             d_printf1("Connection with client has been broken\n");
//         }
//         event_logger_release();
//         if (tr_globals::ppc)
//         {
//             tr_globals::ppc->shutdown();
//             delete tr_globals::ppc;
//             tr_globals::ppc = NULL;
//         }
//         set_session_finished();
//         close_gov_shm();
//         uSocketCleanup(__sys_call_error);
//         ret_code = 1;
//     }
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
