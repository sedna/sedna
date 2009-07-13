/*
* File:  rcv_db.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include <string>

#include "common/sedna.h"
#include "common/base.h"
#include "common/SSMMsg.h"
#include "common/errdbg/d_printf.h"
#include "common/u/ushm.h"
#include "common/config.h"
#include "common/u/usem.h"
#include "common/pping.h"
#include "common/ipc_ops.h"

#include "tr/rcv/rcv_funcs.h"
#include "tr/tr_utils.h"
#include "tr/tr_globals.h"
#include "tr/tr_common_funcs.h"
#include "tr/vmm/vmm.h"

#ifndef _WIN32
#define _atoi64 atoll
#endif

using namespace std;
using namespace tr_globals;

SSMMsg *sm_server = NULL;

DECLARE_TIME_VARS

extern "C"
int TRmain (int argc, char** argv)
{
    if (argc != 3)
    {
        d_printf1("bad number of parameters\n");
        return -1;
    }

    pping_client *ppc = NULL;
    char buf[ENV_BUF_SIZE + 1];
    SednaUserException e = USER_EXCEPTION(SE4400);
    USemaphore signal_end;

    memset(buf, 0, ENV_BUF_SIZE + 1);

    try{
        if (uGetEnvironmentVariable(SEDNA_OS_PRIMITIVES_ID_MIN_BOUND, buf, ENV_BUF_SIZE, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4073, SEDNA_OS_PRIMITIVES_ID_MIN_BOUND);

        InitGlobalNames(atoi(buf), INT_MAX);
        SetGlobalNames();

        vmm_preliminary_call();

        OS_EXCEPTIONS_INSTALL_HANDLER;

        strcpy(db_name, argv[1]);

        if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION(SE3001);

        open_gov_shm();

        //      DebugBreak();
        ppc = se_new pping_client(GOV_HEADER_GLOBAL_PTR -> ping_port_number, EL_RCV);
        ppc->startup(e);

        int db_id = get_db_id_by_name(GOV_CONFIG_GLOBAL_PTR, db_name);

        /* There is no such database? */
        if (db_id == -1)
            throw USER_EXCEPTION2(SE4200, db_name);

        SEDNA_DATA = GOV_HEADER_GLOBAL_PTR->SEDNA_DATA;

        SetGlobalNamesDB(db_id);

        sid=0;

        event_logger_init(EL_RCV, db_name, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
        event_logger_set_sid(sid);
        elog(EL_LOG, ("recovery process by logical log started"));

        on_session_begin(sm_server, db_id, true);
        on_transaction_begin(sm_server, ppc, true);//true means recovery active
        on_kernel_recovery_statement_begin();

        LSN last_cp_lsn = _atoi64(argv[2]);
        //      std::cout << "last checkpoint lsn=" << last_cp_lsn << endl;

        //      DebugBreak();
        /*
        if (AllocConsole())
        {
        freopen("CON","wt",stderr);
        freopen("CON","wt",stdout);
        }
        */
        recover_db_by_logical_log(last_cp_lsn);

        on_kernel_recovery_statement_end();
        on_transaction_end(sm_server, true, ppc, true);
        on_session_end(sm_server);

        if (0 != USemaphoreOpen(&signal_end, CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG, __sys_call_error))
            throw USER_EXCEPTION2(SE4012, "CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG");

        if (0 != USemaphoreUp(signal_end, __sys_call_error))
            throw USER_EXCEPTION2(SE4014, "CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG");

        if (0 != USemaphoreClose(signal_end, __sys_call_error))
            throw USER_EXCEPTION2(SE4013, "CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG");

        elog(EL_LOG, ("recovery process by logical log finished"));
        event_logger_release();
        event_logger_set_sid(-1);

        ppc->shutdown();
        delete ppc;
        ppc = NULL;

        close_gov_shm();

        uSocketCleanup(__sys_call_error);

    } catch(SednaUserException &e) {
        event_logger_release();
        event_logger_set_sid(-1);
        if (ppc) { ppc->shutdown(); delete ppc; ppc = NULL; }
        close_gov_shm();
        uSocketCleanup(NULL);
        fprintf(stderr, "%s\n", e.what());
    } catch(SednaException & e) {
        sedna_soft_fault(e, EL_RCV);
    } catch(ANY_SE_EXCEPTION) {
        sedna_soft_fault(EL_RCV);
    }

    return 0;
}
