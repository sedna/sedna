
/*
* File:  tr_common_funcs.cpp
* Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include <sstream>
#include <string>

#include "common/sedna.h"
#include "common/base.h"
#include "common/ipc_ops.h"
#include "common/llcommon/llMain.h"

#include "tr/tr_globals.h"
#include "tr/tr_common_funcs.h"
#include "tr/auth/auc.h"
#include "tr/locks/locks.h"
#include "tr/log/log.h"
#include "tr/idx/index_types.h"
#include "tr/executor/base/XPath.h"
#include "tr/structures/metadata.h"
#include "tr/rcv/rcv_funcs.h"
#include "tr/cat/catalog.h"
#include "tr/triggers/triggers_data.h"
#include "tr/mo/boundaries.h"
#include "tr/crmutils/serialization.h"

#ifdef SE_ENABLE_FTSEARCH
#include "tr/ft/ft_cache.h"
#include "tr/ft/ft_index_data.h"
#endif

#ifdef SE_ENABLE_DTSEARCH
#include "tr/ft/FTindex.h"
#endif

using namespace std;
using namespace tr_globals;

static bool is_sm_server_inited = false;
static bool wait_sem_inited     = false;
static bool is_trid_obtained    = false;
static bool need_sem            = true; // need to use semaphore for updater


void wait_for_sm_to_unblock()
{
    int res;

    for (;;)
    {
        res = USemaphoreDownTimeout(tr_globals::wait_sem, 1000, __sys_call_error);

        if (res == 0) //unblocked
        {
            break;
        }
        else if (res == 2) // timeout
        {
            if (is_stop_session())
                throw USER_EXCEPTION(SE4608);
        }
        else // error
        {
            throw USER_EXCEPTION2(SE4015, "SEDNA_TRANSACTION_LOCK");
        }
    }
}

static transaction_id
get_transaction_id(SSMMsg* sm_server)
{
    sm_msg_struct msg;

    msg.trid = -1;

    for (;;)
    {
        msg.cmd = 1; // give me a trid
        msg.sid = tr_globals::sid;
        msg.data.data[0] = tr_globals::is_ro_mode;
        msg.data.data[1] = tr_globals::is_log_less_mode;

        if (sm_server->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE3034);

        U_ASSERT(msg.cmd == 0 || msg.cmd == 1);

        if (tr_globals::is_log_less_mode || msg.cmd == 1)
        {
            // wait for sm to unblock
            wait_for_sm_to_unblock();
        }

        // we've obtained a trid
        if (msg.cmd == 0) break;
    }

    if (msg.trid == -1)
        throw USER_EXCEPTION(SE4607);

    is_trid_obtained = true;
    d_printf2("get trid=%d\n", msg.trid);
    return msg.trid;
}

static void
release_transaction_id(SSMMsg* sm_server)
{
    if ( is_trid_obtained == true )
    {
        if (trid < 0 || trid >= CHARISMA_MAX_TRNS_NUMBER) return;

        d_printf2("return trid=%d\n", trid);
        sm_msg_struct msg;
        msg.cmd = 2;
        msg.trid = trid;
        msg.sid = tr_globals::sid;
        msg.data.data[0] = tr_globals::is_ro_mode;

        if (sm_server->send_msg(&msg) !=0 )
            throw USER_EXCEPTION(SE3034);
    }

    is_trid_obtained = false;
}

void on_session_begin(SSMMsg* &sm_server, int db_id, bool rcv_active)
{
    string log_files_path = string(SEDNA_DATA) + string("/data/") + string(db_name) + string("_files/");
    char buf[1024];

    d_printf1("Initializing waiting semaphore...");
    if (0 != USemaphoreCreate(&tr_globals::wait_sem, 0, 1,
            SEDNA_TRANSACTION_LOCK(tr_globals::sid, buf, 1024), NULL, __sys_call_error))
       throw USER_EXCEPTION2(SE4010, "SEDNA_TRANSACTION_LOCK");
    wait_sem_inited = true;
    d_printf1("OK\n");

    sm_server = se_new SSMMsg(SSMMsg::Client,
                              sizeof (sm_msg_struct),
                              CHARISMA_SSMMSG_SM_ID(db_id, buf, 1024),
                              SM_NUMBER_OF_SERVER_THREADS,
                              U_INFINITE);

    d_printf1("Connecting to SM...");
    if (sm_server->init() != 0)
        throw USER_EXCEPTION2(SE4200, db_name);
    is_sm_server_inited = true;
    d_printf1("OK\n");

    d_printf1("Initializing VMM...");
    vmm_on_session_begin(sm_server, rcv_active);
    d_printf1("OK\n");

    d_printf1("Initializing catalog...");
    catalog_on_session_begin();
    d_printf1("OK\n");

    d_printf1("Initializing metadata...");
    metadata_on_session_begin();
    d_printf1("OK\n");

    d_printf1("Initializing indexes...");
    index_on_session_begin();
#ifdef SE_ENABLE_FTSEARCH
    ft_index_on_session_begin();
#endif
    d_printf1("OK\n");

#ifdef SE_ENABLE_TRIGGERS
    d_printf1("Initializing triggers...");
    triggers_on_session_begin();
    d_printf1("OK\n");
#endif

    d_printf1("Initializing local lock manager...");
    init_local_lock_mgr(sm_server);
    d_printf1("OK\n");

    d_printf1("Initializing logical log...");
    hl_logical_log_on_session_begin(log_files_path, rcv_active);
    d_printf1("OK\n");

    is_ft_disabled = is_ro_mode;
}

void on_session_end(SSMMsg* &sm_server)
{
    d_printf1("Releasing logical log...");
    hl_logical_log_on_session_end();
    d_printf1("OK\n");

    d_printf1("Deleting local lock manager...");
    release_local_lock_mgr();
    d_printf1("OK\n");

#ifdef SE_ENABLE_TRIGGERS
    d_printf1("Releasing triggers...");
    triggers_on_session_end();
    d_printf1("OK\n");
#endif

    d_printf1("Releasing indexes...");
    index_on_session_end();
#ifdef SE_ENABLE_FTSEARCH
    ft_index_on_session_end();
#endif
    d_printf1("OK\n");

    d_printf1("Releasing metadata...");
    metadata_on_session_end();
    d_printf1("OK\n");

    d_printf1("Releasing catalog...");
    catalog_on_session_end();
    d_printf1("OK\n");

    d_printf1("Releasing VMM...");
    vmm_on_session_end();
    d_printf1("OK\n");

    if (tr_globals::serializer != NULL) {
        delete tr_globals::serializer;
        tr_globals::serializer = NULL;
    }

    d_printf1("Deleting SSMMsg...");
    if (is_sm_server_inited)
    {
        sm_server->shutdown();
        delete sm_server;
        sm_server = NULL;
        is_sm_server_inited = false;
    }
    d_printf1("OK\n");

    d_printf1("Releasing waiting semaphore...");
    if (wait_sem_inited)
    {
        if (0 != USemaphoreRelease(tr_globals::wait_sem, __sys_call_error))
            throw USER_EXCEPTION2(SE4011, "SEDNA_TRANSACTION_LOCK");
        wait_sem_inited = false;
    }
    d_printf1("OK\n");
}

void on_transaction_begin(SSMMsg* &sm_server, pping_client* ppc, bool rcv_active)
{
    TIMESTAMP ts;

    need_sem = !is_ro_mode;

    d_printf1("Getting transaction identifier...");
    trid = get_transaction_id(sm_server);
    d_printf1("OK\n");

    if (need_sem)
        down_transaction_block_sems();

    event_logger_set_trid(trid);

    d_printf1("Initializing VMM...");
    vmm_on_transaction_begin(is_ro_mode, ts);
    d_printf1("OK\n");

    d_printf1("Initializing catalog...");
    catalog_on_transaction_begin();
    d_printf1("OK\n");

    d_printf1("Initializing internal storage...");
    storage_on_transaction_begin();
    d_printf1("OK\n");

    d_printf1("Logical log on transaction begin...");
    hl_logical_log_on_transaction_begin(rcv_active, is_ro_mode);
    d_printf1("OK\n");

    d_printf1("Setting transaction mode for local lock manager...");
    set_tr_mode_lock_mgr(is_ro_mode);
    d_printf1("OK\n");

    is_ft_disabled = is_ro_mode;

#ifdef SE_ENABLE_TRIGGERS
    d_printf1("Triggers on transaction begin...");
    triggers_on_transaction_begin(rcv_active);
    d_printf1("OK\n");
#endif

    d_printf2("Reset transaction timeout (%d secs.)...", query_timeout);
    ppc->start_timer(query_timeout);
    d_printf1("OK\n");
}

// we need all this to be able to call 38 operation from logical log and from here
// sorry i couldn't find a more appropriate solution to this
static bool wu_reported = false; // are we already reported? needed for checkpoint-on-commit
static SSMMsg* sm_server_wu = NULL; // server to report to wu

static void rollbackTransaction()
{
    // disable logical logging
    hl_disable_log();

    // we should unmap all blocks here since sm will "fix" some offsets and xptrs on physical rollback
    // they might be not valid anymore
    // for now it is needed only for ft-indexes to see consistent state
    // note, that won't hamper our performance since the second call will just exit on empty unmap bitset
    // or will unmap just the blocks ft-indexes rollback have read
#ifdef SE_ENABLE_DTSEARCH
    vmm_unmap_all_blocks();
    /*
     * We need to rollback Ft-Indexes AFTER physical rollback since ft-rollback
     * will be reading before-data
     */
    SednaIndexJob::rollback();
#endif
    hl_logical_log_rollback(tr_globals::trid);

    //Here trid is a global variable inited before
    llOnTransEnd(tr_globals::trid);
}

void reportToWu(bool rcv_active, bool is_commit)
{
    sm_msg_struct msg;

    if (wu_reported)
        return;

    if (!rcv_active || (rcv_active && is_commit))
    {
#ifdef LOG_TRACE
        if (!is_commit)
            elog(EL_LOG, ("LOG_TRACE: Transaction starts rolling back: trid=%d", tr_globals::trid));
#endif
        msg.cmd = 38; // transaction commit/rollback
        msg.trid = trid;
        msg.sid = sid;
        msg.data.data[0] = (is_commit) ? 0 : 1;

        catalog_before_commit(is_commit);
        if (sm_server_wu->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);
        catalog_after_commit(is_commit);

        if (!is_commit)
        {
            rollbackTransaction();
        }
    }

    wu_reported = true;
}

// is_commit defines mode:
//  true - transaction commit
//  false - transaction rollback
void on_transaction_end(SSMMsg* &sm_server, bool is_commit, pping_client* ppc, bool rcv_active)
{
    sm_server_wu = sm_server;
#ifdef SE_ENABLE_FTSEARCH
    d_printf1("Flushing full-text cache...");
    ftc_flush(); //FIXME: remove this when cache is shared
    d_printf1("OK\n");
#endif

    ppc->stop_timer();
    clear_authmap();

#ifdef SE_ENABLE_TRIGGERS
    d_printf1("Triggers on transaction end...");
    triggers_on_transaction_end(is_commit);
    d_printf1("OK\n");
#endif

    try {
        d_printf1("\nReleasing logical log...");
        hl_logical_log_on_transaction_end(is_commit, rcv_active);
        d_printf1("OK\n");
    } catch (SednaUserException) {
        throw SYSTEM_EXCEPTION("Double error: user exception on rollback!");
    }

    d_printf1("Releasing storage...");
    if (!wu_reported) { storage_on_transaction_end(); }
    d_printf1("OK\n");

    d_printf1("\nNotifying sm of commit...");
    reportToWu(rcv_active, is_commit);
    wu_reported = false;

    d_printf1("Releasing catalog...");
    if (!wu_reported) { catalog_on_transaction_end(is_commit); }
    d_printf1("OK\n");

    d_printf1("Releasing VMM...");
    vmm_delete_tmp_blocks();
    vmm_on_transaction_end();
    d_printf1("OK\n");

    d_printf1("Releasing locks...");
    release_locks();
    d_printf1("OK\n");

    d_printf1("Releasing transaction_id...");
    release_transaction_id(sm_server);
    d_printf1("OK\n");

    event_logger_set_trid(-1);

    if (is_commit) {
        elog(EL_LOG, ("Transaction has been COMMITED"));
    } else {
        elog(EL_LOG, ("Transaction has been ROLLED BACK"));
    }

    if (need_sem)
        up_transaction_block_sems();
}

void on_kernel_recovery_statement_begin()
{
    sid = 0;
}

void on_kernel_recovery_statement_end()
{
    tr_globals::estr_global.clear();
    if (pe_local_aspace->free_all) pe_local_aspace->free_all();
    vmm_delete_tmp_blocks();
}


bool is_stop_session()
{
    if (NULL == sedna_gov_shm_ptr) return true;

    if (sid < 0 || sid >= MAX_SESSIONS_NUMBER) return true;

    return  (GOV_CONFIG_GLOBAL_PTR -> sess_vars[sid].stop == 1) ? true : false;
}

// switches transactions (from the next one) to ro-mode
// true -- ro-mode
// false -- update mode
void SwitchSessionToRO(bool flag)
{
    is_ro_mode = flag;
}

// switches log modes
// if SEDNA_LOG_LESS -- every bulkload will be logged with one record; drawback -- checkpoint at commit
// if SEDNA_LOG_FULL -- bulkload will be logged fully. checkpoint might be made though on truncate
void SwitchLogMode(int log_less_mode)
{
    is_log_less_mode = (log_less_mode == SEDNA_LOG_LESS);
}
