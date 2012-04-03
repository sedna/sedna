/*
 * File:  tr_functions.cpp
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */
#include <sstream>

#include "common/sedna.h"
#include "common/base.h"
#include "common/u/uutils.h"
#include "common/ipc_ops.h"
#include "common/sp.h"

#include "tr/tr_globals.h"
#include "tr/locks/locks.h"
#include "tr/auth/auc.h"
#include "tr/tr_functions.h"
#include "tr/log/log.h"
#include "tr/structures/metadata.h"
#include "tr/rcv/rcv_funcs.h"
#include "tr/updates/updates.h"
#include "tr/executor/xqops/PPConstructors.h"
#include "tr/executor/base/XPath.h"
#include "tr/xqp/XQueryDriver.h"
#include "tr/xqp/XQuerytoLR.h"
#include "tr/executor/root/PPQueryRoot.h"
#include "tr/nid/numb_scheme.h"
#include "tr/crmutils/debug_utils.h"
#include "tr/crmutils/serialization.h"
#include <common/socketutils/socketutils.h>

#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers_data.h"
#endif

using namespace std;
using namespace tr_globals;

static bool is_qep_built  = false;
static bool is_qep_opened = false;
static msg_struct sp_msg;

static sedna::XQueryDriver *xqd = NULL;

static void
on_kernel_statement_begin(size_t mod_index,
                          PPQueryEssence* &qep_tree)
{
    xs_decimal_t::init();

    try
    {
        is_qep_built = true; // always consider qep-tree as built; in case of error this allows cleaning up
        qep_tree = xqd->getQEPForModule(mod_index, false);
    }
    catch (SednaUserException)
    {
        delete xqd;
        xqd = NULL;
        qep_tree = NULL;
        throw;
    }

    qep_tree->open();
    is_qep_opened = true;
}

static void
on_kernel_statement_end(PPQueryEssence *&qep_tree)
{
    if (is_qep_opened)
    {
        qep_tree->close();
        is_qep_opened = false;
    }

    if (is_qep_built)
    {
        delete qep_tree;
        qep_tree = NULL;

        //  TODO: We should carefully clear virtual root here. To review it later.
        PPConstructor::clear_virtual_root();

        stmt_str_buf::reset();
        tr_globals::estr_global.clear();
        executor_globals::on_kernel_statement_end();

        system_tables_on_kernel_statement_end();

        /* TMPNIDBLK must be nulled when the last temp node is deleted. */
        nid_on_kernel_statement_end();

        is_qep_built = false;
    }
}

void on_user_statement_begin(QueryType queryType,
                             const char* query_str,
                             PPQueryEssence* &qep_tree,
                             StmntsArray* &st)
{
    elog_long(EL_LOG, "User query:\n", query_str);

    // delete driver for previous query, if any
    if (xqd)
    {
        delete xqd;
        xqd = NULL;
    }

    xqd = new sedna::XQueryDriver;

    // parse and do logical analysis (state is stored in the driver)
    parse_batch(xqd, queryType, query_str, NULL);

    // we don't like >1 modules
    if (xqd->getModulesCount() > 1)
       throw USER_EXCEPTION(SE4003);

    if (xqd->getModulesCount() >= 3) clear_authmap(); // security metadata was updated - clear auth map

#ifdef SE_ENABLE_TRIGGERS
    triggers_on_statement_begin();
#endif

    on_kernel_statement_begin(xqd->getModulesCount() - 1, qep_tree);
}

void on_user_statement_end(PPQueryEssence* &qep_tree, StmntsArray* &st)
{
#ifdef SE_ENABLE_TRIGGERS
    triggers_on_statement_end();
#endif

    on_kernel_statement_end(qep_tree);

    qep_tree = NULL;
    st = NULL;

    clear_current_statement_authmap();

    delete xqd;
    xqd = NULL;
}

void set_session_finished()
{
    d_printf2("sid=%d\n", sid);
    if (NULL == sedna_gov_shm_ptr) return;

    if (sid < 0 || sid >= MAX_SESSIONS_NUMBER) return;

    GOV_CONFIG_GLOBAL_PTR -> sess_vars[sid].idfree = 2;
}

qepNextAnswer execute(PPQueryEssence* qep_tree)
{
    try {
        qep_tree->execute();
    } catch (SednaUserException &e) {
        if (1 == tr_globals::debug_mode) 
            print_pp_stack(tr_globals::client->get_debug_ostream());
        if (e.get_code() == SE2041) return se_result_is_cut_off;
        throw;
    }

    return se_no_next_item;
}

qepNextAnswer next(PPQueryEssence* qep_tree)
{
    bool res;

    try {
        res = ((PPQueryRoot*)qep_tree)->next();
    } catch (SednaUserException &e) {
        if (1 == tr_globals::debug_mode) 
            print_pp_stack(tr_globals::client->get_debug_ostream());
        if (e.get_code() == SE2041) return se_result_is_cut_off;
        throw;
    }

    return (res) ? se_next_item_exists : se_no_next_item;
}

void do_authentication()
{
    if (!authentication || first_transaction) return;

    string query_str = "\
            if (doc('%db_sec_doc%')/db_security_data/users/user[user_name = '%user%' and user_psw = '%pswd%'])\
            then\
                fn:true()\
            else\
                fn:error(fn:QName('http://www.modis.ispras.ru/sedna', 'SE3053'), 'Authentication failed')";

    PPQueryEssence *qep_tree = NULL;

    bool output_enabled = false;

    query_str.replace(query_str.find("%db_sec_doc%", 0), strlen("%db_sec_doc%"), SECURITY_METADATA_DOCUMENT);
    query_str.replace(query_str.find("%user%", 0), strlen("%user%"), login);
    query_str.replace(query_str.find("%pswd%", 0), strlen("%pswd%"), password);

    try
    {
        xqd = new sedna::XQueryDriver;

        // parse and do logical analysis (state is stored in the driver)
        parse_batch(xqd, TL_XQuery, query_str.c_str(), NULL);

        internal_auth_switch = BLOCK_AUTH_CHECK;
        output_enabled = client->disable_output();
        on_kernel_statement_begin(xqd->getModulesCount() - 1, qep_tree);
        execute(qep_tree);
        on_kernel_statement_end(qep_tree);
        if(output_enabled) client->enable_output();
        internal_auth_switch = DEPLOY_AUTH_CHECK;
    }
    catch (SednaUserException)
    {
        on_kernel_statement_end(qep_tree);
        if (output_enabled) client->enable_output();
        delete xqd;
        xqd = NULL;
        throw USER_EXCEPTION(SE3053);
    }

    delete xqd;
    xqd = NULL;
}


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
#else /* !_WIN32 */
#include <signal.h>
void TrnCtrlHandler(int signo)
{
    if (signo == SIGINT || signo == SIGQUIT || signo == SIGTERM)
    {
        //beep();
    }
}
#endif /* _WIN32 */

void set_trn_ctrl_handler() 
{
#ifdef _WIN32
            BOOL fSuccess;
            fSuccess = SetConsoleCtrlHandler((PHANDLER_ROUTINE) TrnCtrlHandler, TRUE);
            if (!fSuccess)
                throw USER_EXCEPTION(SE4207);
#else /* !_WIN32 */
            // For Control-C or Delete
            if (signal(SIGINT, TrnCtrlHandler) == SIG_ERR)
                throw USER_EXCEPTION(SE4207);
            // For Control-backslash
            if (signal(SIGQUIT, TrnCtrlHandler) == SIG_ERR)
                throw USER_EXCEPTION(SE4207);
            //For reboot or halt
            if (signal(SIGTERM, TrnCtrlHandler) == SIG_ERR)
                throw USER_EXCEPTION(SE4207);
#endif /* _WIN32 */
}
