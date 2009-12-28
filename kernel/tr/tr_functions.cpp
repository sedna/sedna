/*
* File:  tr_functions.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
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

#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers_data.h"
#endif

using namespace std;
using namespace tr_globals;

static bool is_qep_built  = false;
static bool is_qep_opened = false;
static bool is_stmt_built = false;
static msg_struct sp_msg;

static sedna::XQueryDriver *xqd = NULL;

static void
on_kernel_statement_begin(size_t mod_index,
                          PPQueryEssence* &qep_tree)
{
    indirection_table_on_statement_begin();
    xs_decimal_t::init();
    qep_tree = xqd->getQEPForModule(mod_index);
    is_qep_built = true;
    qep_tree->open();
    is_qep_opened = true;
}

static
void on_kernel_statement_end(PPQueryEssence *&qep_tree)
{
    RESET_CURRENT_PP;

    if (is_qep_opened)
    {
        qep_tree->close();
        is_qep_opened = false;
    }

    if (is_qep_built)
    {
        delete_qep(qep_tree);
        qep_tree = NULL;

        //  TODO: We should carefully clear virtual root here. To review it later.
        clear_virtual_root();

        tr_globals::estr_global.clear();
        stmt_str_buf::reset();
        tr_globals::tmp_op_str_buf.reset();

        if (pe_local_aspace->free_all) pe_local_aspace->free_all();

        vmm_delete_tmp_blocks();
        system_tables_on_kernel_statement_end();
        indirection_table_on_statement_end();

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
    is_stmt_built = true;

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

    if (is_stmt_built)
    {
        //delete_scheme_list(st->root);
        //delete st;
        is_stmt_built = false;
    }

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
    OS_exceptions_handler::enter_stack_overflow_critical_section();

    try {
        qep_tree->execute();
    } catch (SednaUserException &e) {
        OS_exceptions_handler::leave_stack_overflow_critical_section();
        if (e.get_code() == SE2041) return se_result_is_cut_off;
        throw;
    }

    OS_exceptions_handler::leave_stack_overflow_critical_section();
    return se_no_next_item;
}

qepNextAnswer next(PPQueryEssence* qep_tree)
{
    bool res;
    OS_exceptions_handler::enter_stack_overflow_critical_section();

    try {
        res = ((PPQueryRoot*)qep_tree)->next();
    } catch (SednaUserException &e) {
        OS_exceptions_handler::leave_stack_overflow_critical_section();
        if (e.get_code() == SE2041) return se_result_is_cut_off;
        throw;
    }

    OS_exceptions_handler::leave_stack_overflow_critical_section();

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
    catch (SednaUserException &e)
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

void register_session_on_gov()
{
    UPID s_pid  = uGetCurrentProcessId(__sys_call_error);
    USOCKET s   = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);

    if(U_SOCKET_ERROR == s) throw USER_EXCEPTION (SE3001);

    while(0 != uconnect_tcp(s, socket_port, "127.0.0.1", __sys_call_error))
    {
        if(!utry_connect_again())
        {
            ushutdown_close_socket(s, __sys_call_error);
            throw USER_EXCEPTION (SE3003);
        }
#ifdef _WIN32
#else
        if(ushutdown_close_socket(s, __sys_call_error)!=0) throw USER_EXCEPTION (SE3011);
        s = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);
        if(s == U_SOCKET_ERROR) throw USER_EXCEPTION (SE3001);
#endif
    }

    sp_msg.instruction  = REGISTER_NEW_SESSION;
    sp_msg.length       = strlen(db_name)+1+sizeof(__int32)+sizeof(UPID); //dbname as a string and session process id as 4 bytes
    sp_msg.body[0]      = '\0';

    int2net_int(strlen(db_name), sp_msg.body + 1);
    memcpy(sp_msg.body+1+sizeof(__int32), db_name, strlen(db_name));

    __int32 tmp = s_pid;
    char *ptr = (char*) &(tmp);

    memcpy(sp_msg.body+1+sizeof(__int32)+strlen(db_name),ptr,sizeof(UPID));


    if(sp_send_msg(s,&sp_msg)!=0) throw USER_EXCEPTION2(SE3006,usocket_error_translator());
    if(sp_recv_msg(s,&sp_msg)!=0) throw USER_EXCEPTION2(SE3007,usocket_error_translator());

    if(sp_msg.instruction == 161)
    {
        d_printf2("se_trn: Trn with %d registered on gov successfully\n", s_pid);
        memmove(ptr, sp_msg.body, 4);
        sid = (*(__int32*)ptr);                            //sid (session identificator) is a global parameter
        d_printf2("session sid=%d\n", sid);
        if (sid >= MAX_SESSIONS_NUMBER) throw SYSTEM_EXCEPTION("Got incorrect session id from GOV");

        if (sid == -1)
        {
            ushutdown_close_socket(s, __sys_call_error);
            throw USER_EXCEPTION(SE4607);
        }
    }
    if(sp_msg.instruction == 171)
    {
        ushutdown_close_socket(s, __sys_call_error);
        throw USER_EXCEPTION2(SE4409,db_name);                   //no db started
    }
    if(sp_msg.instruction == 172)
    {
        ushutdown_close_socket(s, __sys_call_error);
        throw USER_EXCEPTION(SE3046);                            //currently there are maximum number of session in the system
    }
    if(sp_msg.instruction == 173)
    {
        ushutdown_close_socket(s, __sys_call_error);
        throw USER_EXCEPTION(SE3043);                            //failed to register
    }

    if(ushutdown_close_socket(s, __sys_call_error)!=0) throw USER_EXCEPTION (SE3011);
}


//returns true if all database files exists
bool check_database_existence(const char* name)
{
    bool res1 = false, res2 = false, res3 = false;

    res1 = uIsFileExist((string(SEDNA_DATA) + "/cfg/" + string(name) + "_cfg.xml").c_str(), __sys_call_error);

    res2 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(name) + "_files/" + string(name) + ".sedata").c_str(), __sys_call_error);

    res3 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(name) + "_files/" + string(name) + ".setmp").c_str(), __sys_call_error);

    if (res1 && res2 && res3) return true;
    else return false;
}
