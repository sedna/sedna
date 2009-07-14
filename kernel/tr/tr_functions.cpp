/*
* File:  tr_functions.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/
#include <sstream>

#include "common/sedna.h"
#include "common/base.h"
#include "common/u/uutils.h"
#include "common/ipc_ops.h"

#include "tr/tr_globals.h"
#include "tr/locks/locks.h"
#include "tr/auth/auc.h"
#include "tr/tr_functions.h"
#include "tr/pq/pq.h"
#include "tr/log/log.h"
#include "tr/crmutils/exec_output.h"
#include "tr/structures/metadata.h"
#include "tr/rcv/rcv_funcs.h"
#include "tr/updates/updates.h"
#include "tr/executor/xqops/PPConstructors.h"
#include "tr/executor/base/XPath.h"
#include "tr/executor/por2qep/por2qep.h"

#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers_data.h"
#endif

using namespace std;
using namespace tr_globals;

static bool is_qep_built  = false;
static bool is_qep_opened = false;
static bool is_stmt_built = false;
static msg_struct sp_msg;

void on_kernel_statement_begin(scheme_list *por,
                               se_ostream* s, 
                               t_print output_type,
                               PPQueryEssence* &qep_tree)
{
    // !!! Additional code review is needed
    // if qep_tree->open() below throws exception, than we will delete 'global' qep_tree 
    // in catch block instead of 'local' qep_tree declared in this function. The possible 
    // solution is to avoid using 'local' qep_tree and pass 'global' qep_tree here by reference
    // (example: create trigger for inexisting document)
    indirection_table_on_statement_begin();
    xs_decimal_t::init();
    qep_tree = build_qep(por, *s, output_type);
    is_qep_built = true;
    qep_tree->open();
    is_qep_opened = true;
}

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

        //tr_globals::st_ct.clear_context();

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
                             t_print output_type,
                             se_ostream* s,
                             const char* query_str,
                             PPQueryEssence* &qep_tree, 
                             StmntsArray* &st)
{
    elog_long(EL_LOG, "User query:\n", query_str);
    st = prepare_stmnt(queryType, query_str); //parse and build phys representation
    is_stmt_built = true;
    se_nullostream auth_s;

    internal_auth_switch = BLOCK_AUTH_CHECK; //turn off security checkings

    for (unsigned int i = 0; i < st->stmnts.size() - 1; i++)
    {
        on_kernel_statement_begin(st->stmnts[i].stmnt, &auth_s, xml, qep_tree);
        execute(qep_tree);
        on_kernel_statement_end(qep_tree);
    }

    if (st->stmnts.size() >= 3) clear_authmap(); // security metadata was updated - clear auth map

    internal_auth_switch = DEPLOY_AUTH_CHECK;                   // turn on security checkings

#ifdef SE_ENABLE_TRIGGERS
    triggers_on_statement_begin();
#endif

    on_kernel_statement_begin(st->stmnts.back().stmnt, s, output_type, qep_tree);
}

void on_user_statement_end(PPQueryEssence* &qep_tree, StmntsArray* &st)
{
#ifdef SE_ENABLE_TRIGGERS
    triggers_on_statement_end();
#endif

    on_kernel_statement_end(qep_tree);

    if (is_stmt_built)
    {
        delete_scheme_list(st->root);
        delete st;
        is_stmt_built = false;
    }

    qep_tree = NULL;
    st = NULL;

    clear_current_statement_authmap();
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
    if(!authentication || first_transaction) return;

    string security_metadata_document = string(SECURITY_METADATA_DOCUMENT);	
    string auth_query_in_por = "(query (query-prolog) (PPQueryRoot 2 (1 (PPIf (1 (PPCalculate (BinaryValEQ (LeafAtomOp 0) (LeafAtomOp 1)) (1 (PPAxisChild qname (\"\" \"user_psw\" \"\") (1 (PPReturn (0) (1 (PPAbsPath (document \"" + security_metadata_document + "\") (((PPAxisChild qname (\"\" \"db_security_data\" \"\"))) ((PPAxisChild qname (\"\" \"users\" \"\")))))) (1 (PPPred1 (1) (1 (PPAxisChild qname (\"\" \"user\" \"\") (1 (PPVariable 0)))) () (1 (PPCalculate (BinaryValEQ (LeafAtomOp 0) (LeafAtomOp 1)) (1 (PPAxisChild qname (\"\" \"user_name\" \"\") (1 (PPVariable 1)))) (1 (PPConst \"" + string(tr_globals::login) +  "\" !xs!string)))) 0)) -1)))) (1 (PPConst \"" + string(tr_globals::password) + "\" !xs!string)))) (1 (PPNil)) ((1 4) (PPFnError ((1 4) (PPFnQName (1 (PPConst \"http://www.modis.ispras.ru/sedna\" !xs!string)) (1 (PPConst \"SE3053\" !xs!string)))) (1 (PPConst \"Authentication failed.\" !xs!string))))))))";
    scheme_list *auth_query_in_scheme_lst = NULL;
    PPQueryEssence *qep_tree = NULL;
    se_nullostream s;

    auth_query_in_scheme_lst = make_tree_from_scheme_list(auth_query_in_por.c_str());

    try {
        internal_auth_switch = BLOCK_AUTH_CHECK;
        on_kernel_statement_begin(auth_query_in_scheme_lst, &s, xml, qep_tree);
        execute(qep_tree);
        on_kernel_statement_end(qep_tree);
        internal_auth_switch = DEPLOY_AUTH_CHECK;
    }
    catch (SednaUserException &e) {

        on_kernel_statement_end(qep_tree);
        delete_scheme_list(auth_query_in_scheme_lst);
        throw USER_EXCEPTION(SE3053);
    }

    delete_scheme_list(auth_query_in_scheme_lst);
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
