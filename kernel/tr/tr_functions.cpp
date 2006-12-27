/*
 * File:  tr_functions.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "locks.h"
#include "auc.h"
#ifdef SE_ENABLE_TRIGGERS
#include "triggers_data.h"
#endif
#include "base.h"
#include "tr_functions.h"
#include "pq.h"
#include "log.h"
#include "XPath.h"
#include "metadata.h"
#include "rcv_funcs.h"

using namespace std;

PPQueryEssence* on_kernel_statement_begin(scheme_list *por,
                                          se_ostream* s, 
                                          t_print output_type)
{
    // !!! Additional code review is needed
    indirection_table_on_statement_begin();
    xs_decimal_t::init();
    PPQueryEssence* qep_tree = build_qep(por, *s, output_type);
    is_qep_built = true;
    qep_tree->open();
    is_qep_opened = true;
    return qep_tree;
}

void on_kernel_statement_end(PPQueryEssence *qep_tree)
{
    if (is_qep_opened)
    {
       qep_tree->close();
       is_qep_opened = false;
    }

    if (is_qep_built)
    {
       delete_qep(qep_tree);

       //tr_globals::st_ct.clear_context();

	   tr_globals::estr_global.clear();

       PathExpr_local_free();
       PathExpr_reset_pers();

       vmm_delete_tmp_blocks();
       indirection_table_on_statement_end();
       
       is_qep_built = false;
    }
}


void on_user_statement_begin(QueryType query_type,
                             t_print output_type,
                             se_ostream* s,
                             const char* query_str,
                             PPQueryEssence* &qep_tree, 
                             StmntsArray* &st)
{
    elog_long(EL_LOG, "User query:\n", query_str);
    st = prepare_stmnt(query_type, query_str); //parse and build phys representation
    is_stmt_built = true;
    se_nullostream auth_s;

    if (AUTH_SWITCH && auth) auth = BLOCK_AUTH_CHECK; //turn off security checkings

    for (int i = 0; i < st->stmnts.size() - 1; i++)
    {
        qep_tree = on_kernel_statement_begin(st->stmnts[i].stmnt, &auth_s, xml);
     
        execute(qep_tree);

        on_kernel_statement_end(qep_tree);
    }

    if (st->stmnts.size() >= 3) clear_authmap(); // security metadata was updated - clear auth map
    if (AUTH_SWITCH && auth) auth = 1;                   // turn on security checkings

#ifdef SE_ENABLE_TRIGGERS
    triggers_on_statement_begin();
#endif

    qep_tree = on_kernel_statement_begin(st->stmnts.back().stmnt, s, output_type);
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
  if (!is_init_gov_shm) return;

  if (sid < 0 || sid >= MAX_SESSIONS_NUMBER) return;

  try{

      ((gov_sess_struct*)((char*)gov_shared_mem + sizeof(gov_header_struct) + MAX_DBS_NUMBER*sizeof(gov_dbs_struct) + sid*sizeof(gov_sess_struct)))->idfree = 2;

  } catch(...){}
}



void execute(PPQueryEssence* qep_tree)
{
    OS_exceptions_handler::enter_stack_overflow_critical_section();

    try {
        qep_tree->execute();
    } catch (SednaUserException &e) {
        OS_exceptions_handler::leave_stack_overflow_critical_section();
        throw;
    }

    OS_exceptions_handler::leave_stack_overflow_critical_section();
}

bool next(PPQueryEssence* qep_tree)
{
    bool res;
    OS_exceptions_handler::enter_stack_overflow_critical_section();

    try {
        res = ((PPQueryRoot*)qep_tree)->next();
    } catch (SednaUserException &e) {
        OS_exceptions_handler::leave_stack_overflow_critical_section();
        throw;
    }

    OS_exceptions_handler::leave_stack_overflow_critical_section();

    return res;
}

bool is_command_line_args_length_overflow(int argc, char ** argv)
{
   for (int i = 1; i < argc; i++)
       if (strlen(argv[i]) > s_min(SE_MAX_DB_NAME_LENGTH, s_min(SE_MAX_PASSWORD_LENGTH, s_min(TR_ARG_MAX_LENGTH, SE_MAX_LOGIN_LENGTH)))) return true;
   return false;
}

void print_tr_usage()
{
   throw USER_SOFT_EXCEPTION((string("Usage: se_trn [options] dbname filename\n\n") +
                              string("options:\n") + string(arg_glossary(tr_argtable, narg, "  ")) + string("\n")).c_str());
}

void authentication()
{
   string security_metadata_document = string(SECURITY_METADATA_DOCUMENT);	
   string auth_query_in_por = "(query (query-prolog) (PPQueryRoot 1 (1 (PPIf (1 (PPFnNot (1 (PPFnEmpty (1 (PPDDO (1 (PPReturn (0)  (1 (PPAbsPath (document \""+ security_metadata_document +"\") (((PPAxisChild qname (\"\" \"db_security_data\"))) ((PPAxisChild qname (\"\" \"users\"))) ((PPAxisChild qname (\"\" \"user\")))))) (1 (PPIf (1 (PPCalculate (BinaryOpAnd (LeafEffectBoolOp 0) (LeafEffectBoolOp 1)) (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"user_name\") (1 (PPVariable 0)))))) (1 (PPConst \"" +string(login) + "\" !xs!string)))) (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"user_psw\") (1 (PPVariable 0)))))) (1 (PPConst \"" + string(password) +  "\" !xs!string)))))) (1 (PPVariable 0)) (1 (PPNil)))) -1)))))))) (1 (PPNil)) (1 (PPFnError (1 (PPFnQName (1 (PPConst \"http://www.modis.ispras.ru/sedna\" !xs!string)) (1 (PPConst \"SE3053\" !xs!string)))) (1 (PPConst \"Authentication failed.\" !xs!string))))))))";
   scheme_list *auth_query_in_scheme_lst = NULL;
   PPQueryEssence *qep_tree = NULL;
   se_nullostream s;

   auth_query_in_scheme_lst = make_tree_from_scheme_list(auth_query_in_por.c_str());

   try {
   	   if (auth) // if security is on
   	   {
   	   	   auth = BLOCK_AUTH_CHECK;

           qep_tree = on_kernel_statement_begin(auth_query_in_scheme_lst, &s, xml);
   	   	   execute(qep_tree);
           on_kernel_statement_end(qep_tree);

   	   	   auth = 1;
       }
   }
   catch (SednaUserException &e) {
       //d_printf2("Auth ERROR!!!=%s\n", e.getMsg().c_str());
       on_kernel_statement_end(qep_tree);
       delete_scheme_list(auth_query_in_scheme_lst);
       throw USER_EXCEPTION(SE3053);
   }

   delete_scheme_list(auth_query_in_scheme_lst);
}

void register_session_on_gov()
{
	USOCKET s;
	int sock_error, res;
	UPID s_pid;
//	msg_struct msg;

	s_pid = uGetCurrentProcessId(__sys_call_error);
	
    s = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);
    if(s == U_SOCKET_ERROR) throw USER_EXCEPTION (SE3001);

	res = 1;
	while(uconnect_tcp(s, socket_port, "127.0.0.1", __sys_call_error)!=0)
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

    sp_msg.instruction = 121; 
    sp_msg.length = strlen(db_name)+1+sizeof(__int32)+sizeof(UPID); //dbname as a string and session process id as 4 bytes
    sp_msg.body[0] = '\0';
    int2net_int(strlen(db_name), sp_msg.body+1);    
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
bool check_database_existence(const char* db_name)
{
   bool res1 = false, res2 = false, res3 = false, res4 = false, res5 = false, res6 = false;

   if (!set_sedna_data(NULL))
      throw USER_EXCEPTION(SE4411);

   res1 = uIsFileExist((string(SEDNA_DATA) + "/cfg/" + string(db_name) + "_cfg.xml").c_str(), __sys_call_error);

   res2 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".data").c_str(), __sys_call_error);

   res3 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".tmp").c_str(), __sys_call_error);

   res4 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".plog").c_str(), __sys_call_error);

   res5 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".ph").c_str(), __sys_call_error);

   res6 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".ph.bu").c_str(), __sys_call_error);


   if (res1 && res2 && res3  && res4 && res5 && res6) return true;
   else return false;
}	
