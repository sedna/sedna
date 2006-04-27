/*
 * File:  tr_functions.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "locks.h"
#include "auc.h"
#include "base.h"
#include "tr_functions.h"
#include "pq.h"
#include "log.h"
#include "XPath.h"
#include "metadata.h"
#include "rcv_funcs.h"

using namespace std;
static bool is_sm_server_inited = false;
static bool is_ph_inited = false;

static bool is_trid_obtained = false;
static bool is_qep_built = false;
static bool is_qep_opened = false;
static bool is_stmt_built = false;



void on_session_begin(SSMMsg* &sm_server)
{
   string log_files_path = string(SEDNA_DATA) + string("/data/") + string(db_name) + string("_files/");
   char buf[1024];
   sm_msg_struct msg;

   sm_server = new SSMMsg(SSMMsg::Client, 
                          sizeof (sm_msg_struct), 
                          CHARISMA_SSMMSG_SM_ID(db_name, buf, 1024), 
                          SM_NUMBER_OF_SERVER_THREADS, 
                          U_INFINITE);

   d_printf1("Connecting to SM...");
   if (sm_server->init() != 0)
      throw USER_EXCEPTION2(SE4200, db_name);
   is_sm_server_inited = true;
   d_printf1("OK\n");

   d_printf1("Initializing PH...");
   string ph_path = string(SEDNA_DATA) + "/data/" + db_name + "_files/" + db_name +".ph";
   if (0 != pers_init(ph_path.c_str(), CHARISMA_PH_SHARED_MEMORY_NAME, PERS_HEAP_SEMAPHORE_STR, PH_ADDRESS_SPACE_START_ADDR, 1))
      throw USER_EXCEPTION(SE4605);
   is_ph_inited = true;
   d_printf1("OK\n");

   d_printf1("Initializing VMM...");
   entry_point = vmm_on_session_begin(sm_server);
   d_printf1("OK\n");

   d_printf1("Initializing indirection table...");
   indirection_table_on_session_begin();
   d_printf1("OK\n");

   d_printf1("Initializing metadata...");
   metadata_on_session_begin(entry_point->metadata);
   d_printf1("OK\n");
 
   d_printf1("Initializing indexes...");
   index_on_session_begin(entry_point->index, &(entry_point->idx_counter));
   #ifdef SE_ENABLE_FTSEARCH
   ft_index_on_session_begin(entry_point->ft_index, &(entry_point->ft_idx_counter));
   #endif
   d_printf1("OK\n");

   d_printf1("Initializing local lock manager...");
   init_local_lock_mgr(sm_server);
   d_printf1("OK\n");

   d_printf1("Initializing high level physical log...");
   hl_phys_log_on_session_begin(log_files_path + string(db_name) + string(".plog"));
   d_printf1("OK\n");

   d_printf1("Initializing logical log...");
   hl_logical_log_on_session_begin(log_files_path);
   d_printf1("OK\n");
}

void on_session_end(SSMMsg* &sm_server)
{
   d_printf1("Releasing high level physical log...");
   hl_phys_log_on_session_end();
   d_printf1("OK\n");

   d_printf1("Releasing logical log...");
   hl_logical_log_on_session_end();
   d_printf1("OK\n");

   d_printf1("Deleting local lock manager...");
   release_local_lock_mgr();
   d_printf1("OK\n");
 
   d_printf1("Releasing indexes...");
   index_on_session_end();
   #ifdef SE_ENABLE_FTSEARCH
   ft_index_on_session_end();
   #endif
   d_printf1("OK\n");

   d_printf1("Releasing metadata...");
   metadata_on_session_end();
   d_printf1("OK\n");

   d_printf1("Releasing indirection table...");
   indirection_table_on_session_end();
   d_printf1("OK\n");

   d_printf1("Releasing VMM...");
   vmm_on_session_end();
   d_printf1("OK\n");

   d_printf1("Releasing PH... ");
   if (is_ph_inited)
   {
      if (pers_release() != 0)
         throw USER_EXCEPTION(SE4606);

      is_ph_inited = false;
   }
   d_printf1("OK\n");


   d_printf1("Deleting SSMMsg...");
   if (is_sm_server_inited)
   {
      sm_server->shutdown();
      delete sm_server;
      sm_server = NULL;
      is_sm_server_inited = false;
   }
   d_printf1("OK\n");
}

void on_transaction_begin(SSMMsg* &sm_server)
{
   d_printf1("Getting transaction identifier...");
   trid = get_transaction_id(sm_server);
   d_printf1("OK\n");

   event_logger_set_trid(trid);

   d_printf1("Phys log on transaction begin...");
   hl_phys_log_on_transaction_begin();
   d_printf1("OK\n");

   d_printf1("Initializing VMM...");
   vmm_on_transaction_begin();
   d_printf1("OK\n");

   d_printf1("Initializing indirection table...");
   indirection_table_on_transaction_begin();
   d_printf1("OK\n");

   d_printf1("Logical log on transaction begin...");
   hl_logical_log_on_transaction_begin();
   d_printf1("OK\n");

}

// is_commit defines mode: 
//  true - transaction commit
//  false - transaction rollback
void on_transaction_end(SSMMsg* &sm_server, bool is_commit)
{
   clear_authmap();

   d_printf1("\nReleasing logical log...");
   hl_logical_log_on_transaction_end(is_commit);
   d_printf1("OK\n");

   sync_indirection_table();
   release_locks();
 
   d_printf1("Releasing indirection table...");
   indirection_table_on_transaction_end();
   d_printf1("OK\n");

   d_printf1("Releasing VMM...");
   vmm_on_transaction_end();
   d_printf1("OK\n");


   d_printf1("Releasing high level physical log...");
   hl_phys_log_on_transaction_end();
   d_printf1("OK\n");

   d_printf1("Releasing transaction_id...");
   release_transaction_id(sm_server);
   d_printf1("OK\n");

   event_logger_set_trid(-1);
}

PPQueryEssence* on_kernel_statement_begin(scheme_list *por,
                                          se_ostream* s, 
                                          t_print output_type)
{
    // !!! Additional code review is needed
    indirection_table_on_statement_begin();
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

       tr_globals::st_ct.clear_context();

       e_string_first_blk = XNULL;
       e_string_last_blk = XNULL;

	   tr_globals::e_str_global.clear();

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

    if (AUTH_SWITCH) auth = BLOCK_AUTH_CHECK; //turn off security checkings

    for (int i = 0; i < st->stmnts.size() - 1; i++)
    {
        qep_tree = on_kernel_statement_begin(st->stmnts[i].stmnt, &auth_s, xml);
     
        execute(qep_tree);

        on_kernel_statement_end(qep_tree);
    }

    if (st->stmnts.size() >= 3) clear_authmap(); // security metadata was updated - clear auth map
    if (AUTH_SWITCH) auth = 1;                   // turn on security checkings

    qep_tree = on_kernel_statement_begin(st->stmnts.back().stmnt, s, output_type);
}

void on_user_statement_end(PPQueryEssence* &qep_tree, StmntsArray* &st)
{
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



bool is_stop_session()
{
  if (!is_init_gov_shm) return true;

  if (sid < 0 || sid >= MAX_SESSIONS_NUMBER) return true;

  return  (((gov_sess_struct*)((char*)gov_shared_mem + sizeof(gov_header_struct) + MAX_DBS_NUMBER*sizeof(gov_dbs_struct) + sid*sizeof(gov_sess_struct)))->stop == 1) ? true : false;
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


transaction_id get_transaction_id(SSMMsg* sm_server)
{
   sm_msg_struct msg;
   msg.cmd = 1;
   if (sm_server->send_msg(&msg) !=0 )
      throw USER_EXCEPTION(SE3034);


   if (msg.trid == -1)
      throw USER_EXCEPTION(SE4607);

   is_trid_obtained = true;
   d_printf2("get trid=%d\n", msg.trid);
   return msg.trid;
}

void release_transaction_id(SSMMsg* sm_server)
{
   if ( is_trid_obtained == true )
   {
      if (trid < 0 || trid >= CHARISMA_MAX_TRNS_NUMBER) return;

      d_printf2("return trid=%d\n", trid);
      sm_msg_struct msg;
      msg.cmd = 2;
      msg.trid = trid;
      if (sm_server->send_msg(&msg) !=0 )
         throw USER_EXCEPTION(SE3034);
   }
   is_trid_obtained = false;

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
   string auth_query_in_por = "(query (query-prolog) (PPQueryRoot 1 (1 (PPIf (1 (PPFnNot (1 (PPFnEmpty (1 (PPDDO (1 (PPReturn (0)  (1 (PPAbsPath (document \""+ security_metadata_document +"\") (((PPAxisChild qname (\"\" \"db_security_data\"))) ((PPAxisChild qname (\"\" \"users\"))) ((PPAxisChild qname (\"\" \"user\")))))) (1 (PPIf (1 (PPCalculate (BinaryOpAnd (LeafEffectBoolOp 0) (LeafEffectBoolOp 1)) (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"user_name\") (1 (PPVariable 0)))))) (1 (PPConst \"" +string(login) + "\" !xs!string)))) (1 (PPGeneralCompEQ (1 (PPDDO (1 (PPAxisChild qname (\"\" \"user_psw\") (1 (PPVariable 0)))))) (1 (PPConst \"" + string(password) +  "\" !xs!string)))))) (1 (PPVariable 0)) (1 (PPNil)))))))))))) (1 (PPNil)) (1 (PPFnError (1 (PPConst \"Authentication failed\" !xs!string))))))))";
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

	s_pid = uGetCurrentProcessId();
	
    s = usocket(AF_INET, SOCK_STREAM, 0);
    if(s == U_SOCKET_ERROR) throw USER_EXCEPTION (SE3001);

	res = 1;
	while(uconnect_tcp(s, socket_port, "127.0.0.1")!=0)
	{
		if(!utry_connect_again())
		{
			ushutdown_close_socket(s);
			throw USER_EXCEPTION (SE3003);
		}
#ifdef _WIN32
#else
        if(ushutdown_close_socket(s)!=0) throw USER_EXCEPTION (SE3011);
        s = usocket(AF_INET, SOCK_STREAM, 0);
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
	 	if (sid == -1)
        {
            ushutdown_close_socket(s);
            throw USER_EXCEPTION(SE4607); 
        }
    }
    if(sp_msg.instruction == 171)
    {
        ushutdown_close_socket(s);
    	throw USER_EXCEPTION2(SE4409,db_name);                   //no db started
    }
    if(sp_msg.instruction == 172)
    {
        ushutdown_close_socket(s);
    	throw USER_EXCEPTION(SE3046);                            //currently there are maximum number of session in the system
    }
    if(sp_msg.instruction == 173)
    {
        ushutdown_close_socket(s);
    	throw USER_EXCEPTION(SE3043);                            //failed to register
    }

    if(ushutdown_close_socket(s)!=0) throw USER_EXCEPTION (SE3011);

}


//returns true if all database files exists
bool check_database_existence(const char* db_name)
{
   bool res1 = false, res2 = false, res3 = false, res4 = false, res5 = false, res6 = false;

   set_sedna_data();
   //delete cfg file
   res1 = uIsFileExist((string(SEDNA_DATA) + "/cfg/" + string(db_name) + "_cfg.xml").c_str());

   //delete data file
   res2 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".data").c_str());

   //delete tmp file
   res3 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".tmp").c_str());

   //delete llog file
//   res4 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".llog").c_str());

   //delete ph.bu file
   res5 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".ph").c_str());

   //delete ph file
   res6 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".ph.bu").c_str());


   if (res1 && res2 && res3/*!!! && res4*/ && res5 && res6) return true;
   else return false;
}	
