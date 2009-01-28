#include "common/sedna.h"
#include "tr/locks/locks.h"
#include "tr/auth/auc.h"
#include "common/base.h"
#include "tr/tr_functions.h"
#include "tr/pq/pq.h"
#include "tr/log/log.h"
#include "tr/executor/base/XPath.h"
#include "tr/structures/metadata.h"
#include "tr/rcv/rcv_funcs.h"
#include "tr/tr_common_funcs.h"
#include <string>

using namespace std;

bool is_sm_server_inited = false;
bool is_ph_inited = false;

bool is_trid_obtained = false;
bool is_qep_built = false;
bool is_qep_opened = false;
bool is_stmt_built = false;

static bool need_sem = true; // need to use semaphore for updater

void on_session_begin(SSMMsg* &sm_server, bool rcv_active)
{
   string log_files_path = string(SEDNA_DATA) + string("/data/") + string(db_name) + string("_files/");
   char buf[1024];
   sm_msg_struct msg;

//   DebugBreak();
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

   d_printf1("Initializing PH...");
   string ph_path = string(SEDNA_DATA) + "/data/" + db_name + "_files/" + db_name +".seph";
   if (0 != pers_init(ph_path.c_str(), CHARISMA_PH_SHARED_MEMORY_NAME, PERS_HEAP_SEMAPHORE_STR, PH_ADDRESS_SPACE_START_ADDR, 1))
      throw USER_EXCEPTION(SE4605);
   is_ph_inited = true;
   d_printf1("OK\n");

   d_printf1("Initializing VMM...");
   entry_point = vmm_on_session_begin(sm_server, rcv_active);
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

   #ifdef SE_ENABLE_TRIGGERS
   d_printf1("Initializing triggers...");
   triggers_on_session_begin(entry_point->trigger);
   d_printf1("OK\n");
   #endif

   d_printf1("Initializing local lock manager...");
   init_local_lock_mgr(sm_server);
   d_printf1("OK\n");

   d_printf1("Initializing logical log...");
   hl_logical_log_on_session_begin(log_files_path, rcv_active);
   d_printf1("OK\n");

   need_ph_reinit = is_ft_disabled = is_ro_mode;
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

void on_transaction_begin(SSMMsg* &sm_server, pping_client* ppc, bool rcv_active)
{
   // ph shutdown between transactions
   d_printf1("Releasing PH between transactions on the same session...");
   if (is_ph_inited && need_ph_reinit)
   {
      if (pers_release() != 0)
         throw USER_EXCEPTION(SE4606);

      is_ph_inited = false;
   }
   d_printf1("OK\n");
   // ph shutdown between transactions

   TIMESTAMP ts;
   int type_of_snp;

   need_sem = !is_ro_mode;

   if (need_sem)
	   down_transaction_block_sems();

   d_printf1("Getting transaction identifier...");
   trid = get_transaction_id(sm_server);
   d_printf1("OK\n");
   
   event_logger_set_trid(trid);

   d_printf1("Initializing VMM...");
   vmm_on_transaction_begin(is_ro_mode, ts, type_of_snp);
   d_printf1("OK\n");

   // start of ph reinitialization
   if (!is_ph_inited && need_ph_reinit)
   {
   	   if (is_ro_mode)
   	   {
   	   		char buf[20];

   			string ph_path = string(SEDNA_DATA) + "/data/" + db_name + "_files/" + 
   				db_name + "." + string(u_ui64toa(ts, buf, 10)) + ".seph";

   			d_printf1("Initializing PH between transactions on the same session...");
   			if (0 != pers_init(ph_path.c_str(), (type_of_snp == 1) ? CHARISMA_PH_1_SNP_SHARED_MEMORY_NAME : CHARISMA_PH_0_SNP_SHARED_MEMORY_NAME, 
   				(type_of_snp == 1) ? PERS_HEAP_1_SNP_SEMAPHORE_STR : PERS_HEAP_0_SNP_SEMAPHORE_STR, PH_ADDRESS_SPACE_START_ADDR, 0))
      				throw USER_EXCEPTION(SE4605);

   			is_ph_inited = true;
   			d_printf1("OK\n");
   	   }
   	   else
   	   {
   			d_printf1("Initializing PH between transactions on the same session...");
   			string ph_path = string(SEDNA_DATA) + "/data/" + db_name + "_files/" + db_name +".seph";
   			if (0 != pers_init(ph_path.c_str(), CHARISMA_PH_SHARED_MEMORY_NAME, PERS_HEAP_SEMAPHORE_STR, PH_ADDRESS_SPACE_START_ADDR, 0))
      			throw USER_EXCEPTION(SE4605);
   			is_ph_inited = true;

   			need_ph_reinit = false;

   			d_printf1("OK\n");
   	   }
   }	      
   // end of ph reinitialization
   
   d_printf1("Initializing indirection table...");
   indirection_table_on_transaction_begin();
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
void reportToWu(bool rcv_active, bool is_commit)
{
    sm_msg_struct msg;
    
    if (wu_reported)
        return;
    
    if (!rcv_active || (rcv_active && is_commit))
    {
        msg.cmd = 38; // transaction commit/rollback
        msg.trid = trid; 
        msg.sid = sid;
        msg.data.data[0] = 0;

        if (sm_server_wu->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);
    }
    
    wu_reported = true;
}    

// is_commit defines mode: 
//  true - transaction commit
//  false - transaction rollback
void on_transaction_end(SSMMsg* &sm_server, bool is_commit, pping_client* ppc, bool rcv_active)
{
   sm_server_wu = sm_server;
    
   ppc->stop_timer();

   clear_authmap();

   sm_msg_struct msg;
        
/*   if (!rcv_active || (rcv_active && is_commit))
   {
       msg.cmd = 38; // transaction commit/rollback
       msg.trid = trid; 
       msg.sid = sid;
       msg.data.data[0] = !is_commit;

       if (sm_server->send_msg(&msg) != 0)
           throw USER_EXCEPTION(SE1034);
   }
*/
#ifdef SE_ENABLE_TRIGGERS
   d_printf1("Triggers on transaction end...");
   triggers_on_transaction_end(is_commit);
   d_printf1("OK\n");
#endif

   d_printf1("\nReleasing logical log...");
   hl_logical_log_on_transaction_end(is_commit, rcv_active);
   d_printf1("OK\n");

/*   d_printf1("Syncing indirection table...");
   sync_indirection_table();
   d_printf1("OK\n");*/

   d_printf1("Releasing indirection table...");
   indirection_table_on_transaction_end();
   d_printf1("OK\n");

   d_printf1("\nNotifying sm of commit...");
   reportToWu(rcv_active, is_commit);
   wu_reported = false;
   
/*   
   if (!rcv_active || (rcv_active && is_commit))
   {
       msg.cmd = 38; // transaction commit/rollback
       msg.trid = trid; 
       msg.sid = sid;
       msg.data.data[0] = 0;

       if (sm_server->send_msg(&msg) != 0)
           throw USER_EXCEPTION(SE1034);
   }
*/
   // ph shutdown between transactions
   d_printf1("Releasing PH between transactions on the same session...");
   if (is_ph_inited && need_ph_reinit)
   {
      if (pers_release() != 0)
         throw USER_EXCEPTION(SE4606);

      is_ph_inited = false;
   }
   d_printf1("OK\n");
   // ph shutdown between transactions
   
   d_printf1("Releasing VMM...");
   vmm_on_transaction_end();
   d_printf1("OK\n");

   d_printf1("Releasing locks...");
   release_locks();
   d_printf1("OK\n");

   d_printf1("Releasing transaction_id...");
   release_transaction_id(sm_server);
   d_printf1("OK\n");

   event_logger_set_trid(-1);

   if (need_sem)
       up_transaction_block_sems();
}

void on_kernel_recovery_statement_begin()
{
    sid = 0;
    indirection_table_on_statement_begin();  
}

void on_kernel_recovery_statement_end()
{
    //tr_globals::st_ct.clear_context();

    tr_globals::estr_global.clear();

    PathExpr_local_free();
    PathExpr_reset_pers();

    vmm_delete_tmp_blocks();
    indirection_table_on_statement_end();
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
      msg.data.data[0] = !need_sem;

      if (sm_server->send_msg(&msg) !=0 )
         throw USER_EXCEPTION(SE3034);
   }
   is_trid_obtained = false;

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
	if (flag || (is_ro_mode && !flag))
		need_ph_reinit = true;
	else
		need_ph_reinit = false;
		
	is_ro_mode = flag;
}

// switches log modes 
// if SEDNA_LOG_LESS -- every bulkload will be logged with one record; drawback -- checkpoint at commit
// if SEDNA_LOG_FULL -- bulkload will be logged fully. checkpoint might be made though on truncate
void SwitchLogMode(int log_less_mode)
{
    is_log_less_mode = (log_less_mode == SEDNA_LOG_LESS);
}    
