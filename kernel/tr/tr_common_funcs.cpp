#include "sedna.h"
#include "locks.h"
#include "auc.h"
#include "base.h"
#include "tr_functions.h"
#include "pq.h"
#include "log.h"
#include "XPath.h"
#include "metadata.h"
#include "rcv_funcs.h"
#include "tr_common_funcs.h"
#include <string>

using namespace std;

bool is_sm_server_inited = false;
bool is_ph_inited = false;

bool is_trid_obtained = false;
bool is_qep_built = false;
bool is_qep_opened = false;
bool is_stmt_built = false;


void on_session_begin(SSMMsg* &sm_server, bool rcv_active)
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

   d_printf1("Initializing local lock manager...");
   init_local_lock_mgr(sm_server);
   d_printf1("OK\n");

   d_printf1("Initializing high level physical log...");
   hl_phys_log_on_session_begin(log_files_path + string(db_name) + string(".plog"));
   d_printf1("OK\n");

   d_printf1("Initializing logical log...");
   hl_logical_log_on_session_begin(log_files_path, rcv_active);
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

void on_transaction_begin(SSMMsg* &sm_server, bool rcv_active)
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
   hl_logical_log_on_transaction_begin(rcv_active);
   d_printf1("OK\n");
}

// is_commit defines mode: 
//  true - transaction commit
//  false - transaction rollback
void on_transaction_end(SSMMsg* &sm_server, bool is_commit, bool rcv_active)
{
   clear_authmap();

   d_printf1("\nReleasing logical log...");
   hl_logical_log_on_transaction_end(is_commit, rcv_active);
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

void on_kernel_recovery_statement_begin()
{
    sid = 0;
    indirection_table_on_statement_begin();  
}

void on_kernel_recovery_statement_end()
{
    tr_globals::st_ct.clear_context();

    e_string_first_blk = XNULL;
    e_string_last_blk = XNULL;

    tr_globals::e_str_global.clear();

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
      if (sm_server->send_msg(&msg) !=0 )
         throw USER_EXCEPTION(SE3034);
   }
   is_trid_obtained = false;

}

bool is_stop_session()
{
  if (!is_init_gov_shm) return true;

  if (sid < 0 || sid >= MAX_SESSIONS_NUMBER) return true;

  return  (((gov_sess_struct*)((char*)gov_shared_mem + sizeof(gov_header_struct) + MAX_DBS_NUMBER*sizeof(gov_dbs_struct) + sid*sizeof(gov_sess_struct)))->stop == 1) ? true : false;
}
