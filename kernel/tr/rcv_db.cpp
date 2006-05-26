/*
 * File:  tr_rcv.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include <string>
#include "base.h"
#include "SSMMsg.h"
#include "d_printf.h"
#include "rcv_funcs.h"
#include "usem.h"
#include "pping.h"
#include "tr_functions.h"


using namespace std;

//persistent_db_data* entry_point;
//char db_name[SE_MAX_DB_NAME_LENGTH+1];
//transaction_id trid = 0;
//int sid = 0;
SSMMsg *sm_server = NULL;

//this variables are not used during recovery
//int auth =AUTH_SWITCH;
//char login[1];
//char password[1];





int main (int argc, char** argv)
{
  getchar();
  if (argc != 3)
  {
     d_printf1("bad number of paprameters\n");
     return -1;
  }


  pping_client ppc(5151);

  try{
      vmm_preliminary_call();
 	  OS_exceptions_handler::install_handler();

      strcpy(db_name, argv[1]);

      if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION(SE3001);

      SednaUserException e = USER_EXCEPTION(SE4400);
      ppc.startup(e);
      
      set_global_names();
      set_global_names(db_name);

      // sid is known
      event_logger_init(EL_RCV, db_name, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
      event_logger_set_sid(sid);
      elog(EL_LOG, ("recovery process by logical log started"));



/* 
      SSMMsg sm_server(SSMMsg::Client, 
                       sizeof (sm_msg_struct), 
                       CHARISMA_SSMMSG_SM_ID(db_name, buf, 1024), 
                       SM_NUMBER_OF_SERVER_THREADS, 
                       U_INFINITE);

      d_printf1("Connecting to SM...");
      if (sm_server.init() != 0)
          throw USER_EXCEPTION(SE3030);
      d_printf1("OK\n");

      d_printf1("Initializing PH...");
      string ph_path = string(SEDNA_DATA) + "/data/" + db_name + "_files/" + db_name +".ph";
      if (0 != pers_init(ph_path.c_str(), CHARISMA_PH_SHARED_MEMORY_NAME, PERS_HEAP_SEMAPHORE_STR, PH_ADDRESS_SPACE_START_ADDR, 1))
          throw USER_EXCEPTION(SE4605);
      d_printf1("OK\n");


      d_printf1("Initializing VMM...");
      entry_point = vmm_init(&sm_server);
      d_printf1("OK\n");

      d_printf1("Initializing indirection table...");
      init_indirection_table();
      d_printf1("OK\n");


      d_printf1("Initializing metadata...");
      init_metadata(entry_point->metadata);
      d_printf1("OK\n");

      d_printf1("Initializing high level physical log...");
      string phys_log_file_path = string(SEDNA_DATA) + "/data/" + db_name + "_files/" + db_name + ".plog";
      hl_phys_log_init(phys_log_file_path);
      d_printf1("OK\n");

      d_printf1("Initializing high level logical log...");
      string logical_log_file_path = string(SEDNA_DATA) + "/data/" + db_name + "_files/" + db_name + ".llog";
      hl_logical_log_init(logical_log_file_path, true);
      d_printf1("OK\n");
*/
      on_session_begin(sm_server, true);
      on_transaction_begin(sm_server, true);//true means recovery active
      on_kernel_recovery_statement_begin();

      LONG_LSN last_cp_lsn = _atoi64(argv[2]);
//      std::cout << "last checkpoint lsn=" << last_cp_lsn << endl;

      recover_db_by_logical_log(last_cp_lsn);

/*
      d_printf1("Releasing high level physical log...");
      hl_phys_log_release();
      d_printf1("OK\n");

      d_printf1("Releasing high level logical log...");
      hl_logical_log_release();
      d_printf1("OK\n");

      d_printf1("Releasing metadata...");
      release_metadata();
      d_printf1("OK\n");

      d_printf1("Releasing indirection table...");
      release_indirection_table();
      d_printf1("OK\n");

      d_printf1("Releasing VMM...		");
      vmm_release();
      d_printf1("OK\n");

      d_printf1("Releasing PH... ");
      if (pers_release() != 0)
          throw USER_EXCEPTION(SE4606);
      d_printf1("OK\n");
*/
      on_kernel_recovery_statement_end();
      on_transaction_end(sm_server, true, true);
      on_session_end(sm_server);


      USemaphore signal_end;
      if (0 != USemaphoreOpen(&signal_end, CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG, __sys_call_error))
         throw USER_EXCEPTION2(SE4012, "CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG");

      if (0 != USemaphoreUp(signal_end, __sys_call_error))
         throw USER_EXCEPTION2(SE4014, "CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG");

      if (0 != USemaphoreClose(signal_end, __sys_call_error))
         throw USER_EXCEPTION2(SE4013, "CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG");

      elog(EL_LOG, ("recovery process by logical log finished"));
      event_logger_release();
      event_logger_set_sid(-1);

	  ppc.shutdown();

      uSocketCleanup(__sys_call_error);

  } catch(SednaUserException &e) {
      event_logger_release();
      event_logger_set_sid(-1);
	  ppc.shutdown();
      uSocketCleanup(__sys_call_error);
      fprintf(stderr, "%s\n", e.getMsg());
  } catch(...) {
      fprintf(stderr, "unknown error");
  }

  return 0;
}
