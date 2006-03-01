/*
 * File:  tr_rcv.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>
#include "persistent_db_data.h"
#include "base.h"
#include "exceptions.h"
#include "SSMMsg.h"
#include "pers_heap.h"
#include "vmm.h"
#include "indirection.h"
#include "metadata.h"
#include "log.h"
#include "d_printf.h"
#include "rcv_funcs.h"
#include "tr_debug.h"
#include "usem.h"
#include "pping.h"

using namespace std;

persistent_db_data* entry_point;
char db_name[1000];
transaction_id trid;
int sid;

//this variables are not used during recovery
int auth =AUTH_SWITCH;
char login[1];
char password[1];






int main (int argc, char** argv)
{
  if (argc != 3)
  {
     d_printf1("bad number of paprameters\n");
     return -1;
  }

  pping_client ppc("System error. This error means system malfunction.\n", 5151);

  try{
      vmm_preliminary_call();
 	  OS_exceptions_handler::install_handler();

      char buf[1024];
      strcpy(db_name, argv[1]);

      SednaUserException e = USER_EXCEPTION(SE4400);
      ppc.startup(e);
      
      set_global_names();
      set_global_names(db_name);

      INIT_DEBUG_LOG2(db_name);


 
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

      LONG_LSN last_cp_lsn = _atoi64(argv[2]);
//      std::cout << "last checkpoint lsn=" << last_cp_lsn << endl;

      recover_db_by_logical_log(last_cp_lsn);

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

      RELEASE_DEBUG_LOG2();

      USemaphore signal_end;
      if (0 != USemaphoreOpen(&signal_end, CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG))
         throw USER_EXCEPTION2(SE4012, "CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG");

      if (0 != USemaphoreUp(signal_end))
         throw USER_EXCEPTION2(SE4014, "CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG");

	  ppc.shutdown();

  } catch(SednaUserException &e) {
	  ppc.shutdown();
      fprintf(stderr, "%s\n", e.getMsg());
  } catch(...) {
      fprintf(stderr, "unknown error");
  }

  return 0;
}
