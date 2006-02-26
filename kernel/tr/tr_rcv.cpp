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

using namespace std;

persistent_db_data* entry_point;


int main (int argc, char** argv)
{
  try{
      char buf[1024];
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
      if (0 != pers_init(ph_path.c_str(), CHARISMA_PH_SHARED_MEMORY_NAME, PERS_HEAP_SEMAPHORE_STR, PH_ADDRESS_SPACE_START_ADDR))
          throw USER_EXCEPTION(SE4605);
      d_printf1("OK\n");


      d_printf1("Initializing VMM...");
      entry_point = vmm_init(&sm_server, db_name);
      d_printf1("OK\n");

      d_printf1("Initializing indirection table...");
      init_indirection_table(db_name);
      d_printf1("OK\n");

      d_printf1("Initializing metadata...");
      init_metadata(&(entry_point->metadata), db_name);
      d_printf1("OK\n");

      d_printf1("Initializing high level physical log...");
      hl_phys_log_init(phys_log_file_path);
      d_printf1("OK\n");


      //actions....

      d_printf1("Releasing high level physical log...");
      hl_phys_log_release();
      d_printf1("OK\n");

      d_printf1("Releasing metadata...");
      release_metadata();
      d_printf1("OK\n");

      d_printf1("Releasing indirection table...");
      release_indirection_table();
      d_printf1("OK\n");

      d_printf1("Releasing VMM...		");
      vmm_destroy();
      d_printf1("OK\n");

      d_printf1("Releasing PH... ");
      if (pers_release() != 0)
          throw USER_EXCEPTION(SE4606);
      d_printf1("OK\n");
  } catch(CharismaException &e) {
      cout << e.getMsg() << endl;
  } catch(...) {
      cout << "unknown error" << endl;
  }


}
