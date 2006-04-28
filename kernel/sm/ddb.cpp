/*
 * File:  ddb.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include <iostream>
#include "base.h"
#include "d_printf.h"
#include "argtable.h"
#include "version.h"
#include "pping.h"
#include "usecurity.h"
#include "ipc_ops.h"
#include "sp.h"
#include "cdb_globals.h"
#include "sm_functions.h"
#include "ugc.h"
#include "db_utils.h"



using namespace std;


const size_t narg = 4;
int ddb_help1 = 0;
int ddb_help2 = 0;
int ddb_version;
char db_name[1000];



arg_rec ddb_argtable[] =
{
{"--help",            NULL,       arg_lit,   &ddb_help1,                 "0",    "\t\t   display this help and exit"},
{"-help",             NULL,       arg_lit,   &ddb_help2,                 "0",    "\t\t\t   display this help and exit"},
{"-version",          NULL,       arg_lit,   &ddb_version,              "0",    "\t\t   display product version and exit"},
{NULL,               " db-name",  arg_str,   &db_name,                "???",  "\t\t   The name of the database "}
};


void print_ddb_usage()
{
   throw USER_SOFT_EXCEPTION((string("Usage: se_ddb [options] dbname \n\n") +
                              string("options:\n") + string(arg_glossary(ddb_argtable, narg, "  ")) + string("\n")).c_str());
}


bool start_ppc(pping_client& ppc, SednaUserException& e)
{
  try
  {
    ppc.startup(e);
    return true;

  } catch (...) {
    return false;//cannot connect to pping server
  }

}


int main(int argc, char** argv)
{
  program_name_argv_0 = argv[0];
            
  pping_client ppc(5151);

  bool sedna_work = false;
            

  try {

        UShMem gov_mem_dsc;
        msg_struct msg;
        void* gov_shm_pointer = NULL;
        int port_number;

        SednaUserException e = USER_EXCEPTION(SE4400);

#ifdef REQUIRE_ROOT
        if (!uIsAdmin()) throw USER_EXCEPTION(SE3064);
#endif
   
        int arg_scan_ret_val = 0; // 1 - parsed successful, 0 - there was errors
        char errmsg[1000];
        arg_scan_ret_val = arg_scanargv(argc, argv, ddb_argtable, narg, NULL, errmsg, NULL);
        if (ddb_help1 == 1 || ddb_help2 == 1 ) print_ddb_usage();
        if (ddb_version == 1) { print_version_and_copyright("Sedna Drop Data Base Utility"); throw USER_SOFT_EXCEPTION(""); }
        if (arg_scan_ret_val == 0)
           throw USER_EXCEPTION2(SE4601, errmsg);


        if (string(db_name) == "???")
           throw USER_EXCEPTION2(SE4601, "The name of the database must be specified (type option '-help')");
     
        set_global_names();
	
        if (!exist_db(db_name))
           throw USER_EXCEPTION2(SE4308 , (string("There is no database: ") + db_name).c_str());


        
        set_global_names(db_name);


        sedna_work = start_ppc(ppc, e);

        //case when sedna work and we must check whether database is running
        if (sedna_work)
        {//id needed database is running then throw exception
            gov_shm_pointer = open_gov_shm(&gov_mem_dsc);
            port_number = ((gov_header_struct*)gov_shm_pointer)->lstnr_port_number;
            close_gov_shm(gov_mem_dsc, gov_shm_pointer);

            d_printf2("port number=%d\n", port_number);

            msg_struct msg;
            USOCKET sock;

            sock = usocket(AF_INET, SOCK_STREAM, 0);

            if (uconnect_tcp(sock, port_number, "127.0.0.1") == 0)
            {//connected successfully
                msg.instruction = IS_RUN_SM;
                if (strlen (db_name) > SE_MAX_DB_NAME_LENGTH)
                   throw USER_EXCEPTION(SE3015);
                strcpy(msg.body, db_name);
                msg.length = strlen(db_name)+1;
                sp_send_msg(sock, &msg);
                sp_recv_msg(sock, &msg);
                ushutdown_close_socket(sock);
   
                if ((msg.body)[0] == 'y')//database run
                   throw USER_EXCEPTION2(SE4308, "Database must be stopped firstly");

            }

            //!!!Here gov already closed listening socket (=>all databases already stopped) or database stopped 
        }

	d_printf2("db_name=%s\n", db_name);
	d_printf2("SEDNA_DATA=%s\n", SEDNA_DATA);
        cdb_ugc(db_name);

        int res_clenup_db;
        res_clenup_db = cleanup_db(db_name);



        fflush(stdout);

       
        if (res_clenup_db == 1)
           fprintf(res_os, "The database '%s' has been dropped\n", db_name);
        else
           throw USER_EXCEPTION2(SE4308, "Database files sharing violation");

		if (sedna_work) ppc.shutdown();

  } catch (SednaUserException &e) { 
      fprintf(stderr, "%s\n", e.getMsg().c_str());
      if (sedna_work) ppc.shutdown();
      return 1;
  } catch (SednaException &e) {
      sedna_soft_fault(e);
  } catch (...){
      sedna_soft_fault();
  }


}
