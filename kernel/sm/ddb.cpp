/*
 * File:  ddb.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <iostream>
#include "common/base.h"
#include "common/errdbg/d_printf.h"
#include "common/argtable.h"
#include "common/version.h"
#include "common/pping.h"
#include "common/u/usecurity.h"
#include "common/ipc_ops.h"
#include "common/sp.h"
#include "sm/sm_functions.h"
#include "common/ugc.h"
#include "sm/db_utils.h"



using namespace std;

void* gov_shm_pointer = NULL;


const size_t narg = 4;
int ddb_help1 = 0;
int ddb_help2 = 0;
int ddb_version;
char db_name[1000];

/// Should be removed
char db_files_path[U_MAX_PATH + 1];
int bufs_num = 0;
int max_trs_num = 0;

arg_rec ddb_argtable[] =
{
{"--help",            NULL,       arg_lit,   &ddb_help1,                 "0",    "\t\t   display this help and exit"},
{"-help",             NULL,       arg_lit,   &ddb_help2,                 "0",    "\t\t\t   display this help and exit"},
{"-version",          NULL,       arg_lit,   &ddb_version,               "0",    "\t\t   display product version and exit"},
{NULL,               " db-name",  arg_str,   db_name,                    "???",  "\t\t   The name of the database "}
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

  } catch (ANY_SE_EXCEPTION) {
    return false;//cannot connect to pping server
  }

}

void* ddb_open_gov_shm(UShMem *gov_shm_service_dsc)
{
   try{
  
      return open_gov_shm(gov_shm_service_dsc);
   } catch(SednaUserException &e) {
      return NULL;
   }
   
}


int main(int argc, char** argv)
{
  program_name_argv_0 = argv[0];
            
  pping_client *ppc = NULL;

  bool sedna_work = false;
  UShMem gov_mem_dsc;

  int db_id;
  void* gov_shm_pointer = NULL;  
  bool pping_inited = false;          

    /*Under Solaris there is no SO_NOSIGPIPE/MSG_NOSIGNAL/SO_NOSIGNAL,
      so we must block SIGPIPE with sigignore.*/
#if defined(SunOS)
    sigignore(SIGPIPE);
#endif

  try {


        msg_struct msg;
        int port_number;

        SednaUserException e = USER_EXCEPTION(SE4400);

#ifdef REQUIRE_ROOT
        if (!uIsAdmin(__sys_call_error)) throw USER_EXCEPTION(SE3064);
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

        gov_header_struct cfg;
        get_sednaconf_values(&cfg);
     
		InitGlobalNames(cfg.os_primitives_id_min_bound, INT_MAX);
        SetGlobalNames();

        gov_shm_pointer = ddb_open_gov_shm(&gov_mem_dsc);

        if (gov_shm_pointer ==NULL)//gov is not started
           SEDNA_DATA = cfg.SEDNA_DATA;
        else
           SEDNA_DATA = ((gov_header_struct*)gov_shm_pointer)->SEDNA_DATA;

        if (!exist_db(db_name))
           throw USER_EXCEPTION2(SE4308 , (string("There is no database: ") + db_name).c_str());
   

        if (uSocketInit(__sys_call_error) != 0)
           throw USER_EXCEPTION(SE3001);


        //case when sedna work and we must check whether database is running
        if (gov_shm_pointer)
        {//id needed database is running then throw exception
            ppc = new pping_client(cfg.ping_port_number , EL_DDB);
            ppc->startup(e);
            pping_inited = true;

            db_id = get_db_id_by_name((gov_config_struct*)gov_shm_pointer, db_name);

            port_number = ((gov_header_struct*)gov_shm_pointer)->lstnr_port_number;

            if (db_id == -1)//there is no such database
               throw USER_EXCEPTION2(SE4308 , (string("There is no database: ") + db_name).c_str());

            SetGlobalNamesDB(db_id);

            d_printf2("port number=%d\n", port_number);

            msg_struct msg;
            USOCKET sock;

            sock = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);

            if (uconnect_tcp(sock, port_number, "127.0.0.1", __sys_call_error) == 0)
            {//connected successfully
                msg.instruction = IS_RUN_SM;
                if (strlen (db_name) > SE_MAX_DB_NAME_LENGTH)
                   throw USER_EXCEPTION(SE3015);
                strcpy(msg.body, db_name);
                msg.length = strlen(db_name)+1;
                sp_send_msg(sock, &msg);
                sp_recv_msg(sock, &msg);
                ushutdown_close_socket(sock, __sys_call_error);
   
                if ((msg.body)[0] == 'y')//database run
                   throw USER_EXCEPTION2(SE4308, "Database must be stopped firstly");

            }

            cdb_ugc(db_id, cfg.os_primitives_id_min_bound);
            ppc->shutdown();
            delete ppc;
            pping_inited = false;
  
            //!!!Here gov already closed listening socket (=>all databases already stopped) or database stopped 
        }

	d_printf2("db_name=%s\n", db_name);
	d_printf2("SEDNA_DATA=%s\n", SEDNA_DATA);

        int res_clenup_db;
        res_clenup_db = cleanup_db(db_name);



        fflush(stdout);

        if (res_clenup_db == 1)
        {
           if (gov_shm_pointer)  memset(&(((gov_config_struct*)gov_shm_pointer)->db_vars[db_id]), '\0', sizeof(gov_db_struct));
 
           fprintf(res_os, "The database '%s' has been dropped\n", db_name);
        }
        else
           throw USER_EXCEPTION2(SE4308, "Database files sharing violation");

	close_gov_shm(gov_mem_dsc, gov_shm_pointer);

        uSocketCleanup(__sys_call_error);

  } catch (SednaUserException &e) { 
      fprintf(stderr, "%s\n", e.what());
      if (pping_inited) ppc->shutdown();
      uSocketCleanup(__sys_call_error);
      close_gov_shm(gov_mem_dsc, gov_shm_pointer);
      return 1;
  } catch (SednaException &e) {
      sedna_soft_fault(e, EL_DDB);
  } catch (ANY_SE_EXCEPTION){
      sedna_soft_fault(EL_DDB);
  }


}
