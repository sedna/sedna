/*
 * File:  smsd.cpp
 * Storage Manager Shutdwon Utility
 * Copyright (C) 2004-2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <iostream>
#include <string.h>

#include "common/sedna.h"
#include "common/base.h"
#include "common/argtable.h"
#include "common/version.h"
#include "common/pping.h"
#include "common/ipc_ops.h"

using namespace std;


static const size_t narg = 4;
static int smsd_help     = 0;
static int smsd_version  = 0;
static char db_name[1000];

static arg_rec smsd_argtable[] =
{
  {"--help",            NULL,       arg_lit,   &smsd_help,                 "0",    "\t\t   display this help and exit"},
  {"-help",             NULL,       arg_lit,   &smsd_help,                 "0",    "\t\t\t   display this help and exit"},
  {"-version",          NULL,       arg_lit,   &smsd_version,              "0",    "\t\t   display product version and exit"},
  {NULL,               " db-name",  arg_str,   db_name,                    "???",  "\t\t   The name of the database "}
};


static void print_smsd_usage()
{
    fprintf(stdout, "Usage: se_smsd [options]\n\n");
    fprintf(stdout, "options:\n%s\n", arg_glossary(smsd_argtable, narg, "  ")); 
}


int main(int argc, char **argv)
{
    program_name_argv_0 = argv[0];
    UPID sm_pid = -1;
    char gov_address[U_MAX_HOSTNAME];
    int port_number;
    int command = STOP;
    int db_id;
    int res;
    pping_client *ppc = NULL;
    char errmsg[1000];
    UPHANDLE proc_handle;
 
    try {
        SednaUserException e = USER_EXCEPTION(SE4400);

#ifdef REQUIRE_ROOT
        if (!uIsAdmin(__sys_call_error)) throw USER_EXCEPTION(SE3064);
#endif
        res = arg_scanargv(argc, argv, smsd_argtable, narg, NULL, errmsg, NULL);
        
        if (smsd_help == 1 ) {
            print_smsd_usage();
            return 0;
        }
        
        if (smsd_version == 1) { 
            print_version_and_copyright("Sedna Shutdown Data Base Utility"); 
            return 0; 
        }
        
        if (res == 0)
           throw USER_EXCEPTION2(SE4601, errmsg);

        if (string(db_name) == "???")
           throw USER_EXCEPTION2(SE4601, "The name of the database must be specified");

        check_db_name_validness(db_name);
     
        gov_header_struct cfg;
        get_sednaconf_values(&cfg);
     
        InitGlobalNames(cfg.os_primitives_id_min_bound, INT_MAX);
                SetGlobalNames();

        open_gov_shm();

        SEDNA_DATA  = GOV_HEADER_GLOBAL_PTR -> SEDNA_DATA;
        db_id       = get_db_id_by_name(GOV_CONFIG_GLOBAL_PTR, db_name);
        port_number = GOV_HEADER_GLOBAL_PTR -> lstnr_port_number;
        strcpy(gov_address, GOV_HEADER_GLOBAL_PTR -> lstnr_addr);

        /* There is no such database? */
        if (db_id == -1) goto end;
        
        if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION(SE3001);

        SetGlobalNamesDB(db_id);

        ppc = new pping_client(GOV_HEADER_GLOBAL_PTR -> ping_port_number, EL_SMSD);
        ppc->startup(e);

        event_logger_init(EL_SMSD, db_name, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
        elog(EL_LOG, ("Request for SM shutdown issued"));

        port_number = GOV_HEADER_GLOBAL_PTR -> lstnr_port_number;
        sm_pid      = GOV_CONFIG_GLOBAL_PTR -> db_vars[db_id].sm_pid;

        /* Check if database is already stopped */
        if(sm_pid == -1 ||
           GOV_CONFIG_GLOBAL_PTR -> db_vars[db_id].mode == OM_SM_SHUTDOWN ||
           GOV_CONFIG_GLOBAL_PTR -> db_vars[db_id].mode == OM_SM_DOWN) {
            sm_pid = -1;
            goto end;
        }

        /* Check once again if there is SM process? */
        res = uOpenProcess(sm_pid, &proc_handle, __sys_call_error);
        if (res != 0)
            throw USER_ENV_EXCEPTION("Cannot open SM process to stop. SM is already stopped?", false);

        GOV_CONFIG_GLOBAL_PTR -> db_vars[db_id].mode = OM_SM_SHUTDOWN;

        send_command_to_gov(port_number, gov_address, command);
        uWaitForProcess(sm_pid, proc_handle, __sys_call_error);
        uCloseProcessHandle(proc_handle, __sys_call_error);

        elog(EL_LOG, ("Request for SM shutdown satisfied"));
        event_logger_release();

end:    if(ppc) {
            ppc->shutdown();
            delete ppc;
            ppc = NULL;
        }

        close_gov_shm();

        if (uSocketCleanup(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION(SE3000);

        if (db_id != -1)
        {
            if(sm_pid == -1)
                fprintf(res_os, "The database '%s' is not running or already stopped\n", db_name);
            else
                fprintf(res_os, "The database '%s' has been successfully shut down\n", db_name);
        }
        else
            fprintf(res_os, "There is no database with name '%s'\n", db_name);

        fflush(res_os);
        
    } catch (SednaUserException &e) { 
        fprintf(stderr, "%s\n", e.what());
        event_logger_release();
        if (ppc) { 
            ppc->shutdown(); 
            delete ppc; 
            ppc = NULL; 
        }
        close_gov_shm();
        return 1;
    } catch (SednaException &e) {
        sedna_soft_fault(e, EL_SMSD);
    } catch (ANY_SE_EXCEPTION){
        sedna_soft_fault(EL_SMSD);
    }

    return 0;
}
