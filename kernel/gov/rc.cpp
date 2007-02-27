/*
 * File:  rc.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "common/argtable.h"
#include "common/version.h"
#include "common/pping.h"
#include "gov/listener.h"
#include "common/sp.h"
#include "common/ipc_ops.h"
#include "common/base.h"
#include "common/u/uprocess.h"
#include "common/config.h"

using namespace std;


const size_t narg = 3;
int rc_help;
int rc_version;

arg_rec rc_argtable[] =
{
{"--help",            NULL,       arg_lit,   &rc_help,                 "0",    "\t\t   display this help and exit"},
{"-help",             NULL,       arg_lit,   &rc_help,                 "0",    "\t\t\t   display this help and exit"},
{"-version",          NULL,       arg_lit,   &rc_version,              "0",    "\t\t   display product version and exit"}
};


void print_rc_usage()
{
   throw USER_SOFT_EXCEPTION((string("Usage: se_rc [options]\n\n") +
                              string("options:\n") + string(arg_glossary(rc_argtable, narg, "  ")) + string("\n")).c_str());
}


#define RC_TIMEOUT		30000

int main(int argc, char **argv)
{

    char *db_name;
    program_name_argv_0 = argv[0];
    pping_client ppc(5151, EL_RC);
    int port_number;
    USOCKET sock;
    int res;

    msg_struct msg;

    UShMem gov_mem_dsc;
    void* gov_shm_pointer = NULL;


    try{
        SednaUserSoftException ex = USER_SOFT_EXCEPTION("There is no any sign of the SEDNA server running in the system");


        int arg_scan_ret_val = 0; // 1 - parsed successful, 0 - there was errors
        char errmsg[1000];
        arg_scan_ret_val = arg_scanargv(argc, argv, rc_argtable, narg, NULL, errmsg, NULL);
        
        if (rc_help == 1 ) print_rc_usage();
        if (rc_version == 1) { print_version_and_copyright("Sedna Runtime Configuration Utility"); throw USER_SOFT_EXCEPTION(""); }


        if (arg_scan_ret_val == 0)
           throw USER_EXCEPTION2(SE4601, errmsg);

        gov_header_struct cfg;
        get_gov_config_parameters_from_sednaconf(&cfg);//get config parameters from sednaconf
     
        set_global_names(cfg.os_primitives_id_min_bound);

        gov_shm_pointer = open_gov_shm(&gov_mem_dsc);

        SEDNA_DATA = ((gov_header_struct*)gov_shm_pointer)->SEDNA_DATA;


#ifdef REQUIRE_ROOT
        if (!uIsAdmin(__sys_call_error)) throw USER_EXCEPTION(SE3064);
#endif

        if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR) throw SYSTEM_EXCEPTION("Failed to initialize socket library");

        ppc.startup(ex);

        event_logger_init(EL_RC, NULL, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
        elog(EL_LOG, ("Request for runtime configuration issued"));

        port_number = ((gov_config_struct*)gov_shm_pointer)->gov_vars.lstnr_port_number;
        close_gov_shm(gov_mem_dsc, gov_shm_pointer);


        sock = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);
        if (uconnect_tcp(sock, port_number, "127.0.0.1", __sys_call_error) == 0)
        {
            msg.instruction = RUNTIME_CONFIG;
            msg.length = 0;
            res = sp_send_msg(sock, &msg);
            if (res != 0) throw USER_EXCEPTION(SE3006);
            
            res = sp_recv_msg(sock, &msg);
            if (res != 0) throw USER_EXCEPTION(SE3007);

            res = ushutdown_close_socket(sock, __sys_call_error);
            if (res != 0) throw USER_EXCEPTION(SE3011);

            elog(EL_LOG, ("Request for runtime configuration satisfied"));

            fprintf(res_os, "%s\n", msg.body);
        }
        else
            throw USER_EXCEPTION(SE3003);

        event_logger_release();

        ppc.shutdown();
        if (uSocketCleanup(__sys_call_error) == U_SOCKET_ERROR) throw SYSTEM_EXCEPTION("Failed to clean up socket library");
        

    } catch (SednaUserSoftException &e) {
        fprintf(stderr, "%s\n", e.getMsg().c_str());
        event_logger_release();
        ppc.shutdown();
        close_gov_shm(gov_mem_dsc, gov_shm_pointer);
        return 0;        
    } catch (SednaUserException &e) { 
        fprintf(stderr, "%s\n", e.getMsg().c_str());
        event_logger_release();
        ppc.shutdown();
        return 1;
    } catch (SednaException &e) { 
        sedna_soft_fault(e, EL_RC);
    } catch (...) {
        sedna_soft_fault(EL_RC);
    }

    return 0;
}

