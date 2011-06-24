/*
 * File:  rc.cpp
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>

#include "common/sedna.h"

#include "common/argtable.h"
#include "common/version.h"
#include "common/pping.h"
#include "common/sp.h"
#include "common/ipc_ops.h"
#include "common/base.h"
#include "common/config.h"

#include "common/u/uprocess.h"
#include "common/u/uutils.h"

#include "gov/listener.h"
#include "gov/rc.h"

#define RC_TIMEOUT              30000


static const size_t narg = 4;
static int rc_help;
static int rc_version;
static int rc_sm_list;

static arg_rec rc_argtable[] =
{
  {"--help",            NULL,       arg_lit,   &rc_help,                 "0",    "\t\t   display this help and exit"},
  {"-help",             NULL,       arg_lit,   &rc_help,                 "0",    "\t\t\t   display this help and exit"},
  {"-version",          NULL,       arg_lit,   &rc_version,              "0",    "\t\t   display product version and exit"},
  {"-sm-list",          NULL,       arg_lit,   &rc_sm_list,              "0",    "\t\t   display running databases list"}
};


static void print_rc_usage()
{
    fprintf(stdout, "Usage: se_rc [options]\n\n");
    fprintf(stdout, "options:\n%s\n", arg_glossary(rc_argtable, narg, "  ")); 
}


/* Parses message received from the governor. 
 * For details see comment in the listener.cpp.
 */
static void parse_and_print_rc(const msg_struct* msg, bool sm_list)
{
    /// Anyway at this moment we know that gov is running.
    if(!sm_list) fprintf(res_os, "Sedna GOVERNOR is running.\n\n");
    
    switch(msg->body[0])
    {
        case SE_RC_INVALID:
            fprintf(res_os, "Runtime configuration has not been sent since it's state is inconsistent!\n");    
            break;
        case SE_RC_OVERFLOW:
            fprintf(res_os, "Runtime configuration has not been sent since it is too large!\n");    
            break;
        case SE_RC_VALID:
        {
            rc_vector rc;
            size_t offset = 1;
            uint32_t db_number;

            net_int2int(&db_number, msg->body + offset);
            offset += sizeof(int);

            for(uint32_t i = 0; i < db_number; i++)
            {
                uint32_t sessions_number;
                net_int2int(&sessions_number, msg->body + offset);
                offset += sizeof(int);

                std::string db_name(msg->body + offset);
                offset += db_name.size() + 1;

                rc.insert( rc_pair(db_name, sessions_number) );

                if(offset > SE_SOCKET_MSG_BUF_SIZE)
                    throw USER_EXCEPTION2(SE5200, "Offset has been overflowed while parsing runtime configuration");
            }

            rc_const_iterator rit = rc.begin();
            rc_const_iterator rit_end = rc.end();
  
            if(sm_list)
            {
                for(; rit != rit_end; rit++)
                    fprintf(res_os, "%s\n", (rit->first).c_str());
            }
            else if(rit != rit_end)
            {
                    fprintf(res_os, "The following databases (SMs) are started:\n\n");
                    for(; rit != rit_end; rit++)
                        fprintf(res_os, "\t%s, %d session(s)\n", (rit->first).c_str(), rit->second);
            }
            break;
        } 
        default:
            throw USER_EXCEPTION2(SE5200, "Impossible case while parsing runtime configuration");
    }
}

int main(int argc, char **argv)
{
    program_name_argv_0 = argv[0];  /// This global variable is used in uGetImageProcPath! Don't remove it!
    pping_client *ppc = NULL;
    int port_number;
    char gov_address[U_MAX_HOSTNAME];
    USOCKET sock;
    int res;
    msg_struct msg;
    SednaUserSoftException ex = USER_SOFT_EXCEPTION("There is no any sign of the SEDNA server running in the system");
    char errmsg[1000];
    gov_header_struct cfg;


    /*Under Solaris there is no SO_NOSIGPIPE/MSG_NOSIGNAL/SO_NOSIGNAL,
      so we must block SIGPIPE with sigignore.*/
#if defined(SunOS)
    sigignore(SIGPIPE);
#endif


    try{
        res = arg_scanargv(argc, argv, rc_argtable, narg, NULL, errmsg, NULL);
        
        if (rc_help == 1 ) { 
            print_rc_usage();
            return 0;
        }    
        
        if (rc_version == 1) { 
            print_version_and_copyright("Sedna Runtime Configuration Utility"); 
            return 0; 
        }

        if (res == 0) throw USER_EXCEPTION2(SE4601, errmsg);

        /* Parse config file to get id_min_bound value */
        get_sednaconf_values(&cfg);
     
                /* Initialize global names with given id_min_bound number */
        InitGlobalNames(cfg.os_primitives_id_min_bound, INT_MAX);
        SetGlobalNames();

        /* Connect to the governor shared memory to 
         * get port number, ping port and SEDNA_DATA 
         */
        open_gov_shm();
        SEDNA_DATA = GOV_HEADER_GLOBAL_PTR -> SEDNA_DATA;


#ifdef REQUIRE_ROOT
        if (!uIsAdmin(__sys_call_error)) throw USER_EXCEPTION(SE3064);
#endif

        if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR) throw SYSTEM_EXCEPTION("Failed to initialize socket library");

        ppc = new pping_client(GOV_HEADER_GLOBAL_PTR -> ping_port_number, EL_RC);
        ppc->startup(ex);

        event_logger_init(EL_RC, NULL, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
        elog(EL_LOG, ("Request for runtime configuration issued"));

        port_number = GOV_HEADER_GLOBAL_PTR -> lstnr_port_number;
        strcpy(gov_address, GOV_HEADER_GLOBAL_PTR -> lstnr_addr);
        close_gov_shm();

        sock = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);
        if (uconnect_tcp(sock, port_number, gov_address, __sys_call_error) == 0)
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

            parse_and_print_rc(&msg, rc_sm_list != 0);
        }
        else
            throw USER_EXCEPTION(SE3003);

        event_logger_release();

        ppc->shutdown();
        delete ppc;
        ppc = NULL;
        
        if (uSocketCleanup(__sys_call_error) == U_SOCKET_ERROR) 
           throw SYSTEM_EXCEPTION("Failed to clean up socket library");

    } catch (SednaUserSoftException &e) {
        fprintf(stderr, "%s\n", e.what());
        event_logger_release();
        if (ppc)  { ppc->shutdown(); delete ppc; ppc = NULL; }
        close_gov_shm();
        return 0;
    } catch (SednaUserException &e) { 
        fprintf(stderr, "%s\n", e.what());
        event_logger_release();
        if (ppc) { ppc->shutdown(); delete ppc; ppc = NULL; }
        close_gov_shm();
        return 1;
    } catch (SednaException &e) { 
        sedna_soft_fault(e, EL_RC);
    } catch (ANY_SE_EXCEPTION) {
        sedna_soft_fault(EL_RC);
    }

    return 0;
}
