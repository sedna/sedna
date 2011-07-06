/*
 * File:  sm_globals.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>

#include "common/sedna.h"

#include "common/base.h"
#include "common/u/usocket.h"
#include "common/ipc_ops.h"
#include "common/sp.h"
#include "common/argtable.h"
#include "common/version.h"
#include "common/u/uprocess.h"
#include "common/u/uutils.h"
#include "sm/sm_functions.h"
#include "sm/sm_globals.h"
#include "common/SSMMsg.h"
#include "common/errdbg/d_printf.h"

using namespace std;
using namespace sm_globals;

///////////////////////////////////////////////////////////////////////////////
///                  STORAGE MANAGER GLOBAL VARIABLES                       ///
///////////////////////////////////////////////////////////////////////////////

namespace sm_globals {
    int    bufs_num;                                /* Number of pages to allocate for buffer memory */
    double upd_crt;                                 /* Advance snapshot criterion */
    int    max_log_files;                           /* Maximum log files */
    int    tmp_file_initial_size;                   /* Temp file initial size (in MBs)*/
    char   db_name[SE_MAX_DB_NAME_LENGTH + 1];      /* Must be set with database name */
    char   db_files_path[U_MAX_PATH + 1];           /* Must be set with path to the database files (dbname_files folder) */
    int    background_mode = 0;
}

static int    sm_help    = 0;
static int    sm_version = 0;

/* '__param_name__' variables store initial command line values:
   to pass them when we fork SM to run it in background mode
 */
static int    __bufs_num__               = 0;
static double __upd_crt__                = 0;
static int    __max_log_files__          = 0;
static int    __tmp_file_initial_size__  = 0;

static const size_t narg = 9;           /* Don't forget to change this if you want to add some new param! */
static arg_rec sm_argtable[] =
{
  {"-help",            NULL,       arg_lit,  &sm_help,                  "0",    "\t\t\t   display this help and exit"},
  {"--help",           NULL,       arg_lit,  &sm_help,                  "0",    "\t\t   display this help and exit"},
  {"-version",         NULL,       arg_lit,  &sm_version,               "0",    "\t\t   display product version and exit"},
  {"-background-mode", " on/off",  arg_bool, &background_mode,          "on",   "  start the server in the background mode (default on)"},
  {"-bufs-num",        " N",       arg_int,  &__bufs_num__,             "0",    "\t\t   the number of buffers in main memory, \n\t\t\t   (default value retrieved from config file)" },
  {"-upd-crt",         " N",       arg_dbl,  &__upd_crt__,              "0.0" , "\t\t   criterion parameter to advance snapshots, \n\t\t\t   (default value retrieved from config file)"},
  {"-max-log-files",   " N",       arg_int,  &__max_log_files__,        "0",    "\t   maximum log files until log truncate (default: 3)"},
  {"-tmp-file-init-size", " Mbs",  arg_int,  &__tmp_file_initial_size__,"0",    "  the tmp file initial size (in Mb),\n\t\t\t   (default value retrieved from config file)"},
  {NULL,               " db-name", arg_str,  db_name,                   "???",  "\t\t   the name of the database "},
};

static void 
print_sm_usage(int ret_code) {

    fprintf(stdout, "Usage: se_sm [options] dbname\n\n");
    fprintf(stdout, "options:\n%s\n", arg_glossary(sm_argtable, narg, "  ")); 
    exit(ret_code);
}

void 
parse_sm_command_line(int argc, char** argv) 
{
   char buf[1024];

   if (argc == 1) print_sm_usage(1);
   
   int res = arg_scanargv(argc, argv, sm_argtable, narg, NULL, buf, NULL);
    
   if (sm_help == 1) print_sm_usage(0);

   if (sm_version == 1) 
   { 
       print_version_and_copyright("Sedna Storage Manager");  
       exit(0);
   }

   if (res == 0)
       throw USER_ENV_EXCEPTION(buf, false);
   if (strcmp(db_name, "???") == 0)
       throw USER_ENV_EXCEPTION("Unexpected command line parameters: database name expected", false);

   check_db_name_validness(sm_globals::db_name);
}

string 
construct_sm_command_line(char** argv) 
{
    string command_line = argv[0];
    command_line += " -background-mode off ";
    command_line += " -bufs-num "           + int2string(__bufs_num__);
    command_line += " -max-log-files "      + int2string(__max_log_files__) + " ";
    command_line += " -tmp-file-init-size " + int2string(__tmp_file_initial_size__) + " ";

    char buf_uc[100];
    sprintf(buf_uc, "%.2f", __upd_crt__);

    command_line += string(" -upd-crt ") + buf_uc + " '";
    command_line += db_name + string("'");
    return command_line;
}

void 
setup_sm_globals(gov_config_struct* cfg, int db_id)
{
   if (strlen(cfg->gov_vars.SEDNA_DATA) + strlen(db_name) + 14 > U_MAX_PATH)
       throw USER_EXCEPTION2(SE1009, "Path to database files is too long");

   strcpy(db_files_path, cfg->gov_vars.SEDNA_DATA);
   strcat(db_files_path, "/data/");
   strcat(db_files_path, db_name);
   strcat(db_files_path, "_files/");

   bufs_num              = __bufs_num__      > 0 ? __bufs_num__      : cfg->db_vars[db_id].bufs_num;
   upd_crt               = __upd_crt__       > 0 ? __upd_crt__       : cfg->db_vars[db_id].upd_crt;
   max_log_files         = __max_log_files__ > 0 ? __max_log_files__ : cfg->db_vars[db_id].max_log_files;
   tmp_file_initial_size = __tmp_file_initial_size__ > 0 ? 
                           __tmp_file_initial_size__     :
                           (int)PAGES2MBS(cfg->db_vars[db_id].tmp_file_initial_size);

   if (sm_globals::tmp_file_initial_size < 1)
           throw USER_EXCEPTION2(SE4601, "'tmp_file_init_size' parameter is incorrect (must be >= 1)");
   if (sm_globals::upd_crt < 0 || sm_globals::upd_crt > 1)
           throw USER_EXCEPTION2(SE4601, "'upd-crt' parameter is incorrect (must be in [0;1])");
   if (sm_globals::max_log_files < 1)
           throw USER_EXCEPTION2(SE4601, "'max-log-files' parameter is incorrect (must be >= 1)");
   if (sm_globals::bufs_num < 1)
           throw USER_EXCEPTION2(SE4601, "'bufs-num' parameter is incorrect (must be >= 1)");
   if (sm_globals::max_log_files < 1)
           throw USER_EXCEPTION2(SE4601, "'max-log-files' parameter is incorrect (must be >= 1)");
}


///////////////////////////////////////////////////////////////////////////////
///       FUNCTIONS FOR REGISTERING/UNREGISTERING SM ON GOVERNOR            ///
///////////////////////////////////////////////////////////////////////////////
void
register_sm_on_gov()
{
    USOCKET s;
    int32_t sm_id;
    int32_t port_number;
    char gov_address[U_MAX_HOSTNAME];
    msg_struct msg;

    /* If this is the case when SM is started for some special purpose,
     * and usuaaly we don't want many transactions to be run now */
    char buf[1024];
    bool special_mode = 
        (uGetEnvironmentVariable(SEDNA_LOAD_METADATA_TRANSACTION, 
                                 buf, 1024, __sys_call_error) == 0);
    
    port_number = GOV_HEADER_GLOBAL_PTR -> lstnr_port_number;
    strcpy(gov_address, GOV_HEADER_GLOBAL_PTR -> lstnr_addr);

        sm_id = uGetCurrentProcessId(__sys_call_error);

    s = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);
    if(s == U_SOCKET_ERROR)
        throw USER_EXCEPTION (SE3001);

    if(uconnect_tcp(s, port_number, gov_address, __sys_call_error)!=0)
    {
        ushutdown_close_socket(s, __sys_call_error);
        throw USER_EXCEPTION (SE3003);
    }

    size_t db_name_len = strlen(db_name);
    /* Special mode flag, database name as a string and SM
     * process id and string length as sizeof(int) bytes. */
    msg.length = sizeof(char) + (sp_int32)db_name_len  + 2 * sizeof(int32_t);
    msg.instruction = REGISTER_DB;
    size_t off = 0;

    /* First write db_name length */
    int2net_int((int32_t)db_name_len, msg.body + off);
    off += sizeof(int32_t);

    /* Then write name itself */
    memmove(msg.body + off, db_name, db_name_len);
    off += db_name_len;
    
    /* Write process id (PID) */
    int32_t tmp = htonl(sm_id);
    memmove(msg.body + off, (void*) &tmp, sizeof(int32_t));
    off += sizeof(int32_t);

    /* Write special mode flag */
    msg.body[off] = special_mode ? 1 : 0;
    off += sizeof(char);

    if(sp_send_msg(s,&msg) != 0)
        /* Socket error occured */
        throw USER_EXCEPTION2(SE3006,usocket_error_translator());
    if(sp_recv_msg(s,&msg) != 0)
        /* Socket error occured */
        throw USER_EXCEPTION2(SE3006,usocket_error_translator());
    if(msg.instruction == 182)
        /* Failed to register */
        throw USER_EXCEPTION(SE3045);

    if(ushutdown_close_socket(s, __sys_call_error) !=0)
        throw USER_EXCEPTION (SE3011);
}

