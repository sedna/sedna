/*
 * File:  sm_globals.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <string>
#include "common/base.h"
#include "common/u/usocket.h"
#include "common/ipc_ops.h"
#include "common/sp.h"
#include "common/u/uprocess.h"
#include "common/u/uutils.h"
#include "sm/sm_functions.h"
#include "sm/sm_globals.h"
#include "common/SSMMsg.h"
#include "common/errdbg/d_printf.h"

using namespace std;


/*******************************************************************************
********************************************************************************
  GLOBAL VARIABLE
********************************************************************************
*******************************************************************************/


int db_id;
int bufs_num;
int max_trs_num;
double upd_crt;
int max_log_files;
int tmp_file_initial_size;

char db_name[SE_MAX_DB_NAME_LENGTH + 1];
char db_files_path[U_MAX_PATH + 1];
int sedna_db_version = 0;

//gov_server is used for connecting to the governor for registr/unregister sm
SSMMsg* gov_server;


void setup_sm_globals(gov_config_struct* cfg)
{
   if (strlen(cfg->gov_vars.SEDNA_DATA) + strlen(db_name) + 14 > U_MAX_PATH)
      throw USER_EXCEPTION2(SE1009, "Path to database files is too long");

   strcpy(db_files_path, cfg->gov_vars.SEDNA_DATA);
   strcat(db_files_path, "/data/");
   strcat(db_files_path, db_name);
   strcat(db_files_path, "_files/");

   bufs_num =  cfg->db_vars[db_id].bufs_num;
   max_trs_num = cfg->db_vars[db_id].max_trs_num;
   upd_crt = cfg->db_vars[db_id].upd_crt;
   max_log_files = cfg->db_vars[db_id].max_log_files;
   tmp_file_initial_size = cfg->db_vars[db_id].tmp_file_initial_size;

   if ( __bufs_num__ > 0 )
       bufs_num = __bufs_num__;

   if ( __max_trs_num__ > 0 )
       max_trs_num = __max_trs_num__;

   if (__upd_crt__ > 0)
   	   upd_crt = __upd_crt__;

   if (__max_log_files__ > 0)
	   max_log_files = __max_log_files__;

   if (__tmp_file_initial_size__ > 0)
       tmp_file_initial_size = __tmp_file_initial_size__ * 0x100000 / PAGE_SIZE; /* MBs -> blocks */
}

/*****************************************************************************/
/************* FUNCTIONS FOR REGISTERING/UNREGISTERING SM ON GOVERNOR ********/
/*****************************************************************************/
void
register_sm_on_gov()
{
    USOCKET s;
    int32_t sm_id;
    msg_struct msg;
    int32_t port_number;

    port_number = GOV_HEADER_GLOBAL_PTR -> lstnr_port_number;

	sm_id = uGetCurrentProcessId(__sys_call_error);

    s = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);
    if(s == U_SOCKET_ERROR)
        throw USER_EXCEPTION (SE3001);

    if(uconnect_tcp(s, port_number, "127.0.0.1", __sys_call_error)!=0)
    {
    	ushutdown_close_socket(s, __sys_call_error);
        throw USER_EXCEPTION (SE3003);
    }

    /*  Database as a string and session process id
     *  and string length as sizeof(int) bytes.
     */
    msg.length = strlen(db_name)+ 1 + 2 * sizeof(int32_t);
    msg.instruction = 122;
    msg.body[0] = 0;
    int2net_int(strlen(db_name), msg.body+1);
    memmove(msg.body+5, db_name, strlen(db_name));

    int32_t tmp = htonl(sm_id);

    memmove(msg.body + 1 + sizeof(int32_t) + strlen(db_name),
            (void*) &tmp,
            sizeof(int32_t));

    if(sp_send_msg(s,&msg) != 0)
        throw USER_EXCEPTION2(SE3006,usocket_error_translator());    /// Socket error
    if(sp_recv_msg(s,&msg) != 0)
        throw USER_EXCEPTION2(SE3006,usocket_error_translator());    /// Socket error
    if(msg.instruction == 182)
    	throw USER_EXCEPTION(SE3045);                                /// Failed to register

    if(ushutdown_close_socket(s, __sys_call_error) !=0)
        throw USER_EXCEPTION (SE3011);
}


/*****************************************************************************/
/************* VARIABLES FOR PARSING INPUT PARAMS ****************************/
/*****************************************************************************/

int sm_help = 0;
int sm_version = 0;

int background_mode = 0;
int __bufs_num__ = 0;
int __max_trs_num__ = 0;
int __tmp_file_initial_size__ = 0;

double __upd_crt__ = 0;
int __max_log_files__ = 0;

const size_t narg = 10;

arg_rec sm_argtable[] =
{
{"-help",            NULL,       arg_lit,  &sm_help,                 "0",    "\t\t\t   display this help and exit"},
{"--help",           NULL,       arg_lit,  &sm_help,                 "0",    "\t\t   display this help and exit"},
{"-version",         NULL,       arg_lit,  &sm_version,              "0",    "\t\t   display product version and exit"},
{"-background-mode", " on/off",  arg_bool, &background_mode,         "on",  "  start the server in the background mode (default on)"},
{"-bufs-num",        " N",       arg_int,  &__bufs_num__,            "-1",  "\t\t   the number of buffers in main memory, \n\t\t\t   (default value retrieved from config file)" },
{"-max-trs-num",     " N",       arg_int,  &__max_trs_num__,         "-1",  "\t   the number of concurrent micro transactions over \n\t\t\t   database, (default value retrieved from config file)" },
{"-upd-crt",         " N",       arg_dbl,  &__upd_crt__,             "-1.0","\t\t   criterion parameter to advance snapshots, \n\t\t\t   (default value retrieved from config file)"},
{"-max-log-files", " N", arg_int, &__max_log_files__, "-1", "\t   maximum log files until log truncate (default: 3)"},
{"-tmp-file-init-size", " Mbs",  arg_int, &__tmp_file_initial_size__,       "0",  "\tthe tmp file initial size (in Mb),\n\t\t\t\(default value retrieved from config file)"},
{NULL,               " db-name", arg_str,   db_name,                 "???", "\t\t   the name of the database "},
};

