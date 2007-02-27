/*
 * File:  sm_globals.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <string>
#include "expat/expat.h"
#include "common/base.h"
#include "common/u/usocket.h"
#include "common/ipc_ops.h"
#include "common/sp.h"
#include "common/u/uprocess.h"
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


void * gov_shm_pointer = NULL; // global gov shared memory pointer
int db_id;
int bufs_num;
int max_trs_num;
int phys_log_ext_portion;
int phys_log_size;

char db_name[SE_MAX_DB_NAME_LENGTH + 1];
char db_files_path[U_MAX_PATH + 1];
int sedna_db_version = 0;


//gov_server is used for connecting to the governor for registr/unregister sm
SSMMsg* gov_server;




void setup_sm_globals(gov_config_struct* cfg)
{   
   char buf[1024];

   if (strlen(cfg->gov_vars.SEDNA_DATA) + strlen(db_name) + 14 > U_MAX_PATH)
      throw USER_EXCEPTION2(SE1009, "Path to database files is too long");

   strcpy(db_files_path, cfg->gov_vars.SEDNA_DATA);
   strcat(db_files_path, "/data/");
   strcat(db_files_path, db_name);
   strcat(db_files_path, "_files/");


   bufs_num =  cfg->db_vars[db_id].bufs_num;
   max_trs_num = cfg->db_vars[db_id].max_trs_num;
   phys_log_ext_portion = cfg->db_vars[db_id].phys_log_ext_portion;
   phys_log_size = cfg->db_vars[db_id].phys_log_size;

   if ( __bufs_num__ > 0 )
       bufs_num = __bufs_num__;

   if ( __max_trs_num__ > 0 )
       max_trs_num = __max_trs_num__;
}

/*****************************************************************************/
/************* FUNCTIONS FOR REGISTERING/UNREGISTERING SM ON GOVERNOR ********/
/*****************************************************************************/
void register_sm_on_gov()
{
	USOCKET s;
	int sm_id;
	msg_struct msg;
    int port_number;
    UShMem gov_mem_dsc;
    void* gov_shm_pointer = NULL;

    gov_shm_pointer = open_gov_shm(&gov_mem_dsc);
    port_number = ((gov_header_struct*)gov_shm_pointer)->lstnr_port_number;
    close_gov_shm(gov_mem_dsc, gov_shm_pointer);


	sm_id = uGetCurrentProcessId(__sys_call_error);
	
    s = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);
    if(s == U_SOCKET_ERROR) throw USER_EXCEPTION (SE3001);
    if(uconnect_tcp(s, port_number, "127.0.0.1", __sys_call_error)!=0)
    {
    	ushutdown_close_socket(s, __sys_call_error);
    	throw USER_EXCEPTION (SE3003);
    }
                
    msg.instruction = 122; 
    msg.length = strlen(db_name)+5+4; //dbname as a string and session process id as 4 bytes
    msg.body[0] = 0;
    int2net_int(strlen(db_name), msg.body+1);    
    memmove(msg.body+5, db_name, strlen(db_name));
    
    __int32 tmp = htonl(sm_id);
    char *ptr = (char*) &(tmp);	

    memmove(msg.body+5+strlen(db_name),ptr,4);

    if(sp_send_msg(s,&msg)!=0) throw USER_EXCEPTION2(SE3006,usocket_error_translator());

    if(sp_recv_msg(s,&msg)!=0) throw USER_EXCEPTION2(SE3006,usocket_error_translator());
//    if(msg.instruction == 181)
//    	d_printf2("se_sm: SM with %d registered on gov successfully\n", sm_id);
    if(msg.instruction == 182)
    	throw USER_EXCEPTION(SE3045);                            //failed to register

    if(ushutdown_close_socket(s, __sys_call_error)!=0) throw USER_EXCEPTION (SE3011);

}


/*****************************************************************************/
/************* VARIABLES FOR PARSING INPUT PARAMS ****************************/
/*****************************************************************************/

int sm_help = 0;
int sm_version = 0;

int background_mode = 0; 
int __bufs_num__ = 0;
int __max_trs_num__ = 0;
int write_phys_log = 1;


const size_t narg = 7;

arg_rec sm_argtable[] =
{
{"-help",            NULL,       arg_lit,  &sm_help,                 "0",    "\t\t\t   display this help and exit"},
{"--help",           NULL,       arg_lit,  &sm_help,                 "0",    "\t\t   display this help and exit"},
{"-version",         NULL,       arg_lit,  &sm_version,              "0",    "\t\t   display product version and exit"},
{"-background-mode", " on/off",  arg_bool, &background_mode,         "on",  "  start the server in the background mode (default on)"},
{"-bufs-num",        " N",       arg_int,  &__bufs_num__,             "-1",  "\t\t   the number of buffers in main memory, \n\t\t\t   (default value retrieved from config file)" },
{"-max-trs-num",     " N",       arg_int,  &__max_trs_num__,          "-1",  "\t   the number of concurrent micro transactions over \n\t\t\t   database, (default value retrieved from config file)" },
/*
{"-write-phys-log",  " on/off",  arg_bool, &write_phys_log,           "on",  "   write to physical log (default on)"},
*/
{NULL,               " db-name", arg_str,  db_name,                  "???",  "\t\t   The name of the database "},
};

