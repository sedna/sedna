/*
 * File:  sm_globals.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>
#include "expat.h"
#include "base.h"
#include "usocket.h"
#include "ipc_ops.h"
#include "sp.h"
#include "uprocess.h"
#include "exceptions.h"
#include "sm_functions.h"
#include "sm_globals.h"
#include "SSMMsg.h"
#include "d_printf.h"

using namespace std;





/*******************************************************************************
********************************************************************************
  GLOBAL VARIABLE
********************************************************************************
*******************************************************************************/

int bufs_num;
int max_trs_num;
int phys_log_ext_portion;
int phys_log_size;

char* db_name = NULL;
char *db_files_path;


//gov_server is used for connecting to the governor for registr/unregister sm
SSMMsg* gov_server;



void setup_sm_globals(char *_db_name_)
{
   
   char buf[1000];

   string data_files_path = string(SEDNA_DATA) + "/data/" + _db_name_ + "_files/";
   db_files_path = new char[data_files_path.length() + 1];
   strcpy(db_files_path, data_files_path.c_str());


   string cfg_file_name = string(SEDNA_DATA) + "/cfg/" + _db_name_ + "_cfg.xml";
   string cfg_file_content;


   FILE *f = fopen(cfg_file_name.c_str(), "r");

   if (f == NULL)
       throw USER_EXCEPTION2(SE4200,  _db_name_);

   while( !feof(f) )
   {
     size_t len = fread (buf, sizeof(char), 1000, f);

     if ( ferror(f) )
        throw USER_EXCEPTION2(SE4044,  cfg_file_name.c_str());

     cfg_file_content.append(buf, len);
  }

  //init data base name
  db_name = new char[strlen(_db_name_)+1];
  strcpy(db_name, _db_name_);

  //init bufs_num and max_trs_num from config file

  XML_Parser parser = XML_ParserCreate (NULL);

  XML_SetElementHandler (parser, startElement_sm_cfg, endElement_sm_cfg);

  XML_SetCharacterDataHandler (parser, characterData_sm_cfg);
   
  //string curr_tag_name = "";
  CfgParserContext cnt;
  cnt.tag_name = "";
  cnt.content = "";

  XML_SetUserData (parser, &cnt);

  int parse_res;

  parse_res = XML_Parse (parser, cfg_file_content.c_str(), cfg_file_content.length(), 1);

  if( parse_res == XML_STATUS_ERROR )

    throw USER_EXCEPTION2(SE4201,  cfg_file_name.c_str());

  XML_ParserFree(parser);
      
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


	sm_id = uGetCurrentProcessId();
	
    s = usocket(AF_INET, SOCK_STREAM, 0);
    if(s == U_SOCKET_ERROR) throw USER_EXCEPTION (SE3001);
    if(uconnect_tcp(s, port_number, "127.0.0.1")!=0)
    {
    	ushutdown_close_socket(s);
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

    if(sp_send_msg(s,&msg)!=0) throw USER_EXCEPTION2(SE3006,string(usocket_error_translator()));

    if(sp_recv_msg(s,&msg)!=0) throw USER_EXCEPTION2(SE3006,string(usocket_error_translator()));
//    if(msg.instruction == 181)
//    	d_printf2("se_sm: SM with %d registered on gov successfully\n", sm_id);
    if(msg.instruction == 182)
    	throw USER_EXCEPTION(SE3045);                            //failed to register

    if(ushutdown_close_socket(s)!=0) throw USER_EXCEPTION (SE3011);

}


/*****************************************************************************/
/************* VARIABLES FOR PARSING INPUT PARAMS ****************************/
/*****************************************************************************/

int sm_help = 0;
int sm_version = 0;

int background_mode = 0; 
int __bufs_num__ = 0;
int __max_trs_num__ = 0;
char __db_name__[1000];
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
{NULL,               " db-name", arg_str,  &__db_name__,             "???",  "\t\t   The name of the database "},
};

