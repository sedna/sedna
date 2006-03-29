/*
 * File:  sm_functions.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <string>
#include "sm_functions.h"
#include "sm_globals.h"
#include "llmgr.h"
#include "plmgr.h"
#include "bm_core.h"
#include "usem.h"
#include "exceptions.h"
#include "ipc_ops.h"

using namespace std;

/******************************************************************************/
// HANDLERS FOR PARSING CONFIG FILES
/******************************************************************************/

//src is the string from which NewLine character must be removed

void rmNewLine(char* src)
{
   char *tmp_buf;
   tmp_buf = new char[strlen(src)+1];
   int i=0, j = 0;

   for (i=0; i < (int)strlen(src); i++)
   {
     if (src[i] != '\n')
     {
        tmp_buf[j] = src[i];
        j++;
     }
   }
   tmp_buf[j] = '\0';

   strcpy(src, tmp_buf);

   delete [] tmp_buf;
}

void startElement_sm_cfg(void *cnt, const char *name, const char **atts)
{
 
  (((CfgParserContext*)cnt)->tag_name).assign(name);
  (((CfgParserContext*)cnt)->content).assign("");
}

void endElement_sm_cfg(void *cnt, const char *name)
{

  string _tag_name_ = ((CfgParserContext*)cnt)->tag_name;
  string _content_ = ((CfgParserContext*)cnt)->content;
/*
  if (_tag_name_ == "name")
  {
     db_name = new char[_content_.length()+1];
     strcpy(db_name, _content_.c_str());
  }
*/
  if ( _tag_name_ == "bufs_num")
  {  
     bufs_num = atoi(_content_.c_str());
  }

  if ( _tag_name_ == "max_trs_num" )
  {
     max_trs_num = atoi(_content_.c_str());
  }

  if ( _tag_name_ == "phys_log_ext_portion")
  {
    phys_log_ext_portion = atoi(_content_.c_str()) * 0x100000;
  }

  if ( _tag_name_ == "init_phys_log_size")
  {
    phys_log_size = atoi(_content_.c_str()) * 0x100000;
  }


  ((CfgParserContext*)cnt)->tag_name = "";
  ((CfgParserContext*)cnt)->content = "";
  
}



void characterData_sm_cfg(void *cnt, const XML_Char *s, int len)
{

  (((CfgParserContext*)cnt)->content).append(s, len);     
}


void send_stop_sm_msg()
{
    UShMem gov_mem_dsc;
    void* gov_shm_pointer = NULL;
    UPID sm_pid;
    int i=0;
    int port_number;
    int command = STOP;
    
    gov_shm_pointer = open_gov_shm(&gov_mem_dsc);
    port_number = ((gov_header_struct*)gov_shm_pointer)->lstnr_port_number;


    char shm_dbname[SE_MAX_DB_NAME_LENGTH + 1];
    int res;

    for (i=0; i < MAX_DBS_NUMBER; i++)
    {
       strcpy(shm_dbname,
             ((gov_dbs_struct*)((char*)gov_shm_pointer+sizeof(gov_header_struct) + i*sizeof(gov_dbs_struct)))->db_name);

       if (string(shm_dbname) == db_name)
       {
          ((gov_dbs_struct*)((char*)gov_shm_pointer+sizeof(gov_header_struct) + i*sizeof(gov_dbs_struct)))->is_stop = 1;
          send_command_to_gov(port_number, command);
          break;
       }
    }


    close_gov_shm(gov_mem_dsc, gov_shm_pointer);

}

