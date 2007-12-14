/*
 * File:  sm_functions.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <string>
#include "sm/sm_functions.h"
#include "sm/sm_globals.h"
#include "sm/llmgr/llmgr.h"
#include "sm/plmgr/plmgr.h"
#include "sm/bufmgr/bm_core.h"
#include "common/u/usem.h"
#include "common/u/umutex.h"
#include "common/ipc_ops.h"
#include "common/errdbg/exceptions.h"

using namespace std;

/******************************************************************************/
// HANDLERS FOR PARSING CONFIG FILES
/******************************************************************************/

void startElement_sm_cfg(void *cnt, const char *name, const char **atts)
{
 
  (((CfgParserContext*)cnt)->tag_name).assign(name);
  (((CfgParserContext*)cnt)->content).assign("");
}

void endElement_sm_cfg(void *cnt, const char *name)
{

  string _tag_name_ = ((CfgParserContext*)cnt)->tag_name;
  string _content_ = ((CfgParserContext*)cnt)->content;

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

  if ( _tag_name_ == "phys_log_size")
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
    int database_id;
    
    gov_shm_pointer = open_gov_shm(&gov_mem_dsc);
    port_number = ((gov_header_struct*)gov_shm_pointer)->lstnr_port_number;

    database_id = get_db_id_by_name((gov_config_struct*)gov_shm_pointer, db_name);

    ((gov_config_struct*)gov_shm_pointer)->db_vars[database_id].is_stop = 1;
    send_command_to_gov(((gov_config_struct*)gov_shm_pointer)->gov_vars.lstnr_port_number, command);
    
    close_gov_shm(gov_mem_dsc, gov_shm_pointer);
}

static uMutexType giantLockMutex;
static bool isGiantLockInitialized = false;

void InitGiantLock()
{
	if (isGiantLockInitialized)
		throw SYSTEM_EXCEPTION("giant lock already initialised");
	if (uMutexInit(&giantLockMutex,__sys_call_error) != 0)
		throw SYSTEM_EXCEPTION("giant lock mutex not initialised");
	isGiantLockInitialized = true;
}

void DestroyGiantLock()
{
	if (isGiantLockInitialized)
		uMutexDestroy(&giantLockMutex, NULL);
}

void ObtainGiantLock()
{
	if (!isGiantLockInitialized || uMutexLock(&giantLockMutex, __sys_call_error)!=0)
		throw SYSTEM_EXCEPTION("failed to obtain giant lock");
}

void ReleaseGiantLock()
{
	if (!isGiantLockInitialized || uMutexUnlock(&giantLockMutex, __sys_call_error)!=0)
		throw SYSTEM_EXCEPTION("failed to release giant lock");
}

