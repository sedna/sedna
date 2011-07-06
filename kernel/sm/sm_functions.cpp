/*
 * File:  sm_functions.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <string>
#include "sm/sm_functions.h"
#include "sm/sm_globals.h"
#include "sm/bufmgr/bm_core.h"
#include "sm/bufmgr/blk_mngmt.h"
#include "common/u/umutex.h"
#include "common/ipc_ops.h"
#include "common/errdbg/exceptions.h"

using namespace std;

/******************************************************************************/
// HANDLERS FOR PARSING CONFIG FILES
/******************************************************************************/

struct CfgParserContext
{
   std::string tag_name;
   std::string content;
};


void startElement_sm_cfg(void *cnt, const char *name, const char **atts)
{

  (((CfgParserContext*)cnt)->tag_name).assign(name);
  (((CfgParserContext*)cnt)->content).assign("");
}

void endElement_sm_cfg(void *cnt, const char *name)
{

  string _tag_name_ = ((CfgParserContext*)cnt)->tag_name;
  string _content_ = ((CfgParserContext*)cnt)->content;

  if ( _tag_name_ == "bufs_num")  {
     sm_globals::bufs_num = atoi(_content_.c_str());
  }

  if ( _tag_name_ == "upd_crt" )  {
     sm_globals::upd_crt = atof(_content_.c_str());
  }

  if ( _tag_name_ == "max_log_files" )  {
     sm_globals::max_log_files = atoi(_content_.c_str());
  }

  if ( _tag_name_ == "tmp_file_initial_size" ) {
     sm_globals::tmp_file_initial_size = atoi(_content_.c_str());
  }

  ((CfgParserContext*)cnt)->tag_name = "";
  ((CfgParserContext*)cnt)->content = "";

}



void characterData_sm_cfg(void *cnt, const XML_Char *s, int len)
{

  (((CfgParserContext*)cnt)->content).append(s, len);
}


void
send_stop_sm_msg()
{
    int port_number = GOV_HEADER_GLOBAL_PTR->lstnr_port_number;
    char gov_address[U_MAX_HOSTNAME];
    strcpy(gov_address, GOV_HEADER_GLOBAL_PTR -> lstnr_addr);
    int database_id = get_db_id_by_name(GOV_CONFIG_GLOBAL_PTR, sm_globals::db_name);

    GOV_CONFIG_GLOBAL_PTR -> db_vars[database_id].mode = OM_SM_SHUTDOWN;
    send_command_to_gov(port_number, gov_address, STOP);
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

void set_layer_parameters(lsize_t layer_size)
{
    LAYER_ADDRESS_SPACE_SIZE = layer_size;
}

void recreate_tmp_file()
{
    // truncate tmp file up to zero size
    if (uSetEndOfFile(tmp_file_handler, 0, U_FILE_BEGIN, __sys_call_error) == 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot truncate tmp file");

    // update master block
    mb->tmp_file_cur_size = 0;
    mb->free_tmp_blocks = XNULL;

    extend_tmp_file((int)MBS2PAGES(sm_globals::tmp_file_initial_size));
}
