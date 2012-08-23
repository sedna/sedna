/*
 * File:  sm_globals.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>

#include "smtypes.h"

#include "u/usocket.h"
#include "u/uprocess.h"
#include "u/uutils.h"

#include "common/errdbg/d_printf.h"

DatabaseOptions databaseOptions_instance;
DatabaseOptions * databaseOptions = &databaseOptions_instance;
global_name smMessageServerName;

static char token[MAX_TICKET_SIZE] = {};

void
register_sm_on_gov(MessageExchanger * communicator)
{
    int32_t sm_pid;

    sm_pid = uGetCurrentProcessId(__sys_call_error);

    communicator->beginSend(se_ConnectProcess);
    communicator->writeString(token);

    if (0 != communicator->endSend()) {
      throw USER_EXCEPTION2(SE3006, usocket_error_translator());;
    }

/*
    while (!communicator->receive()); //!TODO: !FIXME: we need some kind of timeout here. Now it's infinite loop

    if(communicator->getInstruction() == se_SMRegisteringFailed) {
        throw USER_EXCEPTION(SE3045);
    }
*/
}

void
unregister_sm_on_gov(MessageExchanger * communicator)
{
/*  
    communicator->beginSend(se_UnRegisterDB);
    communicator->writeString(db_name);
    communicator->writeInt32(db_id);

    if (0 != communicator->endSend()) {
      throw USER_EXCEPTION2(SE3006,usocket_error_translator());;
    }
    
    while(!communicator->receive()); //wait for gov confirms closure
*/
}

/*
void
register_cdb_on_gov(MessageExchanger * communicator)
{
    int32_t sm_pid;

    sm_id = uGetCurrentProcessId(__sys_call_error);

    communicator->beginSend(se_RegisterDB);
    communicator->writeString(db_name);
    if (0 != communicator->endSend()) {
      throw USER_EXCEPTION2(SE3006,usocket_error_translator());;
    }
    
    // Important note: in this moment cdb receives it's options.
    while (!communicator->receive()); //!TODO: !FIXME: we need some kind of timeout here. Now it's infinite loop
    if(communicator->getInstruction() == se_CdbRegisteringFailed) {
        throw USER_EXCEPTION(SE3045);
    }
}
*/
