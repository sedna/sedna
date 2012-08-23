/*
 * File:  hb_utils.cpp - contains sm-specific part of the hot-backup process
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "u/usem.h"
#include "sm/trmgr.h"
#include "sm/hb_utils.h"

#include "sm/llsm/llhb.h"

#include "common/llcommon/llMain.h"
#include "common/llcommon/lfsGlobals.h"

static bool hb_in_process = false;

// resets hbp state in case of bad request or at the end
static void ResetHbState(hb_state state)
{
    if (hb_in_process) {
        llHotBackup(state, HB_NONE_INCR);  // notify logical log
        llEnableCheckpoints();             // enable checkpoints
    }

    hb_in_process = false;
}

// processes request from hot-backup client
hb_state hbProcessStartRequest(hb_state state, bool is_checkp, hb_state state_incr)
{
	int res;

	// cannot do hot-backup during recovery process
	if (is_recovery_mode)
		return HB_ERR;

	// if hbp requests checkpoint before hot-backup initiate it
	if (is_checkp)
	{
	 	llActivateCheckpoint();
		llDisableCheckpoints();

    	return HB_WAIT;
    }

	// disable checkpoints
	llDisableCheckpoints();

	// if we are currently making checkpoint ask hbp to wait
	if (llGetCheckpointActiveFlag())
	{
		return HB_WAIT;
	}

    // all ok, checkpoints are disabled, we can continue
    // first, switch logical log to hb mode (special records about blocks to support consistency)
    res = llHotBackup(state, state_incr);

    hb_in_process = true;

    return (res == -1) ? HB_ERR : HB_CONT;
}

// processes Archive Logical Log request
hb_state hbProcessLogArchRequest(uint64_t *lnumber)
{
    // archive log and switch off special records about blocks
    llHotBackup(HB_ARCHIVELOG, HB_NONE_INCR);

    // get last logical log archive number
    *lnumber = llHbLastArchiveFile();

    return HB_CONT;
}

// processes end request
hb_state hbProcessEndRequest()
{
	ResetHbState(HB_END);

    return HB_END;
}

// processes end request
hb_state hbProcessErrorRequest()
{
	ResetHbState(HB_ERR);

    return HB_END;
}

// processes request for previous log file
hb_state hbProcessGetPrevLogRequest(uint64_t *lnumber)
{
    // get prev logical log file number, or -1
    *lnumber = llHbPrevArchivedLog(*lnumber);

    return HB_CONT;
}
