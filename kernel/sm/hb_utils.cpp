/*
 * File:  hb_utils.cpp - contains sm-specific part of the hot-backup process
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/u/usem.h"
#include "sm/trmgr.h"
#include "sm/hb_utils.h"
#include "sm/llmgr/llmgr.h"

static bool hb_in_process = false;

// resets hbp state in case of bad request or at the end
static void ResetHbState()
{
	    if (hb_in_process)
	    { 
	    	ll_hotbackup(HB_END);             // notify logical log
			ll_log_set_checkpoint_flag(true); // enable checkpoints
		}
		
		hb_in_process = false;
}

// processes request from hot-backup client
hb_state hbProcessStartRequest(hb_state state)
{
	// cannot do hot-backup during recovery process
	if (is_recovery_mode)
		return HB_ERR;

	// disable checkpoints
	ll_log_set_checkpoint_flag(false);

	// if we are currently making checkpoint ask hbp to wait
	if (ll_log_get_checkpoint_on_flag())
	{
		return HB_WAIT;
	}

	// if hbp requests checkpoint before hot-backup initiate it
	if (state == HB_START_CHECKPOINT)
	{
	 	ll_set_checkpoint_on_flag(true);

	 	if (UEventSet(&start_checkpoint_snapshot,  __sys_call_error) != 0)
    		throw SYSTEM_EXCEPTION("Event signaling for checkpoint on hot-backup failed");

    	return HB_WAIT;
    }

    // all ok, checkpoints are disabled, we can continue
    // first, switch logical log to hb mode (special records about blocks to support consistency)
    ll_hotbackup(HB_START);

    hb_in_process = true;

    return HB_CONT;
}

// processes Archive Logical Log request
hb_state hbProcessLogArchRequest(__int64 *lnumber)
{
    // archive log and switch off special records about blocks
    ll_hotbackup(HB_ARCHIVELOG);

    // get last logical log archive number
    *lnumber = ll_get_last_archived_log_file_number();

    return HB_CONT;
}

// processes end request
hb_state hbProcessEndRequest()
{
	ResetHbState();

    return HB_END;
}

// processes get-persistent-timestamp request
hb_state hbProcessGetTsRequest(TIMESTAMP *ts)
{
    // retrieve timestamp of persistent snapshot
    *ts = ll_returnTimestampOfPersSnapshot();

	return HB_CONT;
}

// processes request for previous log file
hb_state hbProcessGetPrevLogRequest(__int64 *lnumber)
{
    // get prev logical log file number, or -1 
    *lnumber = ll_get_prev_archived_log_file_number(*lnumber);

    return HB_CONT;
}
