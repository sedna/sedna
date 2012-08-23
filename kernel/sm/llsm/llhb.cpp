/*
 * File:  llhb.cpp - Logical log hot-backup support routines
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * Implementation of logical log support routines for hot-backup
 *
 */

#include "common/base.h"
#include "common/llcommon/llMain.h"
#include "common/llcommon/lfsStorage.h"
#include "common/xptr/sm_vmm_data.h"

static hb_state hbStatus = HB_END;  // status of hot-backup process
static uint64_t hbLastFileNum = LFS_INVALID_FILE;  // number of the last archived file
static hb_state hbIncrStatus = HB_NONE_INCR;// increment mode, if any

// Archives current logical log file
static void llHbArchive()
{
	if (hbIncrStatus == HB_START_INCR || hbIncrStatus == HB_NONE_INCR) // reset increment status
       	llInfo->next_arch_file = 0;

	hbLastFileNum = llLogArchive();
    U_ASSERT(hbLastFileNum != LFS_INVALID_FILE);
	hbStatus = HB_END;
}

// Main log hot-backup routine
int llHotBackup(hb_state state, hb_state state_incr)
{
    hbStatus = state;

    if (hbStatus == HB_START)
    {
    	hbIncrStatus = state_incr;

    	// check if increment is possible
    	if (state_incr == HB_ADD_INCR && llInfo->next_arch_file == 0)
    		return -1;
    }

    if (hbStatus == HB_END || hbStatus == HB_ERR)
    {
	    if (hbStatus == HB_END)
	    {
	    	if (hbIncrStatus == HB_STOP_INCR || hbIncrStatus == HB_NONE_INCR)
	       		llInfo->next_arch_file = 0;
	    	else
	    		llInfo->next_arch_file = hbLastFileNum + 1;

	  	    llFlushAll(); // we must flush log here because of header changes
	  	}

        hbLastFileNum = LFS_INVALID_FILE;
	    hbIncrStatus = HB_NONE_INCR;
	    hbStatus = HB_END;
	}

    if (hbStatus == HB_ARCHIVELOG)
    	llHbArchive();

    return 0;
}

// Returns last archived file number
uint64_t llHbLastArchiveFile()
{
	return (hbLastFileNum < llInfo->next_arch_file) ? LFS_INVALID_FILE : hbLastFileNum;
}

// Returns previous archived file number in chain
uint64_t llHbPrevArchivedLog(uint64_t lnumber)
{
	// don't archive beyond last archived file
	if (lnumber == llInfo->next_arch_file)
		return LFS_INVALID_FILE;

	return lfsGetPrevFileNumber(lnumber);
}

// true - if hot-backup is in start stage (physical copying of data file)
bool llHbIsNeedAddLogging()
{
	return (hbStatus == HB_START && (hbIncrStatus == HB_START_INCR || hbIncrStatus == HB_NONE_INCR));
}
