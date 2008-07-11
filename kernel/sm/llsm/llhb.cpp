/*
 * File:  llhb.cpp - Logical log hot-backup support routines
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * Implementation of logical log support routines for hot-backup
 *
 */

#include "common/base.h"
#include "sm/llsm/llMain.h"
#include "sm/llsm/lfsStorage.h"

static hb_state hbStatus = HB_END;  // status of hot-backup process
static int64_t hbLastFileNum = -1;  // number of the archive file

// Archives current logical log file
static void llHbArchive()
{
  	llLock();

	hbLastFileNum = llLogArchive();
	hbStatus = HB_END;

  	llUnlock();
}

// Main log hot-backup routine
void llHotBackup(hb_state state)
{
    hbStatus = state;

    if (hbStatus == HB_END)
	    hbLastFileNum = -1;

    if (hbStatus == HB_ARCHIVELOG)
    	llHbArchive();
}

// Returns last archived file number
uint64_t llHbLastArchiveFile()
{
	return hbLastFileNum;
}

// Returns previous archived file number in chain
uint64_t llHbPrevArchivedLog(uint64_t lnumber)
{
	return lfsGetPrevFileNumber(lnumber);
}

// true - if hot-backup is in start stage (physical copying of data file)
bool llHbIsStartStage()
{
	return (hbStatus == HB_START);
}
