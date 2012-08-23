/*
 * File:  llhb.h - Logical log hot-backup support routines
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * Only the user interface is specified here. For further information refer to physrcv.cpp file.
 *
 */

#ifndef _LL_HB_LOG_
#define _LL_HB_LOG_

#include "common/xptr/sm_vmm_data.h"

// Main log hot-backup routine
// Returns:
// 		-1 - error; 0 - all ok;
int llHotBackup(hb_state state, hb_state state_incr);

// Returns last archived file number
uint64_t llHbLastArchiveFile();

// Returns previous archived file number in chain
uint64_t llHbPrevArchivedLog(uint64_t lnumber);

// true - if additional logging is needed (physical copying of data file)
bool llHbIsNeedAddLogging();

#endif
