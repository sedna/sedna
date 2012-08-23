/*
 * File:  hb_utils.h - contains sm-specific part of the hot-backup process
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _HB_UTILS_H_
#define _HB_UTILS_H_

#include "common/base.h"
#include "common/xptr/sm_vmm_data.h"

// processes start request from hot-backup client
hb_state hbProcessStartRequest(hb_state state, bool is_checkp, hb_state state_incr);

// processes Archive Logical Log request
hb_state hbProcessLogArchRequest(uint64_t *lnumber);

// processes end request
hb_state hbProcessEndRequest();

// processes error request
hb_state hbProcessErrorRequest();

// processes request for previous log file
hb_state hbProcessGetPrevLogRequest(uint64_t *lnumber);

#endif
