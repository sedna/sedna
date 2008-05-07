/*
 * File:  hb_utils.h - contains sm-specific part of the hot-backup process
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _HB_UTILS_H_
#define _HB_UTILS_H_

#include "common/base.h"

// processes start request from hot-backup client
hb_state hbProcessStartRequest(hb_state state);

// processes Archive Logical Log request
hb_state hbProcessLogArchRequest(__int64 *lnumber);

// processes end request
hb_state hbProcessEndRequest();

// processes get-persistent-timestamp request
hb_state hbProcessGetTsRequest(TIMESTAMP *ts);

// processes request for previous log file
hb_state hbProcessGetPrevLogRequest(__int64 *lnumber);

#endif


