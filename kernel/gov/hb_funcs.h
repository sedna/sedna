/*
 * File:  hb_funcs.h - gov hot-backup procedures (procesing of requests from hbp)
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _HB_FUNCS_H
#define _HB_FUNCS_H

#include "common/protocol/sp.h"
#include "u/usocket.h"
#include "common/xptr/sm_vmm_data.h"

// init new hot-backup connection
int hbNewClient(USOCKET sock);

// processes message from hbp
// returns: 0 - all ok, continue; not 0 - client detached (-1 - error; 1 - usual end)
int hbProcessMessage(USOCKET sock);

#endif
