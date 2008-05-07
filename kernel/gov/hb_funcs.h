/*
 * File:  hb_funcs.h - gov hot-backup procedures (procesing of requests from hbp)
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _HB_FUNCS_H
#define _HB_FUNCS_H

#include "common/sp.h"

// processes message from hbp
// returns: 0 - all ok, continue; not 0 - send notification to hbp and close connection (HB_ERR or HB_END)
int hbProcessMessage(msg_struct *msg);

// process hbp specific error (connection lost or error in hbp)
// do: send HB_ERROR to sm to end hot-backup process, shutdown and close corresponding socket
void hbProcessErrorHbp();  

#endif
