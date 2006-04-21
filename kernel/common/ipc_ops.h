/*
 * File:  ipc_ops.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _IPC_H
#define _IPC_H

#include "base.h"
#include "ushm.h"
#include "uutils.h"
#include "upipe.h"



//#include "upipe.h"

// return 0 indicates success
// negative return values indicates an error
int WriteHead(UPIPE p, int *cmd, int *len);

// return 0 indicates success
// negative return values indicates an error
int ReadHead(UPIPE p, int *cmd, int *len);

void* open_gov_shm(UShMem *gov_shm_service_dsc);

int close_gov_shm(UShMem gov_shm_service_dsc, void* gov_shared_mem);

void send_command_to_gov(int port_number, int cmd);


#endif

