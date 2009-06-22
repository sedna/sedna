/*
 * File:  listener.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _LST_H
#define _LST_H

#include "common/u/usocket.h"
#include "common/config.h"

typedef int (*clProcess_fun)(USOCKET sock);

struct clClient 
{
	USOCKET sock;
	clProcess_fun clProcess;       // process function (called on receive)
};

void set_session_common_environment();
int sess_registering(USOCKET s, char* msg_buf);
int sm_registering(USOCKET s, char* msg_buf);
int client_listener(gov_config_struct* cfg, bool is_background_mode);
void CreateNewSessionProcess(USOCKET socknew, bool is_background_mode);
void send_runtime_config(USOCKET s);
void check_sm_run(USOCKET s, char* msg_buf);

#endif
