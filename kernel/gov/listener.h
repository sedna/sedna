/*
 * File:  listener.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _LST_H
#define _LST_H

#include "usocket.h"

int sess_registering(USOCKET s, char* msg_buf);
int sm_registering(USOCKET s, char* msg_buf);
int client_listener(bool is_background_mode);
void CreateNewSessionProcess(USOCKET socknew, bool is_background_mode);
void send_runtime_config(USOCKET s);
void check_sm_run(USOCKET s, char* msg_buf);

#endif
