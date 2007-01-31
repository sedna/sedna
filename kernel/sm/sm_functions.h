/*
 * File:  sm_functions.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _SM_FUNCTIONS_H
#define _SM_FUNCTIONS_H

#include "common/sedna.h"
#include "expat/expat.h"

/******************************************************************************/
//HANDLERS FOR PARSING CONFIG FILES
/******************************************************************************/

void startElement_sm_cfg(void *tag_name, const char *name, const char **atts);

void endElement_sm_cfg(void *tag_name, const char *name);

void characterData_sm_cfg(void *tag_name, const XML_Char *s, int len);

void start_chekpoint_thread();
void shutdown_chekpoint_thread();

void execute_recovery_by_logical_log_process();

void send_stop_sm_msg();

#endif
