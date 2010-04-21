/*
 * File:  sm_functions.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _SM_FUNCTIONS_H
#define _SM_FUNCTIONS_H

#include "common/sedna.h"
#include "expat.h"
#include "common/xptr.h"

/* Handlers to parse config files */
void startElement_sm_cfg(void *tag_name, const char *name, const char **atts);
void endElement_sm_cfg(void *tag_name, const char *name);
void characterData_sm_cfg(void *tag_name, const XML_Char *s, int len);

/* Various SM functions */
void start_chekpoint_thread();
void shutdown_chekpoint_thread();

void execute_recovery_by_logical_log_process();

void send_stop_sm_msg();

void InitGiantLock();
void DestroyGiantLock();
void ObtainGiantLock();
void ReleaseGiantLock();

void set_layer_parameters(lsize_t layer_size);

#endif /* _SM_FUNCTIONS_H */
