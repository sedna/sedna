/*
 * File:  llmgr.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _LLMGR_H
#define _LLMGR_H

#include "base.h"

void ll_logical_log_startup();

void ll_logical_log_shutdown();

LONG_LSN ll_logical_log_checkpoint();

void ll_logical_log_flush();

void ll_logical_log_flush_last_record();


#endif

