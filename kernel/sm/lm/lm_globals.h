/*
 * File:  lm_globals.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _LM_GLOBALS_H_
#define _LM_GLOBALS_H_

#include "common/sedna.h"
#include "common/lockmantypes.h"

#include "sm/lm/lock_table.h"
#include "sm/lm/trans_table.h"

extern lock_table lm_table;
extern trans_table tr_table;

#endif
