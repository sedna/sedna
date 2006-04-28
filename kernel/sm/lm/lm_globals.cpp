/*
 * File:  lm_globals.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "lock_table.h"
#include "trans_table.h"

#ifdef LOCK_MGR_ON
lock_table lm_table;

trans_table tr_table;
#endif

