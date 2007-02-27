/*
 * File:  gov_globals.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _GOV_GLOBALS_H_
#define _GOV_GLOBALS_H_

#include "common/argtable.h"
#include "gov/gov_table.h"


extern info_table *gov_table;

extern arg_rec gov_argtable[];

extern int background_mode;
extern int gov_help_l;
extern int gov_help_s;
extern int gov_version;
extern const size_t narg;
extern int lstnr_port;
extern ping_port;

#endif

