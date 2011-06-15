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
extern const int narg;

namespace gov_globals 
{
    extern int  cl_el_level;
    extern char cl_lstnr_addr[U_MAX_FNAME];
    extern int  cl_lstnr_port;
    extern int  cl_ping_port;
    extern int  cl_ka_timeout;
    extern int  cl_pp_stack_depth;
}


#endif

