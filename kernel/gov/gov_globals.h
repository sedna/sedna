/*
 * File:  gov_globals.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _GOV_GLOBALS_H_
#define _GOV_GLOBALS_H_

#include "aux/argtable2/argtable2.h"


extern info_table *gov_table;

extern int background_mode;
extern int gov_help_l;
extern int gov_help_s;
extern int gov_version;

namespace gov_globals
{
    /* TODO: make gov able to listen multiple addresses and define a max ifaces constant */
    struct arg_lit * help;
    struct arg_str * cl_data_dir;
    struct arg_str * cl_lstnr_addr;
                                                   
    struct arg_int * cl_lstnr_port;
    struct arg_int * cl_el_level;
    struct arg_int * cl_ka_timeout;
    struct arg_int * cl_pp_stack_depth;
}

void * gov_argtable[] = {gov_globals::cl_install_dir, gov_globals::cl_lstnr_addr, gov_globals::cl_lstnr_port, gov_globals::cl_el_level,
                         gov_globals::cl_ka_timeout, gov_globals::cl_pp_stack_depth};

#endif

