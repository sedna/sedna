/*
 * File:  gov_globals.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <map>
#include <list>
#include <string>
#include <iostream>
#include "base.h"
#include "uprocess.h"
#include "exceptions.h"
#include "gov_globals.h"
#include "gov_table.h"
#include "utils.h"
#include "argtable.h"
#include "d_printf.h"


using namespace std;


info_table *gov_table;


int background_mode = 0;
int gov_help_s = 0;
int gov_help_l = 0;
int gov_version = 0;
int socket_port = 0;

const size_t narg = 5;


arg_rec gov_argtable[] =
{
{"--help",            NULL,        arg_lit,  &gov_help_l,                  "0",   "\t\t   display this help and exit"},
{"-help",             NULL,        arg_lit,  &gov_help_s,                  "0",   "\t\t           display this help and exit"},
{"-version",          NULL,        arg_lit,  &gov_version,                 "0",   "\t\t   display product version and exit"},
{"-background-mode", " on/off",   arg_bool, &background_mode,              "on",  "  start the server in the background mode (default on)"},
{"-port",             " number",    arg_int,  &socket_port,                 "5050","\t   socket listening port\n\t\t\t   (default 5050)"}
};

