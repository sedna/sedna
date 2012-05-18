/*
 * File:  gov_globals.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _GOV_GLOBALS_H_
#define _GOV_GLOBALS_H_

#include "aux/argtable2/argtable2.h"


extern info_table *gov_table;

extern int background_mode;

namespace gov_globals
{
    /* TODO: make gov able to listen multiple addresses and define a max ifaces constant */
    struct arg_lit * help;
    struct arg_lit * govVersion;
    struct arg_str * cmdDataDirectory;
    struct arg_str * cmdBindAddress;
                                                   
    struct arg_int * cmdListenPort;
    struct arg_int * cmdLogLevel;
    struct arg_int * cmdKeepAliveTimeout;
    struct arg_int * cmdStackDepth;
    struct arg_int * cmdOsObjectsOffset;
}

void * govArgtable[] =  {gov_globals::help, 
                         gov_globals::govVersion, 
                         gov_globals::cmdDataDirectory, 
                         gov_globals::cmdBindAddress, 
                         gov_globals::cmdListenPort, 
                         gov_globals::cmdLogLevel, 
                         gov_globals::cmdStackDepth, 
                         gov_globals::cmdOsObjectsOffset};

#endif

