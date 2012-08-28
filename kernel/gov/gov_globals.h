/*
 * File:  gov_globals.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _GOV_GLOBALS_H_
#define _GOV_GLOBALS_H_

#include "common/structures/config_data.h"

bool parseSednaOptions(int argc, char** argv, GlobalParameters* sednaGlobalOptions, const char* progname);

#endif

