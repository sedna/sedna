/*
 * File:  cdb_globals.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _CDB_GLOBALS_H
#define _CDB_GLOBALS_H

#include "common/sedna.h"
#include "common/u/u.h"
#include "common/argtable.h"
#include "common/base.h"
#include "sm/sm_globals.h"

#include <stdint.h>

#define MAX_ARGS_LENGTH 1000

namespace cdb_globals {
    extern int    data_file_max_size;
    extern int    tmp_file_max_size;
    extern int    data_file_extending_portion;
    extern int    tmp_file_extending_portion;
    extern int    data_file_initial_size;
    extern int    log_file_size;
    extern char   db_security[32];
}


void parse_cdb_command_line(int argc, char** argv);

void setup_cdb_globals(gov_config_struct* cfg);

void create_cfg_file();

void create_data_directory();


#endif /* _CDB_GLOBALS_H */

