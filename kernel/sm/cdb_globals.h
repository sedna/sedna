/*
 * File:  cdb_globals.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _CDB_GLOBALS_H
#define _CDB_GLOBALS_H

#include "common/sedna.h"
#include "common/u/u.h"
#include "common/argtable.h"
#include "common/base.h"

#include <stdint.h>

#define MAX_ARGS_LENGTH 1000

extern int _cdb_s_help_;
extern int _cdb_l_help_;
extern int _cdb_version_;

extern int _data_file_max_size_;
extern int _tmp_file_max_size_;
extern int _data_file_extending_portion_;
extern int _tmp_file_extending_portion_;
extern int _data_file_initial_size_;
extern int _tmp_file_initial_size_;
extern int _persisitent_heap_size_;

extern int _bufs_num_;
extern int _max_trs_num_;
extern double _upd_crt_;

extern char db_security[32];

extern arg_rec cdb_argtable[];
extern const size_t cdb_narg;

void print_cdb_usage();

void setup_cdb_globals(int argc,
                      char** argv,
                      int64_t &data_file_max_size,
                      int64_t &tmp_file_max_size,
                      int &data_file_extending_portion,
                      int &tmp_file_extending_portion,
                      int &data_file_initial_size,
                      int &tmp_file_initial_size,
                      int &persistent_heap_size,
                      uint64_t &log_file_size);

void create_cfg_file(char *db_name,
                     int max_trs_num,
                     int bufs_num,
                     double upd_crt,
                     int max_log_files,
                     int tmp_file_initial_size);

void create_data_directory();


#endif

