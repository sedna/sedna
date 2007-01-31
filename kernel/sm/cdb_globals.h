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

extern arg_rec cdb_argtable[];
extern const size_t cdb_narg;

void print_cdb_usage();

void setup_cdb_globals(int, char**, __int64 &, __int64 &, int &, int &, int &, int &, int &, int &, int &);

void create_cfg_file(char *, int, int, int, int);

void create_data_directory();


#endif

