/*
 * File: db_utils.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _DB_UTILS_H
#define _DB_UTILS_H

#include "common/sedna.h"
#include "common/config.h"

int cleanup_db(const char* db_name);

bool exist_db(const char* db_name);

int load_metadata_in_database(const char* db_name, const char* db_security_level, const gov_header_struct& cfg);

int delete_logical_log(const char* db_name);

#ifdef SE_ENABLE_FTSEARCH
int delete_dtsearch_files(const char* db_name);
#endif

int delete_ph_files(const char* db_name);

#endif
