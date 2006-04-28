/*
 * File: db_utils.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _DB_UTILS_H
#define _DB_UTILS_H

#include "sedna.h"

int cleanup_db(const char* db_name);

bool exist_db(const char* db_name);

int load_metadata_in_database(const char* db_name);
#endif
