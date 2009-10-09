/*
 * File:  pq.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __PQ_H
#define __PQ_H

#include <string>

#include "common/sedna.h"

#include "tr/tr_base.h"
#include "tr/xqp/XQuerytoLR.h"
#include "tr/client_core.h"

int process_query_in_scheme (char *str, int step_id);
std::string prepare_module(std::string init_module);
std::string prepare_modules(const std::vector<client_file> &cf_vec, std::string *module_name);

StmntsArray *prepare_stmnt(QueryType type, const char *stmnt);

char *get_module(const char *module_uri);

#endif
