/*
 * File:  pq.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __PQ_H
#define __PQ_H


#include <string>
#include "base.h"
#include "XQuerytoLR.h"

int process_query_in_scheme (char *str, int step_id);


StmntsArray *prepare_stmnt(QueryType type, const char *stmnt);


#endif

