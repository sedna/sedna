/*
 * File:  rcv_funcs.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _RCV_FUNCS
#define _RCV_FUNCS

#include <vector>

#include "common/sedna.h"

#include "common/base.h"
#include "common/xptr/xptr.h"

void rollback_tr_by_logical_log(transaction_id _trid);
void recover_db_by_logical_log();
void print_value(const char* value, int value_size);

extern int rcv_number_of_records;
#endif
