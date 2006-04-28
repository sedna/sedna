/*
 * File:  rcv_funcs.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _RCV_FUNCS
#define _RCV_FUNCS

#include "sedna.h"
#include <vector>
#include "base.h"
#include "xptr.h"

void rollback_tr_by_logical_log(transaction_id _trid);
void recover_db_by_logical_log(const LONG_LSN& last_cp_lsn);
void exec_micro_op(const char* rec, int len, bool isUNDO);
void print_value(const char* value, int value_size);

void rcv_allocate_blocks(const std::vector<xptr> &arr);

extern int rcv_number_of_records;
#endif
