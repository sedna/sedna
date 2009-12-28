/*
 * File:  rcv_funcs.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */
#include "common/sedna.h"
#include "tr/rcv/logicrcv.h"
#include "tr/structures/indirection.h"
#include "common/errdbg/d_printf.h"
#include "tr/rcv/rcv_test_tr.h"
#include "tr/rcv/logicrcv.h"

void rollback_tr_by_logical_log(transaction_id trid)
{
	switch_to_rollback_mode(MODE_UNDO);
	llLogRollbackTrn(trid);
}

void recover_db_by_logical_log()
{
	switch_to_rollback_mode(MODE_REDO);
#ifdef SE_ENABLE_DTSEARCH
	llLogicalRecover();
	switch_to_rollback_mode(MODE_NORMAL);
	rcvRecoverFtIndexes(); // need to perform remapping of ft
#else
	llLogicalRecover();
#endif

#ifdef TEST_AFTER_RCV
	test_db_after_rcv();
#endif
}

void print_value(const char* value, int value_size)
{
   d_printf1("value=");
   for (int i=0; i<value_size; i++)
     d_printf2("%c", value[i]);

   d_printf1("#\n");
}
