/*
 * File:  logiclog.cpp - Logical logging on tr
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * Only two functions here. All transaction logging is done via hl_logical_log
 * interface and llLogGeneral function.
 *
 */

#include "tr/log/logiclog.h"
#include "common/llcommon/llMain.h"

void llLogCommit(transaction_id trid)
{
	// don't need to log empty transaction
	if (llInfo->llTransInfoTable[trid].first_lsn == LFS_INVALID_LSN)
		return;

	llLogGeneral(TR_RECORD, trid, LL_COMMIT, false, 0);

	// flush all transaction records
	llFlushTransRecs(trid);
}

void llLogRollback(transaction_id trid)
{
	RECOVERY_CRASH;

    llLogGeneral(TR_RECORD, trid, LL_ROLLBACK, false, 0);
}
