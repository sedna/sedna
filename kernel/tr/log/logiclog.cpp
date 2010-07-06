/*
 * File:  logiclog.cpp - Logical logging on tr
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * This functions should be used to write logical information in logical log.
 *
 */

#include "tr/log/logiclog.h"
#include "common/llcommon/llMain.h"

// mem copy with offset increment
inline
static
void inc_mem_copy(void* dest, size_t &offs, const void* src, size_t len)
{
	if (len == 0) return;
	memcpy((char*)dest + offs, src, len);
	offs += len;
}

/*
 * Temporary record buffer; it is expanded if needed.
 *
 * Initial size of intermediate record buffer is REC_BUF_INIT_SIZE bytes
 */
#define REC_BUF_INIT_SIZE 128
static unsigned char* rec_buf = NULL;
static size_t rec_buf_size = 0;

void llLogGeneral(transaction_id trid, llOperations op, unsigned num, ...)
{
    va_list args;
    void *field = &op;
    size_t max_len, field_len, rec_len = 0;
    char cop = (char)op;

    RECOVERY_CRASH;

    if (rollback_active || recovery_active) return;

    max_len = llGetMaxRecordSize();

    U_ASSERT(max_len >= sizeof(cop) + sizeof(trid));

    if (!rec_buf)
    {
        rec_buf = (unsigned char *)malloc(REC_BUF_INIT_SIZE);
        if (!rec_buf)
            throw SYSTEM_EXCEPTION("logical log: not enough memory");

        rec_buf_size = REC_BUF_INIT_SIZE;
    }

    inc_mem_copy(rec_buf, rec_len, &cop, sizeof(cop));
    inc_mem_copy(rec_buf, rec_len, &trid, sizeof(trid));

    va_start(args, num);

    for (unsigned i = 0; i < num; i++)
    {
        field = va_arg(args, void *);
        field_len = va_arg(args, size_t);

        if (rec_len + field_len > max_len)
            throw USER_EXCEPTION(SE4156);

        if (rec_len + field_len > rec_buf_size)
        {
            rec_buf_size = s_max(rec_buf_size * 2, rec_len + field_len);
            rec_buf = (unsigned char *)realloc(rec_buf, rec_buf_size);

            if (!rec_buf)
                throw SYSTEM_EXCEPTION("logical log: not enough memory");
        }

        inc_mem_copy(rec_buf, rec_len, field, field_len);
    }

    //insert record
    if (llInsertRecord(rec_buf, rec_len, trid) == LFS_INVALID_LSN)
        throw SYSTEM_EXCEPTION("logical log: internal transaction error");

    va_end(args);
}

void llLogCommit(transaction_id trid)
{
	// don't need to log empty transaction
	if (llInfo->llTransInfoTable[trid].first_lsn == LFS_INVALID_LSN)
		return;

	llLogGeneral(trid, LL_COMMIT, 0);

	// flush all transaction records
	llFlushTransRecs(trid);
}

void llLogRollback(transaction_id trid)
{
	RECOVERY_CRASH;

    llLogGeneral(trid, LL_ROLLBACK, 0);
}
