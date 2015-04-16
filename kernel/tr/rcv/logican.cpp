/*
 * File:  logican.cpp - Analysis of logical log
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * This module provides analysis routines for handling transaction list. 
 *
 * To change behaviour of anaysis change llRcvAnalyzeRecs info for specified record and llGetNextRcvRec to change
 * scan behaviour (sequential scan by default).
 *
 */

#include "common/sedna.h"
#include "common/llcommon/llMain.h"
#include "tr/rcv/logican.h"

static trn_cell_analysis_redo *rcv_list = NULL;

static LSN highLSNBoundary = 0; // higher boundary for analysis

static LSN llGetNextRcvRec(LSN curr_lsn, void *RecBuf)
{
	LSN lsn = curr_lsn + llGetRecordSize(RecBuf, 0);

    // we shouldn't walk beyond high boundary
    if (lsn >= highLSNBoundary) return LFS_INVALID_LSN;

    // we don't need to check lsn validity since lfsGetRecord in llScan will do it for us
    return lsn;
}

trn_cell_analysis_redo *llFindTrnCell(trn_cell_analysis_redo *redo_list, transaction_id trid, LSN lsn)
{
	trn_cell_analysis_redo *it = redo_list;

	while (it != NULL)
	{
		if (it->trid == trid && it->first_lsn <= lsn && it->end_lsn >= lsn)
			return it;

		it = it->next;
	}

	return NULL;
}   

static
trn_cell_analysis_redo *find_last_redo_trn_cell(trn_cell_analysis_redo *redo_list, transaction_id trid)
{
	trn_cell_analysis_redo *it = redo_list;

	// empty list
	if (it == NULL)
		return NULL;

	// find last element
	while (it->next != NULL)
		it = it->next;

	while (it != NULL)
	{
		if (it->trid == trid)
			return it;

		it = it->prev;
	}

	return NULL;
}

static
trn_cell_analysis_redo *put_redo_trn_cell(trn_cell_analysis_redo *rcv_redo_list, transaction_id trid, LSN first_lsn)
{
	trn_cell_analysis_redo *it = rcv_redo_list, *new_cell;

	if ((new_cell = (trn_cell_analysis_redo *)malloc(sizeof(trn_cell_analysis_redo))) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory!");

	new_cell->trid = trid;
	new_cell->first_lsn = first_lsn;
	new_cell->end_lsn = LFS_INVALID_LSN;
	new_cell->finish_status = TRN_NOT_FINISHED;
	new_cell->prev = NULL;
	new_cell->next = NULL;

	// empty list
	if (rcv_redo_list == NULL)
	{
		rcv_redo_list = new_cell;
		return new_cell;
	}
	
	// find last element
	while (it->next != NULL)
		it = it->next;
	
	new_cell->prev = it;
	it->next = new_cell;

	return new_cell;
}

static void llRcvAnalyzeCommitRec(LSN lsn, void *RecBuf)
{
	transaction_id trid = *((transaction_id *)((char *)RecBuf + sizeof(char)));
    trn_cell_analysis_redo *redo_trn_cell;

	redo_trn_cell = find_last_redo_trn_cell(rcv_list, trid);

	if (redo_trn_cell != NULL && redo_trn_cell->finish_status == TRN_NOT_FINISHED)
    {
		redo_trn_cell->finish_status = TRN_COMMIT_FINISHED;
		redo_trn_cell->end_lsn = lsn;
    }
}

static void llRcvAnalyzeRollbackRec(LSN lsn, void *RecBuf)
{
	transaction_id trid = *((transaction_id *)((char *)RecBuf + sizeof(char)));
    trn_cell_analysis_redo *redo_trn_cell;

	redo_trn_cell = find_last_redo_trn_cell(rcv_list, trid);

	if (redo_trn_cell != NULL && redo_trn_cell->finish_status == TRN_NOT_FINISHED)
    {
		redo_trn_cell->finish_status = TRN_ROLLBACK_FINISHED;
		redo_trn_cell->end_lsn = lsn;
    }
}

static void llRcvAnalyzeLogicalRec(LSN lsn, void *RecBuf)
{
	transaction_id trid = *((transaction_id *)((char *)RecBuf + sizeof(char)));
    trn_cell_analysis_redo *redo_trn_cell;

	redo_trn_cell = find_last_redo_trn_cell(rcv_list, trid);

	if (redo_trn_cell == NULL || redo_trn_cell->finish_status != TRN_NOT_FINISHED)
	{
		redo_trn_cell = put_redo_trn_cell(rcv_list, trid, lsn);
		if (rcv_list == NULL)
			rcv_list = redo_trn_cell;
	}
}

void llDestroyRedoList(trn_cell_analysis_redo *rcv_list)
{
	trn_cell_analysis_redo *it = rcv_list, *pit;

	while (it != NULL)
	{
		pit = it;
		it = it->next;
		free(pit);
	}
}

// Main structure to determine analysis behaviour. For each type record the corresponding function is specified
// See llScanRecords in llMain.h for details.
static
struct llRecInfo llRcvAnalyzeRecs[] =
{
	{LL_INSERT_ELEM, llRcvAnalyzeLogicalRec},
	{LL_DELETE_ELEM, llRcvAnalyzeLogicalRec},
	{LL_INSERT_ATTR, llRcvAnalyzeLogicalRec},
	{LL_DELETE_ATTR, llRcvAnalyzeLogicalRec},
	{LL_INSERT_TEXT, llRcvAnalyzeLogicalRec},
	{LL_DELETE_TEXT, llRcvAnalyzeLogicalRec},
	{LL_INSERT_LEFT_TEXT, llRcvAnalyzeLogicalRec},
	{LL_DELETE_LEFT_TEXT, llRcvAnalyzeLogicalRec},
	{LL_INSERT_RIGHT_TEXT, llRcvAnalyzeLogicalRec},
	{LL_DELETE_RIGHT_TEXT, llRcvAnalyzeLogicalRec},
	{LL_INSERT_DOC, llRcvAnalyzeLogicalRec},
	{LL_DELETE_DOC, llRcvAnalyzeLogicalRec},
	{LL_INSERT_COMMENT, llRcvAnalyzeLogicalRec},
	{LL_DELETE_COMMENT, llRcvAnalyzeLogicalRec},
	{LL_INSERT_PI, llRcvAnalyzeLogicalRec},
	{LL_DELETE_PI, llRcvAnalyzeLogicalRec},
	{LL_INSERT_COLLECTION, llRcvAnalyzeLogicalRec},
	{LL_DELETE_COLLECTION, llRcvAnalyzeLogicalRec},
	{LL_INSERT_NS, llRcvAnalyzeLogicalRec},
	{LL_DELETE_NS, llRcvAnalyzeLogicalRec},
	{LL_INSERT_DOC_INDEX, llRcvAnalyzeLogicalRec},
	{LL_DELETE_DOC_INDEX, llRcvAnalyzeLogicalRec},
	{LL_INSERT_COL_INDEX, llRcvAnalyzeLogicalRec},
	{LL_DELETE_COL_INDEX, llRcvAnalyzeLogicalRec},
	{LL_INSERT_DOC_FTS_INDEX, llRcvAnalyzeLogicalRec},
	{LL_DELETE_DOC_FTS_INDEX, llRcvAnalyzeLogicalRec},
	{LL_INSERT_COL_FTS_INDEX, llRcvAnalyzeLogicalRec},
	{LL_DELETE_COL_FTS_INDEX, llRcvAnalyzeLogicalRec},
	{LL_INSERT_DOC_TRG, llRcvAnalyzeLogicalRec},
	{LL_DELETE_DOC_TRG, llRcvAnalyzeLogicalRec},
	{LL_INSERT_COL_TRG, llRcvAnalyzeLogicalRec},
	{LL_DELETE_COL_TRG, llRcvAnalyzeLogicalRec},
	{LL_RENAME_COLLECTION, llRcvAnalyzeLogicalRec},
	{LL_COMMIT, llRcvAnalyzeCommitRec},
	{LL_ROLLBACK, llRcvAnalyzeRollbackRec},
};

// length of the aforementioned table
static int llRcvAnalyzeRecsLen = sizeof(llRcvAnalyzeRecs) / sizeof(llRecInfo);

trn_cell_analysis_redo *llGetRedoList(LSN start_lsn)
{
	rcv_list = NULL;
    trn_cell_analysis_redo *it, *pit;

    highLSNBoundary = llGetHighLSNBoundary();

    llScanRecords(llRcvAnalyzeRecs, llRcvAnalyzeRecsLen, start_lsn, llGetNextRcvRec, NULL);

	// here we've got rcv_list, but we need to filter out rolled back and unfinished transactions
	it = rcv_list;
    	
	while (it != NULL)
	{
		pit = it;
		it = it->next;

		if (pit->finish_status != TRN_COMMIT_FINISHED) // remove unneeded cell
		{
			if (pit->prev != NULL)
				pit->prev->next = pit->next;
			
			if (pit->next != NULL)
				pit->next->prev = pit->prev;

			if (pit == rcv_list)
				rcv_list = pit->next;

			free(pit);
	    }
	}

	return rcv_list;
}

// Returns high lsn watermark for redo process
LSN llGetHighRcvLSN(trn_cell_analysis_redo *redo_lst)
{
    trn_cell_analysis_redo *it = redo_lst;
    LSN endLSN = 0;

    while (it != NULL)
    {
        U_ASSERT(it->finish_status == TRN_COMMIT_FINISHED);

        if (it->end_lsn > endLSN) endLSN = it->end_lsn;

        it = it->next;
    }

    return  endLSN;
}
