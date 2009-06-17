/*
 * File:  logican.h -  - Analysis of logical log
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * Only the user interface is specified here. For further information refer to logican.cpp file.
 *
 */

#ifndef _LL_LOGICAL_ANALYS_
#define _LL_LOGICAL_ANALYS_

// type of transactions cell
enum trn_analysis_enum 
{
	TRN_NOT_FINISHED,         // state is uknown
	TRN_ROLLBACK_FINISHED,    // rolled back
	TRN_COMMIT_FINISHED,      // committed
};

// Cell of the analysis list
struct trn_cell_analysis_redo
{
  transaction_id trid;               // transaction identifier
  
  LSN first_lsn;                     // starting lsn of transaction
  LSN end_lsn;                       // ending lsn of transaction
  
  trn_analysis_enum finish_status;   // status of transaction

  trn_cell_analysis_redo *next;      
  trn_cell_analysis_redo *prev;
};

// Returns list after analyzing records starting from start_lsn to the end of the log
// Parameters:
// 		start_lsn - start lsn for analysis
// Returns:
// 		root of the list or NULL in case of empty list
trn_cell_analysis_redo *llGetRedoList(LSN start_lsn);

// Destroys list
// Parameters:
// 		rcv_list - root of the list (returned by llGetRedoList)
// Returns:
// 		Nothing	
void llDestroyRedoList(trn_cell_analysis_redo *rcv_list);

// Returns cell in the list by specified parameters.
// Parameters:
// 		redo_list - root of the list
//		trid - transaction identifier
//		lsn - lsn to find (for returned cell: first_lsn <= lsn <= end_lsn)
// Returns:
//		cell or NULL if nothing is found
trn_cell_analysis_redo *llFindTrnCell(trn_cell_analysis_redo *redo_list, transaction_id trid, LSN lsn);

// Returns high lsn watermark for redo process
// Parameters:
//      redo_lst - root of the redo list (must contain only committed transactions!)
// Returns:
//      high watermark lsn
LSN llGetHighRcvLSN(trn_cell_analysis_redo *redo_lst);

#endif
