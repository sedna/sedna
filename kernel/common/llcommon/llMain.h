/*
 * File:  llMain.h - Main part of logical log.
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * Only the user interface is specified here. For further information refer to lfsClient.cpp file.
 *
 */

#ifndef _LL_MAIN_
#define _LL_MAIN_

#include "common/base.h"
#include "common/llcommon/lfsGlobals.h"
#include "common/xptr/wutypes.h"

//#define LOG_TRACE // turns on extensive logical log trace in event log

// Main logical log structures.
enum llOperations
{
	// Logical records start here
	LL_INSERT_ELEM,
	LL_DELETE_ELEM,
	LL_INSERT_ATTR,
	LL_DELETE_ATTR,
	LL_INSERT_TEXT,
	LL_DELETE_TEXT,
	LL_INSERT_LEFT_TEXT,
	LL_DELETE_LEFT_TEXT,
	LL_INSERT_RIGHT_TEXT,
	LL_DELETE_RIGHT_TEXT,
	LL_INSERT_DOC,
	LL_DELETE_DOC,
	LL_INSERT_COMMENT,
	LL_DELETE_COMMENT,
	LL_INSERT_PI,
	LL_DELETE_PI,
	LL_INSERT_COLLECTION,
	LL_DELETE_COLLECTION,
	LL_INSERT_NS,
	LL_DELETE_NS,
	LL_INSERT_DOC_INDEX,
	LL_DELETE_DOC_INDEX,
	LL_INSERT_COL_INDEX,
	LL_DELETE_COL_INDEX,
	LL_INSERT_DOC_FTS_INDEX,
	LL_DELETE_DOC_FTS_INDEX,
	LL_INSERT_COL_FTS_INDEX,
	LL_DELETE_COL_FTS_INDEX,
	LL_INSERT_DOC_TRG,
	LL_DELETE_DOC_TRG,
	LL_INSERT_COL_TRG,
	LL_DELETE_COL_TRG,
	LL_RENAME_COLLECTION,
	LL_COMMIT,
	LL_ROLLBACK,
	// Physical records start here
	LL_CHECKPOINT,
	LL_FREE_BLOCKS,       // info about free blocks
	LL_PERS_SNAPSHOT_ADD, // additional info about persistent snapshot
	LL_DECREASE,          // decrease_info from physical log
	LL_HBBLOCK,           // info about block during hot-backup procedure
	LL_HBINFO,            // hb-specific info (used only in hot-backup recovery)

	LL_DEFAULT,           // bogus operation (see description of the llScanRecords)
};

/*
 * Note on record types: TR_RECORDs will be retrieved on logical recovery phase,
 * PHYS_RECORDs will be retrieved on physical recovery phase, GEN_RECORDs will
 * not be automatically retrieved; if you store such records you should retrieve
 * it yourself by the given LSN number. One example of such record is hot-backup
 * record.
 */
enum llRecordType
{
    TR_RECORD,   /* all such records are linked together in transaction-chain */
    PHYS_RECORD, /* these records are linked together in the physical chain */
    GEN_RECORD,  /* stored, but not automatically retrieved */
};

enum llTransMode
{
	NORMAL_MODE,     // transaction is doing some usual work
	ROLLBACK_MODE    // transaction is being rolled back
};

struct llTransInfo
{
	LSN last_lsn;                 // last lsn of the given transaction  
	LSN first_lsn;                // lsn of first transaction's record in log
	llTransMode mode;             // indicates transaction mode
	int num_of_log_records;       // used for debug
	int last_len;                 // length of the last record
};

// info about logical log shared between processes
struct llGlobalInfo
{
	LSN checkpoint_lsn;                                     // lsn of the last checkpoint record (first of the bunch)
	LSN min_rcv_lsn;                                        // lsn of the start record of logical recovery
	LSN last_chain_lsn;                                     // lsn of the last record in physical chain
	TIMESTAMP ts;                                           // timestamp of the last persistent snapshot
	llTransInfo llTransInfoTable[CHARISMA_MAX_TRNS_NUMBER]; // transaction table
	bool checkpoint_flag;                                   // true, if checkpoint is enabled
	bool checkpoint_on;                                     // true, if checkpoint is currently in progress
	bool hotbackup_needed;                                  // recover from hotbackup copy needed
	uint64_t next_arch_file;       							// file from which to archive on next increment; if 0 - then db isn't in incremental mode
};

static global_name logicalLogShm = "logical_log_shm";
static global_name logicalLogSem = "logical_log_sem";

static global_name checkpointFinishedSemName = "checkpoint_finished_sem";
static global_name checkpointEventName = "checkpoint_finished_event";

extern llGlobalInfo *llInfo; // pointer to the global info memory

extern int rollback_active; // true, if rollback is active on current transaction
extern int recovery_active; // true, if this process is a recovery process

// Main functions

// Create new logical log.
// Parameters:
//     db_files_path - full path where files are stored;
//     db_name   - name of the database;
//     log_file_size - maximum size of one log file
// Returns: 
//     -1 - error; 0 - all ok
int llCreateNew(const char *db_files_path, const char *db_name, uint64_t log_file_size);

// Insert record in logical log. 
// Data can be stored in memory. To guarantee write on disk, llFlush should be used.
// Parameters:
//     rtype - type of record
//     RecBuf  - allocated buffer to write a record
//     RecLen - size of a record
//     trid - transaction identifier;
// Returns: 
//     LSN of the written record;
//     LFS_INVALID_LSN in case of error;
LSN llInsertRecord(llRecordType rtype, const void* RecBuf, unsigned int RecLen,
        transaction_id trid);

// Locks logical log (global ll synchronization)
void llLock();

// Unlocks logical log (global ll synchronization)
void llUnlock();

// Inits logical log.
// Parameters:
//     db_files_path - full path where files are stored;
//     db_name - name of the database;
//     max_log_files_param - maximum number of log files until commit
//	   sedna_db_version - (out) sedna data structures
//     exit_status - (out) status of previous exit (true - log was shutdowned successfully; false - abnormal termination).
//     rcv_active - if recovery is active
// Returns: 
//     -1 - error; 0 - all ok
int llInit(const char *db_files_path, const char * db_name, int max_log_files_param, 
	   int *sedna_db_version, bool *exit_status, int rcv_active);

// Releases logical log.
// Returns: 
//     -1 - error; 0 - all ok
int llRelease();

// Opens logical log (must be inited by other thread by llInit first). Should be called on transaction process.
// Parameters:
//     db_files_path - full path where files are stored;
//     db_name - name of the database;
//     rcv_active - is recovery active?
// Returns: 
//     -1 - error; 0 - all ok
int llOpen(const char *db_files_path, const char *db_name_, bool rcv_active);

// Closes logical log (pair-function for open).
// Returns:
//     -1 - error; 0 - all ok
int llClose();

// Should be called for every new transaction.
// Parameters:
//	   trid - transaction identifier
// Returns: 
//     -1 - error; 0 - all ok
int llOnTransBegin(transaction_id trid);

// Should be called for every rolled back or committed transaction.
// Parameters:
//	   trid - transaction identifier
// Returns: 
//     -1 - error; 0 - all ok
int llOnTransEnd(transaction_id trid);

// Flushes records belonging to the specified transaction.
// Parameters:
//	   trid - transaction identifier
// Returns: 
//     -1 - error; 0 - all ok
int llFlushTransRecs(transaction_id trid);

// Flushes all records.
// Returns: 
//     -1 - error; 0 - all ok
int llFlushAll();

// Flushes records until (not including) specified lsn.
// Parameters:
//	   lsn - until-boundary of flush.
// Returns: 
//     -1 - error; 0 - all ok
int llFlushLsn(LSN lsn);

// Truncates unnecessary logical log records.
// Returns: 
//     -1 - error; 0 - all ok
int llTruncateLog();

// Activates checkpoint procedure.
// Returns: 
//     -1 - error; -2 - already doing checkpoint; 0 - all ok
int llActivateCheckpoint();

// Enables checkpoints.
// Returns: 
//     -1 - error; 0 - all ok
int llEnableCheckpoints();

// Disable checkpoints (every checkpoint request will be ignored).
// Returns: 
//     -1 - error; 0 - all ok
int llDisableCheckpoints();

// Is checkpoint in progress now?
// Returns: 
//     true - checkpoint is active; false - checkpoint isn't active
bool llGetCheckpointActiveFlag();

// Should be called when checkpoint is finished.
// Returns: 
//     -1 - error; 0 - all ok
int llOnCheckpointFinish();

// Should be called when someone waits for checkpoint to finish
// llActivate should be called first!
// Returns:
//      -1 -error; 0 - ok
int llOnCheckpointWait();

// Returns timestamp of persistent snapshot
TIMESTAMP llGetPersTimestamp();

// Retrieves record from disk
// LSN may be corrected by the lfs to point on the needed record.
// Parameters:
// 		RecLsn - pointer to the needed lsn
// Returns:
//		NULL - in case of error (RecLsn won't be set to LFS_INVALID_LSN)
//		NULL - out of bound, but RecLsn will be set to LFS_INVALID_LSN 
//		pointer to record - all ok
void *llGetRecordFromDisc(LSN *RecLsn);

// Returns length of the given record. 
// Parameters:
// 		Rec - pointer to the record in memory. Rec should be the one returned by llMain.
//		len - length of the record (ignored, if Rec != NULL)
// Returns:
//		length of the record (0 - in case of error)
//
// NOTE: if Rec is NULL then it returns the length record of size 'len' would
//       have if it is being committed via llInsertRecord.
int llGetRecordSize(void *Rec, int len);

// Returns previous lsn for given record.
// Parameters:
// 		RecBuf - pointer to the record in memory. RecBuf should be the one returned by llMain.
// Returns:
//		lsn of the previous record (LFS_INVALID_LSN - if there is none).
LSN llGetPrevLsnFromRecord(void *RecBuf);

// Returns first lsn of given transaction
// Parameters:
// 		trid - trasnaction identifier
LSN llGetFirstTranLsn(transaction_id trid);

// Returns higher LSN boundary (lsn >= this don't exist)
// Parameters:
//      None
// Returns:
//      high boundary LSN
LSN llGetHighLSNBoundary();

// Function to walk on logical records
typedef void (*llOperFunc)(LSN lsn, void *RecBuf); 		 // function to perform some action on record
typedef LSN (*llNextLSN)(LSN curr_lsn, void *RecBuf);    // callback function to get next lsn for scan
typedef bool (*llPrereqRec)(LSN curr_lsn, void *RecBuf); // callback function to check if record is needed during scan

struct llRecInfo
{
	enum llOperations rec_oper;
	llOperFunc fun;
};

// This function scans records starting from given start_lsn.
// To get lsn of the next record it calls provided funNextLSN function. When it returns LFS_INVALID_LSN, scan becomes complete.
// RecordsInfo contains information about function for each operation. 
// Important notice: first byte of each record must contain type of the record corresponding to llOperations.
// Another notice: if operation is not in table, then no action is taken. But if LL_DEFAULT is present it will be 
// 		called. LL_DEFAULT has priority! So, it should be placed at the end of llRecInfo list.
// Parameters:
//	   RecordsInfo - info about records that must be processed by the scan (if for some type function is not specified, no
//			action will be taken. llScanRecords just calls funNextLSN and cintinues execution).
//	   RecordsInfoLen - number of elements in RecordsInfo
//     start_lsn - starting point of scan.
//     funNextLSN - callback function to receive next lsn for scanning
//     funPrereq  - this function is called on every specified record before corresponding function.
//						"true" - corresponding function will be called; "false" - the record is ignored
// Returns: 
//     -1 - error; 0 - all ok; 1 - lfs: out of bounds (this is not an error if we perform simple forward scan)
int llScanRecords(llRecInfo *RecordsInfo, unsigned int RecordsInfoLen, LSN start_lsn, llNextLSN funNextLSN, llPrereqRec funPrereq);

// Archives logical log
// Returns:
// 		number of the archived file
uint64_t llLogArchive();

// Check if we need to do checkpoint for maintenance reason.
// For example, when number of files is high enough we want to make checkpoint to truncate log
// Returns:
// 	true - if we need checkpoint; false - otherwise;
bool llNeedCheckpoint();

/*
 * Returns maximum allowed record size
 */
size_t llGetMaxRecordSize();

/*
 * General record function with sanity checks.
 *
 * rtype -- record type
 * trid -- transaction id (makes sense only for TR_RECORDs)
 * op -- operation code
 * num -- number of fields
 * ret_next_lsn -- see return values
 *
 * Returns:
 *   LSN of the inserted record -- if ret_next_lsn is false
 *   LSN of the next record to be inserted -- if ret_next_lsn is true
 *
 * Then, num * 2 variable arguments follow in this format:
 *    field -- void * -- buffer pointer
 *    field_len -- size_t -- length of the field
 *
 * In case of error throws exceptions:
 *    SE4156 -- logical log buffer too small
 *    SYSTEM -- not enough memory to hold temporary record
 *
 * NOTE: since we use variable arguments here, compiler cannot check types of
 *       arguments following 'num'. If you use this function you MUST be sure
 *       that you give (void *, size_t) pairs in every call to this function.
 */
LSN llLogGeneral(llRecordType rtype, transaction_id trid, llOperations op,
        bool ret_next_lsn, unsigned num, ...);

#endif
