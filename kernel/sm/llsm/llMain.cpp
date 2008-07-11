/*
 * File:  lfsMain.cpp - Main part of logical log.
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * General service for logical log. Synchronization primitives are provided here, which should be used to ensure thread-safe
 * behaviour. Remeber, that lfsStorage is already thread-safe. 
 *
 * Thread_safe: llActivateCheckpoint(), llEnableCheckpoints(), llDisableCheckpoints()
 *				llGetCheckpointActiveFlag(), llOnCheckpointFinish().
 *
 *
 */

#include "sm/llsm/llMain.h"
#include "common/u/ushm.h"
#include "common/u/usem.h"
#include "common/base.h"
#include "common/u/uevent.h"
#include "sm/llsm/lfsStorage.h"

#include <assert.h>

#define LL_FILE_PORTION_SIZE (INT64_C(100) * (1024 * 1024)) // size of chunk of logical log
#define LL_WRITEBUF_SIZE 1024 * 1024
#define LL_READBUF_SIZE 1024
#define LL_READBUF_SIZE 1024

#define LOG_FILE_PORTION_SIZE (INT64_C(100) * (1024*1024)) // 100Mb

struct llRecordHead
{
	LSN prev_lsn;  // lsn for previous record of the same transaction (LL_INVALID_LSN if no such record exists)
	int rec_len;   // len without sizeof(llRecordHead)
};

struct llFileHead
{
	LSN last_lsn;                  // last record of the log
	LSN checkpoint_lsn;       	   // lsn of the last checkpoint record
	LSN last_chain_lsn;            // lsn of the last record in physical records chain
	TIMESTAMP ts;                  // timestamp of the last persistent snapshot
	bool is_stopped_successfully;  // true, if the database was stopped correctly
	int sedna_db_version;          // sedna strucures version. should be moved to data master block, but what the hell :)
	bool is_archive;               // indicates that this is hot-backup(archive) file
	//new fields should be appended to the end of the structure
};

static USemaphore SyncSem;    // synchronization semaphore
static UEvent CheckpointEvent;// event to start checkpoint thread
static UShMem SharedMem;      // descriptor for shared global info

static void *ReadBuf;            // buffer for reading records
static int  ReadBufSize = 1024;  // size of the ReadBuf buffer

llGlobalInfo *llInfo = NULL; // pointer to the global info memory

int rollback_active = false; // true, if rollback is active on current transaction
int recovery_active = false; // true, if this process is a recovery process

// Create new logical log.
int llCreateNew(const char *db_files_path, const char *db_name)
{
	llFileHead fileHead;

	fileHead.last_lsn = LFS_INVALID_LSN;
	fileHead.checkpoint_lsn = LFS_INVALID_LSN;
	fileHead.last_chain_lsn = LFS_INVALID_LSN;
	fileHead.ts = 0x10000;
	fileHead.is_stopped_successfully = false;
	fileHead.sedna_db_version = SEDNA_DATA_STRUCTURES_VER;
	fileHead.is_archive = false;
	
	lfsCreateNew(db_files_path, db_name, "llog", LOG_FILE_PORTION_SIZE, &fileHead, sizeof(llFileHead));

	return 0;
}

// Insert record in logical log. 
LSN llInsertRecord(const void *RecBuf, int RecLen, transaction_id trid)
{
	RECOVERY_CRASH;
	void *rec_all;

	LSN ret_lsn;
	llRecordHead log_head;

	llLock();

	rec_all = malloc(RecLen + sizeof(llRecordHead));
	if (rec_all == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

	if (trid != -1 && llInfo->llTransInfoTable[trid].last_lsn != LFS_INVALID_LSN)
		log_head.prev_lsn = llInfo->llTransInfoTable[trid].last_lsn;
	else
		log_head.prev_lsn = LFS_INVALID_LSN;
	log_head.rec_len = RecLen;

	memcpy(rec_all, &log_head, sizeof(llRecordHead));
	memcpy((char *)rec_all + sizeof(llRecordHead), RecBuf, RecLen);
  
	ret_lsn = lfsAppendRecord(rec_all, RecLen + sizeof(llRecordHead));
	
	free(rec_all);

	if (trid != -1)
	{
		llInfo->llTransInfoTable[trid].last_lsn = ret_lsn;

		if (llInfo->llTransInfoTable[trid].first_lsn == LFS_INVALID_LSN) 
			llInfo->llTransInfoTable[trid].first_lsn = ret_lsn;

		llInfo->llTransInfoTable[trid].num_of_log_records += 1;
		llInfo->llTransInfoTable[trid].last_len = RecLen + sizeof(llRecordHead);
    }
    else
		llInfo->last_chain_lsn = ret_lsn;

	llInfo->last_lsn = ret_lsn;

	llUnlock();

	return ret_lsn;
}

// Locks logical log (global ll synchronization)
void llLock()
{
	if (USemaphoreDown(SyncSem, __sys_call_error) != 0)
		throw SYSTEM_EXCEPTION("Can't down semaphore: SEDNA_LOGICAL_LOG_PROTECTION_SEM_NAME");
}

// Unlocks logical log (global ll synchronization)
void llUnlock()
{
	if (USemaphoreUp(SyncSem, __sys_call_error) != 0)
		throw SYSTEM_EXCEPTION("Can't up semaphore: SEDNA_LOGICAL_LOG_PROTECTION_SEM_NAME");
}

// Inits logical log.
int llInit(const char *db_files_path, const char *db_name, int *sedna_db_version, bool *exit_status)
{
	lfsInit(db_files_path, db_name, "llog", LL_WRITEBUF_SIZE, LL_READBUF_SIZE);

	if (USemaphoreCreate(&SyncSem, 1, 1, CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME, NULL, __sys_call_error))
		throw USER_EXCEPTION2(SE4010, "SEDNA_LOGICAL_LOG_PROTECTION_SEM_NAME");

	if (UEventOpen(&CheckpointEvent, SNAPSHOT_CHECKPOINT_EVENT, __sys_call_error) != 0) 
		throw USER_EXCEPTION2(SE4012, "SNAPSHOT_CHECKPOINT_EVENT");  
  
	llFileHead file_head;
	lfsGetHeader(&file_head, sizeof(llFileHead));

	*exit_status = file_head.is_stopped_successfully;
	*sedna_db_version = file_head.sedna_db_version;

	ReadBuf = malloc(LL_READBUF_SIZE);
	if (ReadBuf == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");
	ReadBufSize = LL_READBUF_SIZE;

    //create shared memory
	if (uCreateShMem(&SharedMem, CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME, sizeof(llGlobalInfo), NULL, __sys_call_error) != 0)
		throw USER_EXCEPTION2(SE4016, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

    //init shared memory pointer
	if ((llInfo = (llGlobalInfo *)uAttachShMem(SharedMem, NULL, sizeof(llGlobalInfo), __sys_call_error)) == NULL)
		throw USER_EXCEPTION2(SE4023, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

    //init header of shared memory
	lfsGetHeader(&file_head, sizeof(llFileHead));

	llInfo->checkpoint_lsn = file_head.checkpoint_lsn;
	llInfo->min_rcv_lsn = LFS_INVALID_LSN;
	llInfo->last_chain_lsn = file_head.last_chain_lsn;
	llInfo->last_lsn = file_head.last_lsn;
   
	llInfo->ts = file_head.ts;

	llInfo->hotbackup_needed = file_head.is_archive;

	for (int i = 0; i < CHARISMA_MAX_TRNS_NUMBER; i++)
	{
		llInfo->llTransInfoTable[i].last_lsn = LFS_INVALID_LSN; 
		llInfo->llTransInfoTable[i].first_lsn = LFS_INVALID_LSN; 
		llInfo->llTransInfoTable[i].num_of_log_records = 0;
		llInfo->llTransInfoTable[i].last_len = 0;
	}

	llInfo->checkpoint_on   = false; // checkpoint is currently inactive
	llInfo->checkpoint_flag = false; // checkpoints are initially disabled

	file_head.is_stopped_successfully = false;

	lfsWriteHeader(&file_head, sizeof(llFileHead));

	return 0;
}

// Releases logical log.
int llRelease()
{
	llFileHead file_head;
	lfsGetHeader(&file_head, sizeof(llFileHead));

	file_head.is_stopped_successfully = true;
	file_head.is_archive = false;

	lfsWriteHeader(&file_head, sizeof(llFileHead));
  
	if (uDettachShMem(SharedMem, llInfo, __sys_call_error) != 0)
		throw USER_EXCEPTION2(SE4024, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

	if (uReleaseShMem(SharedMem, __sys_call_error) != 0)
		throw USER_EXCEPTION2(SE4020, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

	if (USemaphoreRelease(SyncSem, __sys_call_error) != 0)
		throw USER_EXCEPTION2(SE4013, "CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME");

	if (UEventClose(&CheckpointEvent, __sys_call_error) != 0) 
		throw USER_EXCEPTION2(SE4013, "SNAPSHOT_CHECKPOINT_EVENT");  

	lfsRelease();

	free(ReadBuf);

	return 0;
}

// Opens logical log (must be inited by other thread by llInit first). Should be called on transaction process.
int llOpen(const char *db_files_path, const char *db_name, bool rcv_active)
{
	lfsConnect(db_files_path, db_name, "llog", LL_READBUF_SIZE);

	if (USemaphoreOpen(&SyncSem, CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME, __sys_call_error) != 0)
		throw USER_EXCEPTION2(SE4012, "CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME");

	if (UEventOpen(&CheckpointEvent, SNAPSHOT_CHECKPOINT_EVENT, __sys_call_error) != 0) 
		throw USER_EXCEPTION2(SE4012, "SNAPSHOT_CHECKPOINT_EVENT");  

	if (uOpenShMem(&SharedMem, CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME, sizeof(llGlobalInfo), __sys_call_error) != 0)
		throw USER_EXCEPTION2(SE4021, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

	if ((llInfo = (llGlobalInfo *)uAttachShMem(SharedMem, NULL, sizeof(llGlobalInfo), __sys_call_error)) == NULL)
		throw USER_EXCEPTION2(SE4023, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

	ReadBuf = malloc(LL_READBUF_SIZE);
	if (ReadBuf == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");
	ReadBufSize = LL_READBUF_SIZE;

	recovery_active = rcv_active;

	return 0;
}

// Closes logical log (pair-function for open).
int llClose()
{
	if (uDettachShMem(SharedMem, llInfo, __sys_call_error) != 0)
		throw USER_EXCEPTION2(SE4024, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

	if (uCloseShMem(SharedMem, __sys_call_error) != 0)
		throw USER_EXCEPTION2(SE4022, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

	if (USemaphoreClose(SyncSem, __sys_call_error) != 0)
		throw USER_EXCEPTION2(SE4013, "CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME");

	if (UEventClose(&CheckpointEvent, __sys_call_error) != 0) 
		throw USER_EXCEPTION2(SE4013, "SNAPSHOT_CHECKPOINT_EVENT");  

	lfsDisconnect();

	free(ReadBuf);

	return 0;
}

// Should be called for every new transaction.
int llOnTransBegin(transaction_id trid)
{
	rollback_active = false;

	assert(trid >= 0 && trid < CHARISMA_MAX_TRNS_NUMBER);

	llInfo->llTransInfoTable[trid].last_lsn = LFS_INVALID_LSN;
	llInfo->llTransInfoTable[trid].first_lsn = LFS_INVALID_LSN;
	llInfo->llTransInfoTable[trid].num_of_log_records = 0;
	llInfo->llTransInfoTable[trid].mode = NORMAL_MODE;
	llInfo->llTransInfoTable[trid].last_len = 0;

	return 0;
}

// Should be called for every rolled back or committed transaction.
int llOnTransEnd(transaction_id trid)
{
	assert(trid >= 0 && trid < CHARISMA_MAX_TRNS_NUMBER);

	llInfo->llTransInfoTable[trid].last_lsn = LFS_INVALID_LSN;
	llInfo->llTransInfoTable[trid].first_lsn = LFS_INVALID_LSN;
	llInfo->llTransInfoTable[trid].num_of_log_records = 0;
	llInfo->llTransInfoTable[trid].last_len = 0;

	lfsCloseAllFiles();

	return 0;
}

// Flushes records belonging to the specified transaction.
int llFlushTransRecs(transaction_id trid)
{
	RECOVERY_CRASH;
	
	assert(trid >= 0 && trid < CHARISMA_MAX_TRNS_NUMBER);

	if (llInfo->llTransInfoTable[trid].last_lsn != LFS_INVALID_LSN)
		llFlushLsn(llInfo->llTransInfoTable[trid].last_lsn + llInfo->llTransInfoTable[trid].last_len);

	return 0;
}

// Flushes all records.
int llFlushAll()
{
	llLock();

	llFileHead file_head;

	lfsFlushAll();  

	llFlushHeader();

	llUnlock();

	return 0;
}

// Flushes records until (not including) specified lsn.
int llFlushLsn(LSN lsn)
{
	RECOVERY_CRASH;
	llFileHead file_head;

	if (lsn == LFS_INVALID_LSN) return 0;

	llLock();

	// since last_chain_lsn might be written here in llFlushHeader() we need to flush all physical records
	if (llInfo->last_chain_lsn >= lsn)
	{
		llUnlock();
		llFlushAll();
		return 0;
	}

	lfsFlush(lsn);

	llFlushHeader();

	llUnlock();

	return 0;
}

// Flushes file header.
int llFlushHeader()
{
	llFileHead file_head;

	lfsGetHeader(&file_head, sizeof(llFileHead));
	
	file_head.checkpoint_lsn = llInfo->checkpoint_lsn;
	file_head.last_chain_lsn = llInfo->last_chain_lsn;
	file_head.ts = llInfo->ts;
	file_head.last_lsn = llInfo->last_lsn;

	lfsWriteHeader(&file_head, sizeof(llFileHead));

	return 0;
}

// Truncates unnecessary logical log records.
int llTruncateLog()
{
	LSN minLSN;

	minLSN = (llInfo->min_rcv_lsn == LFS_INVALID_LSN) ? llInfo->checkpoint_lsn :
  												 		llInfo->min_rcv_lsn;

	if (minLSN == LFS_INVALID_LSN)
		return 0;

	lfsTruncate(minLSN);

	return 0;
}

// Activates checkpoint procedure.
// Returns: 
//     -1 - error; 0 - all ok
int llActivateCheckpoint()
{
	llLock();

	// we are already making checkpoint or checkpoint is disabled    
    if (llInfo->checkpoint_on || !llInfo->checkpoint_flag)
    {
    	llUnlock();
    	return 0;
    }
        
    llInfo->checkpoint_on = true;

	if (UEventSet(&CheckpointEvent,  __sys_call_error) != 0)
		throw SYSTEM_EXCEPTION("Event signaling for checkpoint thread failed");

	llUnlock();

	return 0;
}

// Enables checkpoints.
// Returns: 
//     -1 - error; 0 - all ok
int llEnableCheckpoints()
{
  llLock();

  llInfo->checkpoint_flag = true;
  
  llUnlock();

  return 0;
}

// Disable checkpoints (every checkpoint request will be ignored).
// Returns: 
//     -1 - error; 0 - all ok
int llDisableCheckpoints()
{
  llLock();

  llInfo->checkpoint_flag = false;
  
  llUnlock();

  return 0;
}
 
// Is checkpoint in progress now?
// Returns: 
//     true - checkpoint is active; false - checkpoint isn't active
bool llGetCheckpointActiveFlag()
{
  bool flag;

  llLock();

  flag = llInfo->checkpoint_on;
  
  llUnlock();

  return flag;
}

// Retrieves record from disk
// LSN may be corrected by the lfs to point on the needed record.
void *llGetRecordFromDisc(LSN *RecLsn)
{
	int rec_len;
	llRecordHead *log_head;

	if (*RecLsn == LFS_INVALID_LSN) return NULL;

	lfsGetRecord(RecLsn, ReadBuf, sizeof(llRecordHead));

	log_head = (llRecordHead *)ReadBuf;
	rec_len = log_head->rec_len + sizeof(llRecordHead);

	if (ReadBufSize < rec_len)
	{
		free(ReadBuf);
		if ((ReadBuf = malloc(rec_len)) == NULL)
			return NULL;
		ReadBufSize = rec_len;
	}

	lfsGetRecord(RecLsn, ReadBuf, rec_len);

	return (char *)ReadBuf + sizeof(llRecordHead);
}

// Returns first lsn of given transaction
LSN llGetFirstTranLsn(transaction_id trid)
{
	LSN lsn;

	llLock();

	lsn = llInfo->llTransInfoTable[trid].first_lsn;

	llUnlock();

	return lsn;
}

// Should be called when checkpoint is finished.
// Returns: 
//     -1 - error; 0 - all ok
int llOnCheckpointFinish()
{
  llLock();

  llInfo->checkpoint_on = false;
  
  llUnlock();

  return 0;
}

// Returns timestamp of persistent snapshot
TIMESTAMP llGetPersTimestamp()
{
  TIMESTAMP ts;

  llLock();

  ts = llInfo->ts;
  
  llUnlock();

  return ts;
}


// Returns length of the given record
int llGetRecordSize(void *Rec)
{
  llRecordHead *RecHead;

  if (Rec == NULL) return 0;

  RecHead = (llRecordHead *)((char *)Rec - sizeof(llRecordHead));

  return sizeof(llRecordHead) + RecHead->rec_len;
}

// This function scans records starting from given start_lsn.
int llScanRecords(llRecInfo *RecordsInfo, int RecordsInfoLen, LSN start_lsn, llNextLSN funNextLSN, llPrereqRec funPrereq)
{
	char *RecBuf, cop;
	LSN lsn = start_lsn;

	assert(RecordsInfo != NULL && RecordsInfoLen > 0 && funNextLSN != NULL);

	while (lsn != LFS_INVALID_LSN)
	{
		// get record from lfs
		RecBuf = (char *)llGetRecordFromDisc(&lsn);

		cop = *(RecBuf);

		assert(cop >= 0);

		if (RecordsInfoLen > cop && RecordsInfo[cop].rec_oper == (llOperations)cop)
		{
			if (funPrereq == NULL || funPrereq(lsn, RecBuf)) RecordsInfo[cop].fun(lsn, RecBuf);
		}
		else
			for (int i = 0; i < RecordsInfoLen; i++)
				if (RecordsInfo[i].rec_oper == (llOperations)cop || RecordsInfo[i].rec_oper == LL_DEFAULT)
				{
					if (funPrereq == NULL || funPrereq(lsn, RecBuf)) RecordsInfo[i].fun(lsn, RecBuf);
					break;
				}

		lsn = funNextLSN(lsn, RecBuf);
	}

	return 0;
}

// Archives logical log
uint64_t llLogArchive()
{
	uint64_t num;
	
	llFileHead file_head;

	lfsGetHeader(&file_head, sizeof(llFileHead));
	
	file_head.is_archive = true;

	lfsWriteHeader(&file_head, sizeof(llFileHead));
	
	num = lfsArchiveCurrentFile();

	return num;
}

// Returns previous lsn for given record
LSN llGetPrevLsnFromRecord(void *Rec)
{
  llRecordHead *RecHead;

  if (Rec == NULL) return LFS_INVALID_LSN;

  RecHead = (llRecordHead *)((char *)Rec - sizeof(llRecordHead));

  return RecHead->prev_lsn;
}
