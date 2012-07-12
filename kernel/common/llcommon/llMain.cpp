/*
 * File:  llMain.cpp - Main part of logical log.
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

#include "llMain.h"

#include "u/ushm.h"
#include "u/usem.h"
#include "u/uevent.h"

#include "aux/cppcast.h"

#include "common/sedna.h"
#include "common/base.h"
#include "common/llcommon/lfsStorage.h"

#include <assert.h>
#include <string>
#include <stdarg.h>

#define LL_FILE_PORTION_SIZE (INT64_C(100) * (1024 * 1024))   // size of chunk of logical log
#define LL_WRITEBUF_SIZE 1024 * 1024                          // write buffer size (for lfs)
#define LL_READBUF_SIZE 1024                                  // read buffer size (for lfs)
#define LL_MAX_LOG_FILES ((uint32_t)max_log_files)            // maximum files until truncate

struct llRecordHead {
    LSN prev_lsn;  // lsn for previous record of the same transaction (LL_INVALID_LSN if no such record exists)
    int rec_len;   // len without sizeof(llRecordHead)
};

struct llFileHead {
    LSN checkpoint_lsn;       	   // lsn of the last checkpoint record
    LSN last_chain_lsn;            // lsn of the last record in physical records chain
    TIMESTAMP ts;                  // timestamp of the last persistent snapshot
    bool is_stopped_successfully;  // true, if the database was stopped correctly
    int sedna_db_version;          // sedna strucures version. should be moved to data master block, but what the hell :)
    bool is_archive;               // indicates that this is hot-backup(archive) file
    uint64_t next_arch_file;       // next file to archive; if 0 - then base isn't in incremental mode

    LSN hb_lsn;         		   // lsn of record containing info about hot-backup
    //new fields should be appended to the end of the structure
};

static int max_log_files;     // maximum number of files until commit (external parameter)
static USemaphore SyncSem;    // synchronization semaphore
static UEvent CheckpointEvent;// event to start checkpoint thread
static UShMem SharedMem;      // descriptor for shared global info

static USemaphore WaitCheckpoint; // semaphore to wait on checkpoint

static void *ReadBuf = NULL;     // buffer for reading records (it is realloced automatically if needed)
static int  ReadBufSize = 0;     // size of the ReadBuf buffer

static bool llNeedHbRecord = true;

llGlobalInfo *llInfo = NULL; // pointer to the global info memory

int rollback_active = false; // true, if rollback is active on current transaction
int recovery_active = false; // true, if this process is a recovery process

#define LL_ERROR(err_msg) _llProcessError(__FILE__, __SE_FUNCTION__,  __LINE__, (err_msg))

using namespace std;

// processes error (throws exception for now)
static void _llProcessError(const char *file, const char *func, int line, const char *llErrorMsg)
{
    string err_msg;

    err_msg = string(llErrorMsg);

    err_msg += " - (" + string(file) + ':' + string(func) + ':' + cast_to_string(line) + ')';

    throw SYSTEM_EXCEPTION(err_msg.c_str());
}

// Create new logical log.
int llCreateNew(const char *db_files_path, const char *db_name, uint64_t log_file_size)
{
    llFileHead fileHead;

    fileHead.checkpoint_lsn = LFS_INVALID_LSN;
    fileHead.last_chain_lsn = LFS_INVALID_LSN;
    fileHead.hb_lsn = LFS_INVALID_LSN;
    fileHead.ts = 0x10000;
    fileHead.is_stopped_successfully = false;
    fileHead.sedna_db_version = SEDNA_DATA_STRUCTURES_VER;
    fileHead.is_archive = false;
    fileHead.next_arch_file = 0;

    // tune log_file_size parameter let's take sectorsize + 1Mb as minimum
    if (log_file_size > 0 && log_file_size < LL_WRITEBUF_SIZE + 512) {
        log_file_size = LL_WRITEBUF_SIZE + 512;
    }

    lfsCreateNew(db_files_path, db_name, "llog",
                 (log_file_size == (uint64_t)-1) ? LL_FILE_PORTION_SIZE : log_file_size,
                 &fileHead, sizeof(llFileHead));

    return 0;
}

// Insert record in logical log.
LSN llInsertRecord(llRecordType rtype, const void *RecBuf, unsigned int RecLen,
                   transaction_id trid)
{
    RECOVERY_CRASH;
    void *rec_all;

    LSN ret_lsn;
    llRecordHead log_head;

    rec_all = malloc(RecLen + sizeof(llRecordHead));
    if (rec_all == NULL) {
        LL_ERROR("internal ll error: cannot allocate memory");
    }

    // lsn of previous record
    if (rtype == TR_RECORD && llInfo->llTransInfoTable[trid].last_lsn != LFS_INVALID_LSN) {
        log_head.prev_lsn = llInfo->llTransInfoTable[trid].last_lsn;
    } else {
        log_head.prev_lsn = LFS_INVALID_LSN;
    }

    log_head.rec_len = RecLen;

    memcpy(rec_all, &log_head, sizeof(llRecordHead));
    memcpy((char *)rec_all + sizeof(llRecordHead), RecBuf, RecLen);

    ret_lsn = lfsAppendRecord(rec_all, RecLen + sizeof(llRecordHead));

    free(rec_all);

    // record belongs to some transaction
    if (rtype == TR_RECORD) {
        llInfo->llTransInfoTable[trid].last_lsn = ret_lsn;

        // first record for this trid
        if (llInfo->llTransInfoTable[trid].first_lsn == LFS_INVALID_LSN) {
            llInfo->llTransInfoTable[trid].first_lsn = ret_lsn;
        }

        llInfo->llTransInfoTable[trid].num_of_log_records += 1;					 // debug info
        llInfo->llTransInfoTable[trid].last_len = RecLen + sizeof(llRecordHead); // needed for flush
    } else if (rtype == PHYS_RECORD) {
        llLock();
        llInfo->last_chain_lsn = ret_lsn;   // last record in physical chain
        llUnlock();
    }

    llNeedHbRecord = true; // since we inserted new record log will be archived

    return ret_lsn;
}

// Locks logical log (global ll synchronization)
inline
void llLock()
{
    if (USemaphoreDown(SyncSem, __sys_call_error) != 0) {
        throw SYSTEM_EXCEPTION("Can't down semaphore: SEDNA_LOGICAL_LOG_PROTECTION_SEM_NAME");
    }
}

// Unlocks logical log (global ll synchronization)
inline
void llUnlock()
{
    if (USemaphoreUp(SyncSem, __sys_call_error) != 0) {
        throw SYSTEM_EXCEPTION("Can't up semaphore: SEDNA_LOGICAL_LOG_PROTECTION_SEM_NAME");
    }
}

/*
 hot-backup log record format (this serves as a store of hb-specific info):

 op (1 byte)
 pers snapshot ts (TIMESTAMP)
 hb_checkpoint_lsn (LSN)
 hb_last_chain_lsn (LSN)
*/
static LSN llLogHotBackup()
{
    LSN res;

    res = llLogGeneral(GEN_RECORD, INVALID_TRID, LL_HBINFO, false, 3,
                       &(llInfo->ts), sizeof(TIMESTAMP),
                       &(llInfo->checkpoint_lsn), sizeof(LSN),
                       &(llInfo->last_chain_lsn), sizeof(LSN));

    return res;
}

// retrieves info from hot-backup record
static void llRetrieveHbRec(void *RecBuf)
{
    char *offs;

    offs = (char *)RecBuf + sizeof(char);

    llInfo->ts = *((TIMESTAMP *)offs);
    offs += sizeof(TIMESTAMP);

    llInfo->checkpoint_lsn = *((LSN *)offs);
    offs += sizeof(LSN);

    llInfo->last_chain_lsn = *((LSN *)offs);

    RECOVERY_CRASH;
}

// Inits logical log.
int llInit(const char *db_files_path, const char *db_name, int max_log_files_param,
           int *sedna_db_version, bool *exit_status, int rcv_active)
{
    lfsInit(db_files_path, db_name, "llog", LL_WRITEBUF_SIZE, LL_READBUF_SIZE);

    // sync semaphore
    if (USemaphoreCreate(&SyncSem, 1, 1, logicalLogSem, NULL, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot create semaphore: CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME");
    }

    // to wait for checkpoint to finish
    if (USemaphoreCreate(&WaitCheckpoint, 0, 1, checkpointFinishedSemName, NULL, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot create semaphore: SEDNA_CHECKPOINT_FINISHED_SEM");
    }

    // event to start checkpoint procedure
    if (UEventOpen(&CheckpointEvent, checkpointEventName, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot open event: SNAPSHOT_CHECKPOINT_EVENT");
    }

    llFileHead file_head;
    lfsGetHeader(&file_head, sizeof(llFileHead));

    *exit_status = file_head.is_stopped_successfully; // recovery needed?
    *sedna_db_version = file_head.sedna_db_version;   // data structures version

    ReadBuf = malloc(LL_READBUF_SIZE);
    if (ReadBuf == NULL) {
        LL_ERROR("internal ll error: cannot allocate memory");
    }
    ReadBufSize = LL_READBUF_SIZE;

    //create shared memory
    if (uCreateShMem(&SharedMem, logicalLogShm, sizeof(llGlobalInfo), NULL, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot create shared memory: CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");
    }

    //init shared memory pointer
    if ((llInfo = (llGlobalInfo *)uAttachShMem(&SharedMem, NULL, 0, __sys_call_error)) == NULL) {
        LL_ERROR("internal ll error: cannot attach shared memory: CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");
    }

    //init header of shared memory
    lfsGetHeader(&file_head, sizeof(llFileHead));

    llInfo->min_rcv_lsn = LFS_INVALID_LSN;
    llInfo->checkpoint_lsn = file_head.checkpoint_lsn;
    llInfo->last_chain_lsn = file_head.last_chain_lsn;

    llInfo->ts = file_head.ts;

    llInfo->hotbackup_needed = file_head.is_archive;

    // if we recover hot-backup we need to retrieve hb-record
    if (llInfo->hotbackup_needed) {
        llRetrieveHbRec(llGetRecordFromDisc(&(file_head.hb_lsn)));
    }

    llInfo->next_arch_file = file_head.next_arch_file;

    for (int i = 0; i < CHARISMA_MAX_TRNS_NUMBER; i++) {
        llInfo->llTransInfoTable[i].last_lsn = LFS_INVALID_LSN;
        llInfo->llTransInfoTable[i].first_lsn = LFS_INVALID_LSN;
        llInfo->llTransInfoTable[i].num_of_log_records = 0;
        llInfo->llTransInfoTable[i].last_len = 0;
    }

    llInfo->checkpoint_on   = false; // checkpoint is currently inactive
    llInfo->checkpoint_flag = false; // checkpoints are initially disabled

    file_head.is_stopped_successfully = false;

    lfsWriteHeader(&file_head, sizeof(llFileHead));  // since this moment any crash will lead to recovery

    max_log_files = max_log_files_param;
    if (max_log_files < 1) {
        LL_ERROR("internal ll error: max_log_files parameter is invalid ");
    }
    recovery_active = rcv_active;

    // print some message about consistency of the database
    if (rcv_active) {
        if (!(*exit_status))
            fprintf(res_os, (llInfo->hotbackup_needed) ? "Hot-backup recovery in progress...\n" :
                    "Database recovery in progress...\n");
        else {
            elog(EL_LOG, ("Database is in consistent state. Starting..."));
            fprintf(res_os, "Database is in consistent state. Starting...\n");
        }
    }
    return 0;
}

// Releases logical log.
int llRelease()
{
    llFileHead file_head;
    lfsGetHeader(&file_head, sizeof(llFileHead));

    file_head.is_stopped_successfully = true;        // successful exit
    file_head.is_archive = false;                    // hot-backup recovery won't be needed

    lfsWriteHeader(&file_head, sizeof(llFileHead));

    if (uDettachShMem(&SharedMem, llInfo, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot dettach shared memory: CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");
    }

    if (uReleaseShMem(&SharedMem, logicalLogShm, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot release shared memory: CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");
    }

    if (USemaphoreRelease(SyncSem, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot release semaphore: CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME");
    }

    if (UEventClose(&CheckpointEvent, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot close event: SNAPSHOT_CHECKPOINT_EVENT");
    }

    // to wait for checkpoint to finish
    if (USemaphoreRelease(WaitCheckpoint, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot release semaphore: SEDNA_CHECKPOINT_FINISHED_SEM");
    }

    lfsRelease();

    free(ReadBuf);

    return 0;
}

// Opens logical log (must be inited by other thread by llInit first). Should be called on transaction process.
int llOpen(const char *db_files_path, const char *db_name, bool rcv_active)
{
    lfsConnect(db_files_path, db_name, "llog", LL_READBUF_SIZE);

    if (USemaphoreOpen(&SyncSem, logicalLogSem, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot open semaphore: CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME");
    }

    // to wait for checkpoint to finish
    if (USemaphoreOpen(&WaitCheckpoint, checkpointFinishedSemName, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot open semaphore: SEDNA_CHECKPOINT_FINISHED_SEM");
    }

    if (UEventOpen(&CheckpointEvent, checkpointEventName, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot open event: SNAPSHOT_CHECKPOINT_EVENT");
    }

    if (uOpenShMem(&SharedMem, logicalLogShm, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot open shared memory: CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");
    }

    if ((llInfo = (llGlobalInfo *)uAttachShMem(&SharedMem, NULL, 0, __sys_call_error)) == NULL) {
        LL_ERROR("internal ll error: cannot attach shared memory: CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");
    }

    ReadBuf = malloc(LL_READBUF_SIZE);
    if (ReadBuf == NULL) {
        LL_ERROR("internal ll error: cannot allocate memory");
    }
    ReadBufSize = LL_READBUF_SIZE;

    recovery_active = rcv_active;

    return 0;
}

// Closes logical log (pair-function for open).
int llClose()
{
    if (uDettachShMem(&SharedMem, llInfo, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot dettach shared memory: CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");
    }

    if (uCloseShMem(&SharedMem, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot close shared memory: CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");
    }

    if (USemaphoreClose(SyncSem, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot close semaphore: CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME");
    }

    // to wait for checkpoint to finish
    if (USemaphoreClose(WaitCheckpoint, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot close semaphore: SEDNA_CHECKPOINT_FINISHED_SEM");
    }

    if (UEventClose(&CheckpointEvent, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot close event: SNAPSHOT_CHECKPOINT_EVENT");
    }

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

// Flushes file header.
static int _llFlushHeader()
{
    llFileHead file_head;

    lfsGetHeader(&file_head, sizeof(llFileHead));

    // in case of hot-backup recovery we need to store last_chain_lsn
    // since physical records are written to log
    if (recovery_active && llInfo->hotbackup_needed && !llInfo->checkpoint_on) {
        file_head.hb_lsn = llLogHotBackup();
        lfsFlushAll();
    }

    file_head.checkpoint_lsn = llInfo->checkpoint_lsn;

    file_head.next_arch_file = llInfo->next_arch_file;

    file_head.last_chain_lsn = llInfo->last_chain_lsn;

    file_head.ts = llInfo->ts;

    lfsWriteHeader(&file_head, sizeof(llFileHead));

    return 0;
}

// Flushes records belonging to the specified transaction.
int llFlushTransRecs(transaction_id trid)
{
    RECOVERY_CRASH;

    assert(trid >= 0 && trid < CHARISMA_MAX_TRNS_NUMBER);

    // no sychronization needed since we access trid-specific records
    if (llInfo->llTransInfoTable[trid].last_lsn != LFS_INVALID_LSN) {
        llFlushLsn(llInfo->llTransInfoTable[trid].last_lsn + llInfo->llTransInfoTable[trid].last_len);
    }

    return 0;
}

// Flushes all records.
int llFlushAll()
{
    llLock();

    /*	llFileHead file_head; */

    lfsFlushAll();

    _llFlushHeader(); // last_chain_lsn is valid in llInfo

    llUnlock();

    return 0;
}

// Flushes records until (not including) specified lsn.
int llFlushLsn(LSN lsn)
{
    RECOVERY_CRASH;

    if (lsn == LFS_INVALID_LSN) {
        return 0;
    }

    llLock();

    // since last_chain_lsn might be written here in llFlushHeader() we need to flush all physical records
    if (llInfo->last_chain_lsn >= lsn) {
        llUnlock();
        llFlushAll();
        return 0;
    }

    lfsFlush(lsn);

    _llFlushHeader(); // last_chain_lsn is valid in llInfo

    llUnlock();

    return 0;
}

// Truncates unnecessary logical log records.
int llTruncateLog()
{
    LSN minLSN;

    minLSN = (llInfo->min_rcv_lsn == LFS_INVALID_LSN) ? llInfo->checkpoint_lsn :
             llInfo->min_rcv_lsn;

    if (minLSN == LFS_INVALID_LSN) {
        return 0;
    }

    lfsTruncate(minLSN, llInfo->next_arch_file);

    return 0;
}

// Activates checkpoint procedure.
int llActivateCheckpoint()
{
    int res;

    llLock();

    // we are already making checkpoint or checkpoint is disabled
    if (llInfo->checkpoint_on || !llInfo->checkpoint_flag) {
        llUnlock();
        return -2;
    }

    llInfo->checkpoint_on = true;

    // semaphore may be already down if last checkpoint was initiated by transaction
    // if checkpoint was initiated by llNeedCheckpoint logic then it'd be upped
    res = USemaphoreDownTimeout(WaitCheckpoint, 0, __sys_call_error);

    if (res != 0 && res != 2) { // normal down (0) or timeout (2)
        LL_ERROR("internal ll error: cannot down wait for checkpoint semaphore");
    }

    if (UEventSet(&CheckpointEvent,  __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot set checkpoint event");
    }

    llUnlock();

    return 0;
}

// Enables checkpoints.
int llEnableCheckpoints()
{
    llLock();

    llInfo->checkpoint_flag = true;

    llUnlock();

    return 0;
}

// Disable checkpoints (every checkpoint request will be ignored).
int llDisableCheckpoints()
{
    llLock();

    llInfo->checkpoint_flag = false;

    llUnlock();

    return 0;
}

// Is checkpoint in progress now?
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

    if (*RecLsn == LFS_INVALID_LSN) {
        return NULL;
    }

    if (lfsGetRecord(RecLsn, ReadBuf, sizeof(llRecordHead)) == 0) {
        *RecLsn = LFS_INVALID_LSN;
        return NULL;
    }

    log_head = (llRecordHead *)ReadBuf;
    rec_len = log_head->rec_len + sizeof(llRecordHead);

    if (ReadBufSize < rec_len) {
        free(ReadBuf);
        if ((ReadBuf = malloc(rec_len)) == NULL) {
            LL_ERROR("internal ll error: cannot allocate memory");
        }

        ReadBufSize = rec_len;
    }

    if (lfsGetRecord(RecLsn, ReadBuf, rec_len) == 0) {
        *RecLsn = LFS_INVALID_LSN;
        return NULL;
    }

    return (char *)ReadBuf + sizeof(llRecordHead);
}

// Returns first lsn of given transaction
LSN llGetFirstTranLsn(transaction_id trid)
{
    return llInfo->llTransInfoTable[trid].first_lsn;
}

// Should be called when checkpoint is finished.
// Returns:
//     -1 - error; 0 - all ok
int llOnCheckpointFinish()
{
    llLock();

    if (llInfo->checkpoint_on) {
        llInfo->checkpoint_on = false;

        if (USemaphoreUp(WaitCheckpoint, __sys_call_error) != 0) {
            LL_ERROR("internal ll error: cannot up checkpoint-wait semaphore");
        }
    }

    llUnlock();

    return 0;
}

// Should be called when someone waits for checkpoint to finish
// llActivate should be called first!
int llOnCheckpointWait()
{
    if (USemaphoreDown(WaitCheckpoint, __sys_call_error) != 0) {
        LL_ERROR("internal ll error: cannot down checkpoint-wait semaphore");
    }

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
int llGetRecordSize(void *Rec, int len)
{
    llRecordHead *RecHead;

    if (Rec == NULL) {
        return sizeof(llRecordHead) + len;
    }

    RecHead = (llRecordHead *)((char *)Rec - sizeof(llRecordHead));

    return sizeof(llRecordHead) + RecHead->rec_len;
}

// Returns higher LSN boundary (lsn >= this don't exist)
LSN llGetHighLSNBoundary()
{
    return lfsGetHighLSNBoundary();
}

// This function scans records starting from given start_lsn.
int llScanRecords(llRecInfo *RecordsInfo, unsigned int RecordsInfoLen, LSN start_lsn, llNextLSN funNextLSN, llPrereqRec funPrereq)
{
    char *RecBuf;
    unsigned int cop;
    LSN lsn = start_lsn;

    assert(RecordsInfo != NULL && RecordsInfoLen > 0 && funNextLSN != NULL);

    while (lsn != LFS_INVALID_LSN) {
        // get record from lfs
        if ((RecBuf = (char *)llGetRecordFromDisc(&lsn)) == NULL) {
            return 1;
        }

        cop = *(RecBuf);

        if (RecordsInfoLen > cop && RecordsInfo[cop].rec_oper == (llOperations)cop) {
            if (funPrereq == NULL || funPrereq(lsn, RecBuf)) {
                RecordsInfo[cop].fun(lsn, RecBuf);
            }
        } else
            for (unsigned int i = 0; i < RecordsInfoLen; i++)
                if (RecordsInfo[i].rec_oper == (llOperations)cop || RecordsInfo[i].rec_oper == LL_DEFAULT) {
                    if (funPrereq == NULL || funPrereq(lsn, RecBuf)) {
                        RecordsInfo[i].fun(lsn, RecBuf);
                    }
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
    LSN hb_lsn = LFS_INVALID_LSN;

    llFileHead file_head;

    if (llInfo->next_arch_file == 0 && llNeedHbRecord) { // increment mode off
        hb_lsn = llLogHotBackup(); // store hb-specific info
    }

    llLock();

    lfsGetHeader(&file_head, sizeof(llFileHead));

    file_head.is_archive = true;
    file_head.next_arch_file = 0;

    if (llInfo->next_arch_file == 0 && llNeedHbRecord) { // increment mode off
        file_head.hb_lsn = hb_lsn;
    }

    num = lfsArchiveCurrentFile(&file_head, sizeof(llFileHead));

    // write header for a new file if needed
    if (llInfo->next_arch_file == 0 && llNeedHbRecord) { // increment mode off
        lfsGetHeader(&file_head, sizeof(llFileHead));
        file_head.hb_lsn = hb_lsn;
        lfsWriteHeader(&file_head, sizeof(llFileHead));
        llNeedHbRecord = false;
    }

    llUnlock();

    return num;
}

// Returns previous lsn for given record
LSN llGetPrevLsnFromRecord(void *Rec)
{
    llRecordHead *RecHead;

    if (Rec == NULL) {
        return LFS_INVALID_LSN;
    }

    RecHead = (llRecordHead *)((char *)Rec - sizeof(llRecordHead));

    return RecHead->prev_lsn;
}

// Check if we need to do checkpoint for maintenance reason.
bool llNeedCheckpoint()
{
    // this call is safe because lfsGetNumberOfFiles() is thread-safe
    return lfsGetNumberOfFiles() > LL_MAX_LOG_FILES;
}

size_t llGetMaxRecordSize()
{
    return LL_WRITEBUF_SIZE - sizeof(llRecordHead);
}

/*
 * Mem copy with offset increment, without any checkings.
 *
 * offs WILL be modified, unless len == 0, of course.
 */
inline static void inc_mem_copy(void* dest, size_t &offs, const void* src, size_t len)
{
    if (len == 0) {
        return;
    }
    memcpy((char*)dest + offs, src, len);
    offs += len;
}

/*
 * Temporary record buffer; it is expanded if needed.
 *
 * Initial size of intermediate record buffer is REC_BUF_INIT_SIZE bytes.
 *
 * NOTE: this buffer is never freed since its lifetime starts with the first
 * record and ends with the last, which almost equals to lifetime of the process.
 */
#define REC_BUF_INIT_SIZE 128
static unsigned char* rec_buf = NULL;
static size_t rec_buf_size = 0;

LSN llLogGeneral(llRecordType rtype, transaction_id trid, llOperations op,
                 bool ret_next_lsn, unsigned num, ...)
{
    va_list args;
    void *field;
    size_t max_len, field_len, rec_len = 0;
    char cop = (char)op;
    LSN rec_lsn;

    RECOVERY_CRASH;

    max_len = llGetMaxRecordSize();

    U_ASSERT(max_len >= sizeof(cop) + sizeof(trid));

    if (!rec_buf) {
        rec_buf = (unsigned char *)malloc(REC_BUF_INIT_SIZE);
        if (!rec_buf) {
            LL_ERROR("logical log: not enough memory");
        }

        rec_buf_size = REC_BUF_INIT_SIZE;
    }

    inc_mem_copy(rec_buf, rec_len, &cop, sizeof(cop));

    if (rtype == TR_RECORD) {
        inc_mem_copy(rec_buf, rec_len, &trid, sizeof(trid));
    }

    va_start(args, num);

    for (unsigned i = 0; i < num; i++) {
        field = va_arg(args, void *);
        field_len = va_arg(args, size_t);

        if (rec_len + field_len > max_len) {
            throw USER_EXCEPTION(SE4156);
        }

        if (rec_len + field_len > rec_buf_size) {
            rec_buf_size = s_max(rec_buf_size * 2, rec_len + field_len);
            rec_buf = (unsigned char *)realloc(rec_buf, rec_buf_size);

            if (!rec_buf) {
                LL_ERROR("logical log: not enough memory");
            }
        }

        inc_mem_copy(rec_buf, rec_len, field, field_len);
    }

    //insert record
    if ((rec_lsn = llInsertRecord(rtype, rec_buf, rec_len, trid)) == LFS_INVALID_LSN) {
        LL_ERROR("logical log: internal transaction error");
    }

    va_end(args);

    if (ret_next_lsn) {
        rec_lsn += llGetRecordSize(NULL, rec_len);
    }

    return rec_lsn;
}
