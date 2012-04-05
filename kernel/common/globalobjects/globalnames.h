#ifndef _GLOBAL_NAMES_H
#define _GLOBAL_NAMES_H

#include "u/ugnames.h"

/*
 * global_name
 *     Interprocess name for IPC resources
 */

typedef const char *global_name;

/* Global Names section */

void InitGlobalNames(int rangeBegin, int rangeEnd);

void ReleaseGlobalNames();

global_name CreateNameOfSmTalk(int databaseId, char *buf, size_t bufSize);

global_name CreateNameOfEventVmmCalback(int sessionId, char *buf, size_t bufSize);

global_name CreateNameOfEventVmmCalbackCompleted(int sessionId, char *buf, size_t bufSize);

global_name CreateNameOfEventLockGranted(int sessionId, char *buf, size_t bufSize);

void SetGlobalNames();

void SetGlobalNamesDB(int databaseId);

#define SM_TO_VMM_CALLBACK_SEM1_BASE_STR(SID,BUF,BUFSZ) \
        CreateNameOfEventVmmCalback((SID),(BUF),(BUFSZ))

#define SM_TO_VMM_CALLBACK_SEM2_BASE_STR(SID,BUF,BUFSZ) \
        CreateNameOfEventVmmCalbackCompleted((SID),(BUF),(BUFSZ))

#define SEDNA_TRANSACTION_LOCK(SID,BUF,BUFSZ) \
        CreateNameOfEventLockGranted((SID),(BUF),(BUFSZ))

#define CHARISMA_SSMMSG_SM_ID(DB,BUF,BUFSZ) \
        CreateNameOfSmTalk((DB),(BUF),(BUFSZ))

extern global_name CHARISMA_BUFFER_SHARED_MEMORY_NAME;
extern global_name SEDNA_GLOBAL_MEMORY_MAPPING;
extern global_name CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME;
extern global_name VMM_SM_SEMAPHORE_STR;
extern global_name VMM_SM_EXCLUSIVE_MODE_SEM_STR;
extern global_name SNAPSHOT_CHECKPOINT_EVENT;
extern global_name TRY_ADVANCE_SNAPSHOT_EVENT;

extern global_name CATALOG_NAMETABLE_SEMAPHORE_STR;
extern global_name CATALOG_MASTER_SEMAPHORE_STR;

#ifdef SE_ENABLE_FTSEARCH
//extern global_name FT_INDEX_SEMAPHORE_STR;
#endif

#ifdef SE_ENABLE_TRIGGERS
//extern global_name TRIGGER_SEMAPHORE_STR;
#endif

extern global_name CHARISMA_SSMMSG_GOV_ID;
extern global_name CHARISMA_GOVERNOR_IS_READY;
extern global_name SEDNA_LFS_SEM_NAME;
extern global_name SEDNA_LFS_SHARED_MEM_NAME;
extern global_name CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME;
extern global_name CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME;
extern global_name CHARISMA_SM_WAIT_FOR_SHUTDOWN;
extern global_name CHARISMA_CHECKPOINT_SEM;
extern global_name SEDNA_CHECKPOINT_FINISHED_SEM;
extern global_name SEDNA_TRNS_FINISHED;
extern global_name CHARISMA_WAIT_FOR_CHECKPOINT;
extern global_name CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG;
extern global_name CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME;
extern global_name GOVERNOR_SHARED_MEMORY_NAME;
extern global_name SE_EVENT_LOG_SHARED_MEMORY_NAME;
extern global_name SE_EVENT_LOG_SEMAPHORES_NAME;
extern global_name CHARISMA_SM_SMSD_ID;
extern global_name CHARISMA_SM_IS_READY;

#endif /* _GLOBAL_NAMES_H */