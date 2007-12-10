/*
 * File:  base.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <fstream>
#include "common/base.h"
#include "common/u/ugnames.h"
#include "common/u/uhdd.h"
#include "common/u/uprocess.h"
#include "common/errdbg/d_printf.h"
#include "common/u/uutils.h"


using namespace std;

void  *LAYER_ADDRESS_SPACE_START_ADDR     = NULL;
void  *LAYER_ADDRESS_SPACE_BOUNDARY       = NULL;
void  *PH_ADDRESS_SPACE_START_ADDR        = NULL;
__uint32 LAYER_ADDRESS_SPACE_START_ADDR_INT = 0;
__uint32 LAYER_ADDRESS_SPACE_BOUNDARY_INT   = 0;
__uint32 PH_ADDRESS_SPACE_START_ADDR_INT    = 0;

__uint32 LAYER_ADDRESS_SPACE_SIZE           = 0;

__uint32 LAYER_ADDRESS_SPACE_SIZE           = 0; //0x30000000;
#endif



#define _GOVERNOR_SHARED_MEMORY_NAME			 0
#define _CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME	 (_GOVERNOR_SHARED_MEMORY_NAME + 1)
#define _CHARISMA_ITFE_SHARED_MEMORY_NAME		 (_CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME + MAX_DBS_NUMBER)

#ifdef _WIN32
#define _CHARISMA_PH_SHARED_MEMORY_NAME			 (_CHARISMA_ITFE_SHARED_MEMORY_NAME + MAX_DBS_NUMBER)

#define _CHARISMA_PH_1_SNP_SHARED_MEMORY_NAME	 (_CHARISMA_PH_SHARED_MEMORY_NAME + MAX_DBS_NUMBER)
#define _CHARISMA_PH_0_SNP_SHARED_MEMORY_NAME  (_CHARISMA_PH_1_SNP_SHARED_MEMORY_NAME + MAX_DBS_NUMBER)

#define _CHARISMA_BUFFER_SHARED_MEMORY_NAME		 (_CHARISMA_PH_0_SNP_SHARED_MEMORY_NAME + MAX_DBS_NUMBER)
#define _SM_TO_VMM_CALLBACK_SEM1_BASE_STR		 (_CHARISMA_BUFFER_SHARED_MEMORY_NAME + MAX_DBS_NUMBER)
#else
#define _CHARISMA_PH_SHARED_MEMORY_NAME			 NULL
#define _CHARISMA_BUFFER_SHARED_MEMORY_NAME		 "/CharismaBufferSharedMemory"
#define _SM_TO_VMM_CALLBACK_SEM1_BASE_STR		 (_CHARISMA_ITFE_SHARED_MEMORY_NAME + MAX_DBS_NUMBER)
#endif


#define _SM_TO_VMM_CALLBACK_SEM2_BASE_STR		 (_SM_TO_VMM_CALLBACK_SEM1_BASE_STR + UPPER_SESSIONS_NUM_BOUND)
#define _VMM_SM_SEMAPHORE_STR				 (_SM_TO_VMM_CALLBACK_SEM2_BASE_STR + UPPER_SESSIONS_NUM_BOUND)
#define _INDIRECTION_TABLE_SEMAPHORE_STR		 (_VMM_SM_SEMAPHORE_STR + MAX_DBS_NUMBER)
#define _VMM_SM_EXCLUSIVE_MODE_SEM_STR			 (_INDIRECTION_TABLE_SEMAPHORE_STR + MAX_DBS_NUMBER)
#define _PERS_HEAP_SEMAPHORE_STR			 (_VMM_SM_EXCLUSIVE_MODE_SEM_STR + MAX_DBS_NUMBER)

#define _PERS_HEAP_1_SNP_SEMAPHORE_STR     (_PERS_HEAP_SEMAPHORE_STR + MAX_DBS_NUMBER)
#define _PERS_HEAP_0_SNP_SEMAPHORE_STR     (_PERS_HEAP_1_SNP_SEMAPHORE_STR + MAX_DBS_NUMBER)

#define _SNAPSHOT_CHECKPOINT_EVENT         (_PERS_HEAP_0_SNP_SEMAPHORE_STR + MAX_DBS_NUMBER)
#define _TRY_ADVANCE_SNAPSHOT_EVENT        (_SNAPSHOT_CHECKPOINT_EVENT + MAX_DBS_NUMBER)

#define _METADATA_SEMAPHORE_STR				 (_TRY_ADVANCE_SNAPSHOT_EVENT + MAX_DBS_NUMBER)
#define _INDEX_SEMAPHORE_STR				 (_METADATA_SEMAPHORE_STR + MAX_DBS_NUMBER)
#define _CHARISMA_SSMMSG_SM_ID				 (_INDEX_SEMAPHORE_STR + MAX_DBS_NUMBER)
#define _CHARISMA_SM_SMSD_ID				 (_CHARISMA_SSMMSG_SM_ID + MAX_DBS_NUMBER)
#define _CHARISMA_SM_IS_READY				 (_CHARISMA_SM_SMSD_ID + MAX_DBS_NUMBER)
#define _CHARISMA_GOVERNOR_IS_READY			 (_CHARISMA_SM_IS_READY + MAX_DBS_NUMBER)
#define _PHYS_LOG_SHARED_MEM_NAME			 (_CHARISMA_GOVERNOR_IS_READY + 1)
#define _PHYS_LOG_PROTECTION_SEMAPHORE_NAME		 (_PHYS_LOG_SHARED_MEM_NAME + MAX_DBS_NUMBER)
#define _CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME		 (_PHYS_LOG_PROTECTION_SEMAPHORE_NAME + MAX_DBS_NUMBER)
#define _CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME	 (_CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME + MAX_DBS_NUMBER)
#define _CHARISMA_SM_WAIT_FOR_SHUTDOWN			 (_CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME + MAX_DBS_NUMBER)
#define _CHARISMA_CHECKPOINT_SEM			 (_CHARISMA_SM_WAIT_FOR_SHUTDOWN + MAX_DBS_NUMBER)
#define _SEDNA_CHECKPOINT_FINISHED_SEM			 (_CHARISMA_CHECKPOINT_SEM + MAX_DBS_NUMBER)
#define _SEDNA_TRNS_FINISHED				 (_SEDNA_CHECKPOINT_FINISHED_SEM + MAX_DBS_NUMBER)  
#define _CHARISMA_WAIT_FOR_CHECKPOINT			 (_SEDNA_TRNS_FINISHED + MAX_DBS_NUMBER)
#define _CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG		 (_CHARISMA_WAIT_FOR_CHECKPOINT + MAX_DBS_NUMBER)
#define _CHARISMA_SYNC_TRN_IDS_TABLE			 (_CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG + MAX_DBS_NUMBER)
#define _CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME		 (_CHARISMA_SYNC_TRN_IDS_TABLE + MAX_DBS_NUMBER)
#define _SEDNA_LOCK_MANAGER_SEM				 (_CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME + MAX_DBS_NUMBER)
#define _SEDNA_TRANSACTION_LOCK				 (_SEDNA_LOCK_MANAGER_SEM + MAX_DBS_NUMBER)
#define _SE_EVENT_LOG_SHARED_MEMORY_NAME		 (_SEDNA_TRANSACTION_LOCK + UPPER_SESSIONS_NUM_BOUND)
#define _SE_EVENT_LOG_SEMAPHORES_NAME			 (_SE_EVENT_LOG_SHARED_MEMORY_NAME + 1)

#ifdef SE_ENABLE_FTSEARCH
#define _FT_INDEX_SEMAPHORE_STR				 (_SE_EVENT_LOG_SEMAPHORES_NAME + 1)
#endif
#ifdef SE_ENABLE_TRIGGERS
#define _TRIGGER_SEMAPHORE_STR				 (_SE_EVENT_LOG_SEMAPHORES_NAME + 1 + MAX_DBS_NUMBER)
#endif

#ifdef _WIN32
#define _SEDNA_GLOBAL_MEMORY_MAPPING                     "SEDNA_GLOBAL_MEMORY_MAPPING"
#else 
#define _SEDNA_GLOBAL_MEMORY_MAPPING                     "/SEDNA_GLOBAL_MEMORY_MAPPING"
#endif



global_name CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME;
global_name CHARISMA_ITFE_SHARED_MEMORY_NAME;

char *CHARISMA_PH_SHARED_MEMORY_NAME;

char *CHARISMA_PH_1_SNP_SHARED_MEMORY_NAME;
char *CHARISMA_PH_0_SNP_SHARED_MEMORY_NAME;

char *CHARISMA_BUFFER_SHARED_MEMORY_NAME;
char *SEDNA_GLOBAL_MEMORY_MAPPING;

global_name VMM_SM_SEMAPHORE_STR;
global_name INDIRECTION_TABLE_SEMAPHORE_STR;
global_name VMM_SM_EXCLUSIVE_MODE_SEM_STR;
global_name PERS_HEAP_SEMAPHORE_STR;

global_name PERS_HEAP_1_SNP_SEMAPHORE_STR;
global_name PERS_HEAP_0_SNP_SEMAPHORE_STR;

global_name SNAPSHOT_CHECKPOINT_EVENT;
global_name TRY_ADVANCE_SNAPSHOT_EVENT;

global_name METADATA_SEMAPHORE_STR;
global_name INDEX_SEMAPHORE_STR;
#ifdef SE_ENABLE_FTSEARCH
global_name FT_INDEX_SEMAPHORE_STR;
#endif
#ifdef SE_ENABLE_TRIGGERS
global_name TRIGGER_SEMAPHORE_STR;
#endif


global_name CHARISMA_GOVERNOR_IS_READY;
global_name PHYS_LOG_SHARED_MEM_NAME;
global_name PHYS_LOG_PROTECTION_SEMAPHORE_NAME;
global_name CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME;
global_name CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME;
global_name CHARISMA_SM_WAIT_FOR_SHUTDOWN;
global_name CHARISMA_CHECKPOINT_SEM;
global_name SEDNA_CHECKPOINT_FINISHED_SEM;
global_name SEDNA_TRNS_FINISHED;
global_name CHARISMA_WAIT_FOR_CHECKPOINT;
global_name CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG;
global_name CHARISMA_SYNC_TRN_IDS_TABLE;
global_name CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME;
global_name GOVERNOR_SHARED_MEMORY_NAME;
global_name SEDNA_LOCK_MANAGER_SEM;
global_name SE_EVENT_LOG_SHARED_MEMORY_NAME;
global_name SE_EVENT_LOG_SEMAPHORES_NAME;
global_name CHARISMA_SM_SMSD_ID;
global_name CHARISMA_SM_IS_READY;

/*	global names */ 

#define POLICY_SINGLETON()				NULL,1
#define POLICY_INSTANCE_PER_DB()		"DB",(MAX_DBS_NUMBER)
#define POLICY_INSTANCE_PER_SESSION()	"SES",(UPPER_SESSIONS_NUM_BOUND)

/*	If you want to add a new global IPC object, do it here.
	We are going to extract basenames automaticly, so don't 
	try anything funny below and preserve markers. */ 
static UGlobalNamesRegistryItem globalNamesRegistry[] =
{
	/* {% GlobalNamesRegistry */ 
	{"SHMEM_GLOBAL",					POLICY_SINGLETON()}, /* vmm region info + VMM placeholder when no buffer mapped */ 
	
	{"SHMEM_GOV",						POLICY_SINGLETON()}, /* holding system state and config info (gov_config_struct) */ 
	
	{"SHMEM_EVENT_LOG",					POLICY_SINGLETON()}, /* event logger */ 
	{"SEMAR_EVENT_LOG",				POLICY_SINGLETON()}, /* event logger  */ 

	{"SHMEM_BUFFERS",					POLICY_INSTANCE_PER_DB()}, /* buffer memory */  
	{"SEMAP_BUFMGR_EXCL_MODE",			POLICY_INSTANCE_PER_DB()}, /* regulates the exclusive mode entering by TRN */ 
	{"SHMEM_BUFFERS_LRU",				POLICY_INSTANCE_PER_DB()}, /* LRU stats on buffers usage */ 

	{"SHMEM_SM_TALK",					POLICY_INSTANCE_PER_DB()}, /* used by shared memory-based SM messaging interface */ 
	{"SEMAR_SM_TALK",					POLICY_INSTANCE_PER_DB()}, /* used by shared memory-based SM messaging interface */ 

	{"SEMAP_VMM_INIT",					POLICY_INSTANCE_PER_DB()}, /* VMM initialisation is serialised with this sem */ 
	{"SHMEM_VMM_CALLBACK_PARAMS",		POLICY_INSTANCE_PER_DB()}, /* parameters passed to VMM calback */ 
	{"EVENT_VMM_CALLBACK",				POLICY_INSTANCE_PER_SESSION()}, /* VMM callback thread waits on it */ 
	{"EVENT_VMM_CALLBACK_COMPLETED",	POLICY_INSTANCE_PER_SESSION()}, /* sem for callback thread to signal call completion */ 

	{"SHMEM_PERS_HEAP",					POLICY_INSTANCE_PER_DB()}, /* shared memory for persistent heap */ 
	{"SEMAP_PERS_HEAP",					POLICY_INSTANCE_PER_DB()}, /* serialises memory allocation in PH */ 
	
	{"SHMEM_PERS_HEAP_SNAPSHOT_0",		POLICY_INSTANCE_PER_DB()}, /* PH for snapshot with id #0 */ 
	{"SEMAP_PERS_HEAP_SNAPSHOT_0",		POLICY_INSTANCE_PER_DB()}, /* dummy sem */ 

	{"SHMEM_PERS_HEAP_SNAPSHOT_1",		POLICY_INSTANCE_PER_DB()}, /* PH for snapshot with id #1 */ 
	{"SEMAP_PERS_HEAP_SNAPSHOT_1",		POLICY_INSTANCE_PER_DB()}, /* dummy sem */ 

	{"SHMEM_INDIRECTION",				POLICY_INSTANCE_PER_DB()}, /* indirection table free entry (?) */ 
	{"SEMAP_INDIRECTION",					POLICY_INSTANCE_PER_DB()}, /* sync of some kind */ 

	{"SEMAP_METADATA",					POLICY_INSTANCE_PER_DB()}, /* synchronises access to metadata registry in PH */ 

	{"SEMAP_INDICES",						POLICY_INSTANCE_PER_DB()}, /* synchronises access to indices registry in PH */ 

	{"SEMAP_FT_INDICES",					POLICY_INSTANCE_PER_DB()}, /* synchronises access to full-text indices registry in PH */ 

	{"SEMAP_TRIGGERS",					POLICY_INSTANCE_PER_DB()}, /* synchronises access to triggers registry in PH */ 

	{"SEMAP_LOCKMGR",						POLICY_INSTANCE_PER_DB()}, /* serialises requests to lock manager (in SM) */ 
	{"EVENT_LOCK_GRANTED",				POLICY_INSTANCE_PER_SESSION()}, /* if transaction request for a lock on DB entity is not satisfied immediately trn waits until the event is signalled (hence if transaction enters the wait state, it can't become a victim for the deadlock-resolution process) */ 

	{"SEMAP_TRN_REGULATION",				POLICY_INSTANCE_PER_DB()}, /* currently if checkpoint is active no updater transactions are allowed and vice-versa (earlier mutual exclusion applied to micro-ops but not transactions) */ 
	{"EVENT_NEW_JOB_4_CHECKPOINT_THREAD", POLICY_INSTANCE_PER_DB()}, /* signals that a checkpoint must be activated or snapshots must be advanced */ 
	{"EVENT_READONLY_TRN_COMPLETED",	POLICY_INSTANCE_PER_DB()}, /* signals read-only transaction completion */ 

	{"SHMEM_PHYSICAL_LOG",				POLICY_INSTANCE_PER_DB()}, /* physical log state & buffer in shared memory */ 
	{"SEMAP_PHYSICAL_LOG",				POLICY_INSTANCE_PER_DB()}, /* synchronises operation with physical log */ 

	{"SHMEM_LOGICAL_LOG",				POLICY_INSTANCE_PER_DB()}, /* logical log state & buffer in shared memory */ 
	{"SEMAP_LOGICAL_LOG",					POLICY_INSTANCE_PER_DB()}, /* synchronises operation with logical log */ 

	{"EVENT_SM_SHUTDOWN_COMMAND",		POLICY_INSTANCE_PER_DB()}, /* signaled by SSMMsg thread in SM when shutdown command arrives via messaging interface */ 

	{"EVENT_RECOVERY_COMPLETED",		POLICY_INSTANCE_PER_DB()}, /* signaled when se_rcv completes the recovery */ 

	{"SEMAP_TRNS_TABLE",					POLICY_INSTANCE_PER_DB()}, /* synchronises access to transactions table in SM */ 

	{"EVENT_SM_READY",					POLICY_INSTANCE_PER_DB()}, /* used to signal initialisation completion when starting SM in background mode */ 

	{"EVENT_GOV_READY",					POLICY_SINGLETON()}, /* used to signal initialisation completion when starting GOV in background mode */ 

	/* %} */ 
	{NULL}
};

void InitGlobalNames(int rangeBegin, int rangeEnd)
{
	UInitGlobalNamesRegistry(globalNamesRegistry, NULL, rangeBegin, rangeEnd);
}

void ReleaseGlobalNames()
{
	UReleaseGlobalNamesRegistry();
}

global_name CreateNameOfSmTalk(int databaseId, char *buf, size_t bufSize)
{
	const char *namesVec[2];
	char bufa[128], bufb[128];

	namesVec[0] = UCreateGlobalName("SHMEM_SM_TALK", databaseId, bufa, 128);
	namesVec[1] = UCreateGlobalName("SEMAR_SM_TALK", databaseId, bufb, 128);
	
	return UCreateCompoundName(namesVec, 2, buf, bufSize);
}

global_name CreateNameOfEventVmmCalback(int sessionId, char *buf, size_t bufSize)
{
	return UCreateGlobalName("EVENT_VMM_CALLBACK", sessionId, buf, bufSize);
}

global_name CreateNameOfEventVmmCalbackCompleted(int sessionId, char *buf, size_t bufSize)
{
	return UCreateGlobalName("EVENT_VMM_CALLBACK_COMPLETED", sessionId, buf, bufSize);
}

global_name CreateNameOfEventLockGranted(int sessionId, char *buf, size_t bufSize)
{
	return UCreateGlobalName("EVENT_LOCK_GRANTED", sessionId, buf, bufSize);
}

/* empty string is invalid as global name but NULL is valid, so we use empty string as initializer  */ 
global_name GOVERNOR_SHARED_MEMORY_NAME = "";
global_name CHARISMA_GOVERNOR_IS_READY = "";
global_name SE_EVENT_LOG_SHARED_MEMORY_NAME = "";
global_name SE_EVENT_LOG_SEMAPHORES_NAME = "";
global_name SEDNA_GLOBAL_MEMORY_MAPPING = "";

void SetGlobalNames()
{
	static char 
		GOVERNOR_SHARED_MEMORY_NAME__buf__		[128],
		CHARISMA_GOVERNOR_IS_READY__buf__		[128],
		SE_EVENT_LOG_SHARED_MEMORY_NAME__buf__	[128],
		SE_EVENT_LOG_SEMAPHORES_NAME__buf__		[128],
		SEDNA_GLOBAL_MEMORY_MAPPING__buf__		[128];

	GOVERNOR_SHARED_MEMORY_NAME = 
		UCreateGlobalName("SHMEM_GOV", 0, GOVERNOR_SHARED_MEMORY_NAME__buf__, 128);

	CHARISMA_GOVERNOR_IS_READY = 
		UCreateGlobalName("EVENT_GOV_READY", 0, CHARISMA_GOVERNOR_IS_READY__buf__, 128);

	SE_EVENT_LOG_SHARED_MEMORY_NAME = 
		UCreateGlobalName("SHMEM_EVENT_LOG", 0, SE_EVENT_LOG_SHARED_MEMORY_NAME__buf__, 128);

	SE_EVENT_LOG_SEMAPHORES_NAME = 
		UCreateGlobalName("SEMAR_EVENT_LOG", 0, SE_EVENT_LOG_SEMAPHORES_NAME__buf__, 128);

	SEDNA_GLOBAL_MEMORY_MAPPING = 
		UCreateGlobalName("SHMEM_GLOBAL", 0, SEDNA_GLOBAL_MEMORY_MAPPING__buf__, 128);
}

/* empty string is invalid as global name but NULL is valid, so we use empty string as initializer  */ 
global_name CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME = "";
global_name CHARISMA_ITFE_SHARED_MEMORY_NAME = "";
global_name CHARISMA_BUFFER_SHARED_MEMORY_NAME = "";
global_name CHARISMA_PH_SHARED_MEMORY_NAME = "";
global_name CHARISMA_PH_1_SNP_SHARED_MEMORY_NAME = "";
global_name CHARISMA_PH_0_SNP_SHARED_MEMORY_NAME = "";
global_name VMM_SM_SEMAPHORE_STR = "";
global_name INDIRECTION_TABLE_SEMAPHORE_STR = "";
global_name VMM_SM_EXCLUSIVE_MODE_SEM_STR = "";
global_name PERS_HEAP_SEMAPHORE_STR = "";
global_name PERS_HEAP_1_SNP_SEMAPHORE_STR = "";
global_name PERS_HEAP_0_SNP_SEMAPHORE_STR = "";
global_name SNAPSHOT_CHECKPOINT_EVENT = "";
global_name TRY_ADVANCE_SNAPSHOT_EVENT = "";
global_name METADATA_SEMAPHORE_STR = "";
global_name INDEX_SEMAPHORE_STR = "";
global_name FT_INDEX_SEMAPHORE_STR = "";
global_name TRIGGER_SEMAPHORE_STR = "";
global_name PHYS_LOG_SHARED_MEM_NAME = "";
global_name PHYS_LOG_PROTECTION_SEMAPHORE_NAME = "";
global_name CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME = "";
global_name CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME = "";
global_name CHARISMA_CHECKPOINT_SEM = "";
global_name SEDNA_CHECKPOINT_FINISHED_SEM = "";
global_name SEDNA_TRNS_FINISHED = "";
global_name CHARISMA_WAIT_FOR_CHECKPOINT = "";
global_name CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG = "";
global_name CHARISMA_SYNC_TRN_IDS_TABLE = "";
global_name CHARISMA_SM_WAIT_FOR_SHUTDOWN = "";
global_name CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME = "";
global_name SEDNA_LOCK_MANAGER_SEM = "";
global_name CHARISMA_SM_SMSD_ID = "";
global_name CHARISMA_SM_IS_READY = "";

void SetGlobalNamesDB(int databaseId)
{
	static char
		CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME__buf__  [128],
		CHARISMA_ITFE_SHARED_MEMORY_NAME__buf__			[128],
		CHARISMA_BUFFER_SHARED_MEMORY_NAME__buf__		[128],
		CHARISMA_PH_SHARED_MEMORY_NAME__buf__			[128],
		CHARISMA_PH_1_SNP_SHARED_MEMORY_NAME__buf__		[128],
		CHARISMA_PH_0_SNP_SHARED_MEMORY_NAME__buf__		[128],
		VMM_SM_SEMAPHORE_STR__buf__						[128],
		INDIRECTION_TABLE_SEMAPHORE_STR__buf__			[128],
		VMM_SM_EXCLUSIVE_MODE_SEM_STR__buf__			[128],
		PERS_HEAP_SEMAPHORE_STR__buf__					[128],
		PERS_HEAP_1_SNP_SEMAPHORE_STR__buf__			[128],
		PERS_HEAP_0_SNP_SEMAPHORE_STR__buf__			[128],
		SNAPSHOT_CHECKPOINT_EVENT__buf__				[128],
		TRY_ADVANCE_SNAPSHOT_EVENT__buf__				[128],
		METADATA_SEMAPHORE_STR__buf__					[128],
		INDEX_SEMAPHORE_STR__buf__						[128],
		FT_INDEX_SEMAPHORE_STR__buf__					[128],
		TRIGGER_SEMAPHORE_STR__buf__					[128],
		PHYS_LOG_SHARED_MEM_NAME__buf__					[128],
		PHYS_LOG_PROTECTION_SEMAPHORE_NAME__buf__		[128],
		CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME__buf__		[128],
		CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME__buf__	[128],
		SEDNA_TRNS_FINISHED__buf__						[128],
		CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG__buf__		[128],
		CHARISMA_SYNC_TRN_IDS_TABLE__buf__				[128],
		CHARISMA_SM_WAIT_FOR_SHUTDOWN__buf__			[128],
		CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME__buf__	[128],
		SEDNA_LOCK_MANAGER_SEM__buf__					[128],
		CHARISMA_SM_IS_READY__buf__						[128];

	CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME = 
		UCreateGlobalName("SHMEM_VMM_CALLBACK_PARAMS", databaseId, CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME__buf__, 128);

	CHARISMA_ITFE_SHARED_MEMORY_NAME =
		UCreateGlobalName("SHMEM_INDIRECTION", databaseId, CHARISMA_ITFE_SHARED_MEMORY_NAME__buf__, 128);

	CHARISMA_BUFFER_SHARED_MEMORY_NAME =
		UCreateGlobalName("SHMEM_BUFFERS", databaseId, CHARISMA_BUFFER_SHARED_MEMORY_NAME__buf__, 128);

	CHARISMA_PH_SHARED_MEMORY_NAME =
		UCreateGlobalName("SHMEM_PERS_HEAP", databaseId, CHARISMA_PH_SHARED_MEMORY_NAME__buf__, 128);

	CHARISMA_PH_1_SNP_SHARED_MEMORY_NAME =
		UCreateGlobalName("SHMEM_PERS_HEAP_SNAPSHOT_1", databaseId, CHARISMA_PH_1_SNP_SHARED_MEMORY_NAME__buf__, 128);

	CHARISMA_PH_0_SNP_SHARED_MEMORY_NAME =
		UCreateGlobalName("SHMEM_PERS_HEAP_SNAPSHOT_0", databaseId, CHARISMA_PH_0_SNP_SHARED_MEMORY_NAME__buf__, 128);

	VMM_SM_SEMAPHORE_STR =
		UCreateGlobalName("SEMAP_VMM_INIT", databaseId, VMM_SM_SEMAPHORE_STR__buf__, 128);

	INDIRECTION_TABLE_SEMAPHORE_STR =
		UCreateGlobalName("SEMAP_INDIRECTION", databaseId, INDIRECTION_TABLE_SEMAPHORE_STR__buf__, 128);

	VMM_SM_EXCLUSIVE_MODE_SEM_STR =
		UCreateGlobalName("SEMAP_BUFMGR_EXCL_MODE", databaseId, VMM_SM_EXCLUSIVE_MODE_SEM_STR__buf__, 128);

	PERS_HEAP_SEMAPHORE_STR =
		UCreateGlobalName("SEMAP_PERS_HEAP", databaseId, PERS_HEAP_SEMAPHORE_STR__buf__, 128);

	PERS_HEAP_1_SNP_SEMAPHORE_STR =
		UCreateGlobalName("SEMAP_PERS_HEAP_SNAPSHOT_1", databaseId, PERS_HEAP_1_SNP_SEMAPHORE_STR__buf__, 128);

	PERS_HEAP_0_SNP_SEMAPHORE_STR =
		UCreateGlobalName("SEMAP_PERS_HEAP_SNAPSHOT_0", databaseId, PERS_HEAP_0_SNP_SEMAPHORE_STR__buf__, 128);

	SNAPSHOT_CHECKPOINT_EVENT =
		UCreateGlobalName("EVENT_NEW_JOB_4_CHECKPOINT_THREAD", databaseId, SNAPSHOT_CHECKPOINT_EVENT__buf__, 128);

	TRY_ADVANCE_SNAPSHOT_EVENT =
		UCreateGlobalName("EVENT_READONLY_TRN_COMPLETED", databaseId, TRY_ADVANCE_SNAPSHOT_EVENT__buf__, 128);

	METADATA_SEMAPHORE_STR =
		UCreateGlobalName("SEMAP_METADATA", databaseId, METADATA_SEMAPHORE_STR__buf__, 128);

	INDEX_SEMAPHORE_STR =
		UCreateGlobalName("SEMAP_INDICES", databaseId, INDEX_SEMAPHORE_STR__buf__, 128);

	FT_INDEX_SEMAPHORE_STR =
		UCreateGlobalName("SEMAP_FT_INDICES", databaseId, FT_INDEX_SEMAPHORE_STR__buf__, 128);

	TRIGGER_SEMAPHORE_STR =
		UCreateGlobalName("SEMAP_TRIGGERS", databaseId, TRIGGER_SEMAPHORE_STR__buf__, 128);

	PHYS_LOG_SHARED_MEM_NAME =
		UCreateGlobalName("SHMEM_PHYSICAL_LOG", databaseId, PHYS_LOG_SHARED_MEM_NAME__buf__, 128);

	PHYS_LOG_PROTECTION_SEMAPHORE_NAME =
		UCreateGlobalName("SEMAP_PHYSICAL_LOG", databaseId, PHYS_LOG_PROTECTION_SEMAPHORE_NAME__buf__, 128);

	CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME =
		UCreateGlobalName("SHMEM_LOGICAL_LOG", databaseId, CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME__buf__, 128);

	CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME =
		UCreateGlobalName("SEMAP_LOGICAL_LOG", databaseId, CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME__buf__, 128);

	SEDNA_TRNS_FINISHED =
		UCreateGlobalName("SEMAP_TRN_REGULATION", databaseId, SEDNA_TRNS_FINISHED__buf__, 128);

	CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG =
		UCreateGlobalName("EVENT_RECOVERY_COMPLETED", databaseId, CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG__buf__, 128);

	CHARISMA_SYNC_TRN_IDS_TABLE =
		UCreateGlobalName("SEMAP_TRNS_TABLE", databaseId, CHARISMA_SYNC_TRN_IDS_TABLE__buf__, 128);

	CHARISMA_SM_WAIT_FOR_SHUTDOWN =
		UCreateGlobalName("EVENT_SM_SHUTDOWN_COMMAND", databaseId, CHARISMA_SM_WAIT_FOR_SHUTDOWN__buf__, 128);

	CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME =
		UCreateGlobalName("SHMEM_BUFFERS_LRU", databaseId, CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME__buf__, 128);

	SEDNA_LOCK_MANAGER_SEM =
		UCreateGlobalName("SEMAP_LOCKMGR", databaseId, SEDNA_LOCK_MANAGER_SEM__buf__, 128);

	CHARISMA_SM_IS_READY =
		UCreateGlobalName("EVENT_SM_READY", databaseId, CHARISMA_SM_IS_READY__buf__, 128);
};
