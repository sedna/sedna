/*
 * File:  base.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _BASE_H
#define _BASE_H

#include <string>
#include <vector>

#include "common/sedna.h"

#include "common/utils.h"
#include "common/rcv_test.h"
#include "common/wutypes.h"


#define SEDNA_DATA_STRUCTURES_VER 11

// buffer memory offset; this type is used for addressing buffers in buffer
// memory area by defining offset of buffer from the beginning of the shared
// memory
typedef size_t ramoffs;
#define RAMOFFS_OUT_OFF_BOUNDS SIZE_MAX

//typedef __int64 LSN;
//typedef long LSN;
//typedef __int64 LONG_LSN;
typedef uint32_t CP_counter;
//#define  NULL_LSN (-1)
#define  NULL_FILE (-1)

#define MAX_FILE_SIZE_WITHOUT_CHECKPOINT 50*(1024*1024)


// #define SEDNA_NAMESPACE_URI     "http://www.sedna.org/"
#define SEDNA_NAMESPACE_URI     "http://www.modis.ispras.ru/sedna"
#define SEDNA_NAMESPACE_PREFIX  "se"

typedef int32_t session_id;

/**
 * Transaction identifier
 */
typedef int transaction_id;
#define INVALID_TRID ((transaction_id)-1)

#define MAX_RESOURCE_NAME_LENGTH                100
#define MAX_DATABASE_NAME_LENGTH                100


/* shift in the block */
typedef unsigned short int shft;

/*
 * Our page constants.
 *
 * It's hard to define cross-platform constants for 32- and 64- bits architectures,
 * since, for example, 'long' type is 32-bit on win-64. So, we're just using
 * explicit casting to int-pointer type.
 */
#define PAGE_BIT_SIZE                           16
#define PAGE_SIZE                               (1 << PAGE_BIT_SIZE)
#define PAGE_REVERSE_BIT_MASK                   (uintptr_t)(PAGE_SIZE-1)
#define PAGE_BIT_MASK                           (~PAGE_REVERSE_BIT_MASK)

/* For those, who look for: VMM_REGION_* is now in vmm.cpp */

#define VMM_REGION_MIN_SIZE                             ((uint32_t)0x4000000)

#define TR_AUTHENTICATION_FLAG 1
#define TR_AUTHORIZATION_FLAG  2

#ifdef _WIN32
#define SESSION_EXE "se_trn.exe"
#else
#define SESSION_EXE "se_trn"
#endif


#define MODULES_COLLECTION_NAME "$modules"

/*
	Global Names section */

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

#define TRIGGER_MAX_CASCADING_LEVEL                     10

#define SM_NUMBER_OF_SERVER_THREADS                     1

#define GOV_NUMBER_OF_SERVER_THREADS                    1

#define SEDNA_DETERMINE_VMM_REGION                      "SEDNA_DETERMINE_VMM_REGION"
#define CONNECTION_SOCKET_HANDLE                        "CONNECTION_SOCKET_HANDLE"
#define SEDNA_SERVER_MODE                               "SEDNA_SERVER_MODE"

#define SEDNA_LOAD_METADATA_TRANSACTION                 "SEDNA_LOAD_METADATA_TRANSACTION"

#define SEDNA_OS_PRIMITIVES_ID_MIN_BOUND                "SEDNA_OS_PRIMITIVES_ID_MIN_BOUND"

#define SEDNA_RUN_RECOVERY_TRANSACTION                  "SEDNA_RUN_RECOVERY_TRANSACTION"

#define SECURITY_METADATA_DOCUMENT                      "$db_security_data"
#define INITIAL_SECURITY_METADATA_FILE_NAME             "sedna_auth_md.xml"

#define CHARISMA_MAX_TRNS_NUMBER                        50
#define MAX_SESSIONS_NUMBER								50
#define MAX_DBS_NUMBER                                  10

#define STRMAXSIZE                                      4000000000lu

#define UPPER_SESSIONS_NUM_BOUND                        100

void check_db_name_validness(const char* name);

extern FILE* res_os;

/* The following parameters are related to kernel<-->transaction protocol */
#define  ERR_SYMBOL     ((char)254)
#define  DELIM_SYMBOL   ((char)250)
#define  EOD_SYMBOL     ((char)253)
#define  EOALL_SYMBOL   ((char)252)

/* Command definitions for governor */
enum commands
{
    CREATE_NEW_SESSION     = 110,
    STOP                   = 501,
    REGISTER_NEW_SESSION   = 121,
    REGISTER_DB            = 122,
    RUNTIME_CONFIG         = 600,
    HOTBACKUP_START        = 666,
    IS_RUN_SM              = 888
};

/* Defines variouse kinds of se_stop command */
enum stoptype
{
    SE_STOP_NO   = 0, /// sedna operates
    SE_STOP_SOFT = 1, /// wait for transactions are completed
    SE_STOP_HARD = 2  /// attempts to immediately rollback all running transactions.
};

/* The following definitions are related to gov<-->rc protocol */
#define  SE_RC_VALID     ((char)1)
#define  SE_RC_INVALID   ((char)0)
#define  SE_RC_OVERFLOW  ((char)2)

#endif
