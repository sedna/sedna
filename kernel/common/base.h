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

#define SEDNA_DATA_STRUCTURES_VER 9

// buffer memory offset; this type is used for addressing buffers in buffer
// memory area by defining offset of buffer from the beginning of the shared
// memory
typedef int ramoffs;
#define RAMOFFS_OUT_OFF_BOUNDS                          INT_MAX

//typedef __int64 LSN;
//typedef long LSN;
//typedef __int64 LONG_LSN;
typedef __uint32 CP_counter;
//#define  NULL_LSN (-1)
#define  NULL_FILE (-1)

#define MAX_FILE_SIZE_WITHOUT_CHECKPOINT 50*(1024*1024)


typedef int session_id;

/**
 * Transaction identifier
 */
typedef int transaction_id;

#define MAX_RESOURCE_NAME_LENGTH                100
#define MAX_DATABASE_NAME_LENGTH                100


/* shift in the block */
typedef unsigned short int shft;

#define PAGE_SIZE                               65536
#define PAGE_BIT_SIZE                           16
#define PAGE_BIT_MASK                           (__uint32)0xFFFF0000
#define PAGE_REVERSE_BIT_MASK                   (__uint32)0x0000FFFF


extern void  *LAYER_ADDRESS_SPACE_START_ADDR;
extern void  *LAYER_ADDRESS_SPACE_BOUNDARY;
extern __uint32 LAYER_ADDRESS_SPACE_START_ADDR_INT;
extern __uint32 LAYER_ADDRESS_SPACE_BOUNDARY_INT;

extern __uint32 LAYER_ADDRESS_SPACE_SIZE;

struct vmm_region_values
{
    __uint32 LAYER_ADDRESS_SPACE_START_ADDR_INT;
    __uint32 LAYER_ADDRESS_SPACE_BOUNDARY_INT;
    __uint32 LAYER_ADDRESS_SPACE_SIZE;
};

#define VMM_REGION_SEARCH_MAX_SIZE                      ((__uint32)0x79C00000)
#define VMM_REGION_MIN_SIZE                             ((__uint32)0x4000000)
#define VMM_REGION_MAX_SIZE                             ((__uint32)0x40000000)

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
extern global_name CHARISMA_SYNC_TRN_IDS_TABLE;
extern global_name CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME;
extern global_name GOVERNOR_SHARED_MEMORY_NAME;
extern global_name SEDNA_LOCK_MANAGER_SEM;
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

#define SECURITY_METADATA_DOCUMENT                      "$db_security_data"
#define INITIAL_SECURITY_METADATA_FILE_NAME             "sedna_auth_md.xml"

#define CHARISMA_MAX_TRNS_NUMBER                        50
#define MAX_SESSIONS_NUMBER								50
#define MAX_DBS_NUMBER                                  10

typedef uint64_t strsize_t;

#define STRMAXSIZE                                      4000000000lu

#define UPPER_SESSIONS_NUM_BOUND                        100

void check_db_name_validness(const char* name);

extern FILE* res_os;

/* Hot-Backup states and answers in messages */
enum hb_state
{
	HB_START,      			// start hot-backup
	HB_START_CHECKPOINT,	// start hot-backup with preceding checkpoint
	HB_START_INCR,          // make primary increment copy
	HB_ADD_INCR,            // archive another portion of the db
	HB_STOP_INCR,           // disable increment mode for the given database
	HB_NONE_INCR,           // non-increment mode
	HB_CONT,                // sm answer: can continue
	HB_WAIT,       			// answer: wait for checkpoint to finish
	HB_ARCHIVELOG, 			// archive logical log (switch to the next one)
	HB_END,        			// end of the hot-backup process
	HB_ERR,       			// some error from sm
	HB_NEXTFILE,            // file request from hbp
	HB_GETPREVLOG           // get previous log file number
};

/**
 *
 * cmd list:
 * =========
 * response list:
 * ~~~~~~~~~~~~~~
 * 0 - success
 * otherwise (int > 0) - fail
 *
 * query list:
 * ~~~~~~~~~~~
 * 10 - soft shutdown
 * 11 - hard shutdown
 *
 * 21 - bm_register_session
 * 22 - bm_unregister_session
 * 23 - bm_allocate_data_block (xptr, offs)
 * 24 - bm_allocate_tmp_block (xptr, offs)
 * 25 - bm_delete_block (xptr)
 * 26 - bm_get_block (xptr, offs)
 * 27 - bm_enter_exclusive_mode (num)
 * 28 - bm_exit_exclusive_mode
 * 29 - bm_memlock_block (xptr)
 * 30 - bm_memunlock_block (xptr)
 * 31 - bm_block_statistics (sm_blk_stat)
 * 32 - bm_pseudo_allocate_data_block (xptr)
 * 33 - bm_pseudo_delete_data_block (xptr)
 * 34 - bm_delete_tmp_blocks
 * 35 - bm_register_transaction
 * 36 - bm_unregister_transaction
 * 37 - bm_create_new_version
 * 38 - transaction rollback
 * 39 - hot-backup procedure (receive: state, return: status, or log file numbers; use hb_struct for this)
 */
struct sm_blk_stat
{
    int free_data_blocks_num;
    int free_tmp_blocks_num;
    int used_data_blocks_num;
    int used_tmp_blocks_num;
};

struct sm_msg_struct
{
    // cmd (what do you want to do)
    int cmd;
    // transaction identifier
    transaction_id trid;

    //identifier of session    
    session_id sid;

    // additional parameters
    union {
        struct {
            int num; // number of potentially allocated blocks in call to bm_enter_exclusive_mode
            __int64 mptr; // pointer for persistent_db_data
            int transaction_flags;
        } reg;

        __int64 ptr; // xptr for deletion, locking and unlocking

        struct {
            __int64 ptr;
            __int64 swapped;
            int offs;
        } swap_data;

        struct {
            uint64_t lnumber;
            hb_state state;
            bool is_checkp;
            hb_state incr_state;
            TIMESTAMP ts;
        } hb_struct;

        __int64 snp_ts;   // timestamp of snapshot, not used currently, but may be helpful later

        sm_blk_stat stat; // sm block statistics

        char data[2 + MAX_RESOURCE_NAME_LENGTH]; // first byte->lock mode, second byte->resource type, other bytes->resource name
 
    } data;
};


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
