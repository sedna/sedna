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
#include "common/u/uprocess.h"

#define SEDNA_DATA_STRUCTURES_VER 4

// buffer memory offset; this type is used for addressing buffers in buffer
// memory area by defining offset of buffer from the beginning of the shared
// memory
typedef int ramoffs;
#define RAMOFFS_OUT_OFF_BOUNDS                          INT_MAX

typedef __int64 LSN;
//typedef long LSN;
typedef __int64 LONG_LSN;
typedef __uint32 CP_counter;
#define  NULL_LSN (-1)
#define  NULL_FILE (-1)

#define MAX_FILE_SIZE_WITHOUT_CHECKPOINT 50*(1024*1024)


typedef int session_id;

/**
 * Transaction identifier
 */
typedef int transaction_id;

#define MAX_RESOURCE_NAME_LENGTH 100


typedef int index_id;

typedef __int16 xmlscm_type;


/* shift in the block */
typedef unsigned short int shft;

#define PAGE_SIZE								65536
#define PAGE_BIT_MASK								(__uint32)0xFFFF0000
#define PAGE_REVERSE_BIT_MASK							(__uint32)0x0000FFFF
//#define PAGE_SIZE								4096
//#define PAGE_BIT_MASK								0xFFFFF000
//#define PAGE_REVERSE_BIT_MASK							0x00000FFF


extern void  *LAYER_ADDRESS_SPACE_START_ADDR;
extern void  *LAYER_ADDRESS_SPACE_BOUNDARY;
extern void  *PH_ADDRESS_SPACE_START_ADDR;
extern __uint32 LAYER_ADDRESS_SPACE_START_ADDR_INT;
extern __uint32 LAYER_ADDRESS_SPACE_BOUNDARY_INT;
extern __uint32 PH_ADDRESS_SPACE_START_ADDR_INT;

extern __uint32 LAYER_ADDRESS_SPACE_SIZE;

struct vmm_region_values
{
    __uint32 LAYER_ADDRESS_SPACE_START_ADDR_INT;
    __uint32 LAYER_ADDRESS_SPACE_BOUNDARY_INT;
    __uint32 PH_ADDRESS_SPACE_START_ADDR_INT;
    __uint32 LAYER_ADDRESS_SPACE_SIZE;
};

/*#ifdef _WIN32
#define VMM_REGION_SEARCH_LEFT_BOUND                    ((__uint32)0x20000000)
#define VMM_REGION_SEARCH_RIGHT_BOUND                   ((__uint32)0x80000000)
#elif defined(DARWIN)
#define VMM_REGION_SEARCH_LEFT_BOUND                    ((__uint32)0x20000000)
#define VMM_REGION_SEARCH_RIGHT_BOUND                   ((__uint32)0x68000000)
#else
#define VMM_REGION_SEARCH_LEFT_BOUND                    ((__uint32)0x40000000)
#define VMM_REGION_SEARCH_RIGHT_BOUND                   ((__uint32)0xB0000000)
#endif*/
#define VMM_REGION_SEARCH_MAX_SIZE                      ((__uint32)0x80000000)
#define PH_SIZE                                         ((__uint32)0x6400000)
#define VMM_REGION_MIN_SIZE                             ((__uint32)0x4000000)
#define VMM_REGION_MAX_SIZE                             ((__uint32)0x40000000)



#ifdef _WIN32
#define SESSION_EXE "se_trn.exe"
//#define SEDNA_GLOBAL_MEMORY_MAPPING                     "SEDNA_GLOBAL_MEMORY_MAPPING"
#else 
#define SESSION_EXE "se_trn"
//#define SEDNA_GLOBAL_MEMORY_MAPPING                     "/SEDNA_GLOBAL_MEMORY_MAPPING"
#endif


#define MODULES_COLLECTION_NAME "$modules"

/*
#ifdef _WIN32

// in MBs
#define LAYER_ADDRESS_SPACE_SIZE						768
#define LAYER_ADDRESS_SPACE_SIZE_IN_BYTES				0x30000000


#define LAYER_ADDRESS_SPACE_START_ADDR					((void*)0x30000000)
#define LAYER_ADDRESS_SPACE_BOUNDARY					((void*)0x60000000)
#define PH_ADDRESS_SPACE_START_ADDR						((void*)0x2BC00000)
#define LAYER_ADDRESS_SPACE_START_ADDR_INT				0x30000000
#define LAYER_ADDRESS_SPACE_BOUNDARY_INT				0x60000000
#define PH_ADDRESS_SPACE_START_ADDR_INT					0x2BC00000

#else

// in MBs
#define LAYER_ADDRESS_SPACE_SIZE						768
#define LAYER_ADDRESS_SPACE_SIZE_IN_BYTES				0x30000000

#define LAYER_ADDRESS_SPACE_START_ADDR					((void*)0x60000000)
#define LAYER_ADDRESS_SPACE_BOUNDARY					((void*)0x90000000)
#define PH_ADDRESS_SPACE_START_ADDR						((void*)0x59C00000)
#define LAYER_ADDRESS_SPACE_START_ADDR_INT				((__uint32)0x60000000)
#define LAYER_ADDRESS_SPACE_BOUNDARY_INT				((__uint32)0x90000000)
#define PH_ADDRESS_SPACE_START_ADDR_INT					((__uint32)0x59C00000)

#endif
*/

#define IS_PH_PTR(p)									(PH_ADDRESS_SPACE_START_ADDR <= (p) && (p) < LAYER_ADDRESS_SPACE_START_ADDR)



extern char *CHARISMA_PH_SHARED_MEMORY_NAME;

extern char *CHARISMA_PH_NEW_SNP_SHARED_MEMORY_NAME;
extern char *CHARISMA_PH_OLD_SNP_SHARED_MEMORY_NAME;

extern char *CHARISMA_BUFFER_SHARED_MEMORY_NAME;
extern char *SEDNA_GLOBAL_MEMORY_MAPPING;

extern global_name CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME;
extern global_name CHARISMA_ITFE_SHARED_MEMORY_NAME;

global_name SM_TO_VMM_CALLBACK_SEM1_BASE_STR(session_id id, int os_primitives_id_min_bound, char* buf, int size);
global_name SM_TO_VMM_CALLBACK_SEM2_BASE_STR(session_id id, int os_primitives_id_min_bound, char* buf, int size);
global_name SEDNA_TRANSACTION_LOCK(session_id s_id, int os_primitives_id_min_bound, char* buf, int size);
global_name CHARISMA_SSMMSG_SM_ID(int db_id, int os_primitives_id_min_bound, char* buf, int size );

extern global_name VMM_SM_SEMAPHORE_STR;
extern global_name INDIRECTION_TABLE_SEMAPHORE_STR;
extern global_name VMM_SM_EXCLUSIVE_MODE_SEM_STR;
extern global_name PERS_HEAP_SEMAPHORE_STR;

extern global_name PERS_HEAP_1_SNP_SEMAPHORE_STR;
extern global_name PERS_HEAP_0_SNP_SEMAPHORE_STR;

extern global_name METADATA_SEMAPHORE_STR;
extern global_name INDEX_SEMAPHORE_STR;
#ifdef SE_ENABLE_FTSEARCH
extern global_name FT_INDEX_SEMAPHORE_STR;
#endif
#ifdef SE_ENABLE_TRIGGERS
extern global_name TRIGGER_SEMAPHORE_STR;
#endif

#define TRIGGER_MAX_CASCADING_LEVEL						10

#define SM_NUMBER_OF_SERVER_THREADS						1


//gov's SSMMsg shared memory name 
extern global_name CHARISMA_SSMMSG_GOV_ID;
#define GOV_NUMBER_OF_SERVER_THREADS 		            1

//notify stop_serv that server shutdown completely
extern global_name CHARISMA_GOVERNOR_IS_READY;

extern global_name PHYS_LOG_SHARED_MEM_NAME;
extern global_name PHYS_LOG_PROTECTION_SEMAPHORE_NAME;	

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


#define SEDNA_DETERMINE_VMM_REGION						"SEDNA_DETERMINE_VMM_REGION"
//#define CHARISMA_DB_NAME                                "CHARISMA_DB_NAME"
#define CONNECTION_SOCKET_HANDLE                        "CONNECTION_SOCKET_HANDLE"
#define SEDNA_SERVER_MODE                               "SEDNA_SERVER_MODE"

#define SEDNA_LOAD_METADATA_TRANSACTION					"SEDNA_LOAD_METADATA_TRANSACTION"

#define SEDNA_OS_PRIMITIVES_ID_MIN_BOUND		"SEDNA_OS_PRIMITIVES_ID_MIN_BOUND"

#define SECURITY_METADATA_DOCUMENT						"$db_security_data"
#define INITIAL_SECURITY_METADATA_FILE_NAME				"sedna_auth_md.xml"

#define CHARISMA_MAX_TRNS_NUMBER                        50
#define MAX_SESSIONS_NUMBER								50
#define MAX_DBS_NUMBER									10
#define STRMAXSIZE   4000000000lu


void set_global_names(int os_primitives_id_min_bound);
void set_global_names(int os_primitives_id_min_bound, int db_id);
#define UPPER_SESSIONS_NUM_BOUND			100

extern FILE* res_os;

/**
 * Type of query that executed by query processor
 */
enum QueryType {TL_XQuery	= 9,	// XQuery query
                TL_ForSemAnal   = 8,    // representation for semantic analiz
                TL_ForAuth      = 7,    // representation for authentication processing
                TL_ForMarkLRet	= 4,	// representation for mark LReturns
                TL_ForConvToPOR	= 2,	// representation for converting to POR
                TL_POR 		= 1	// POR query
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
 * 38 - bm_get_snapshot_info
 *
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
            void *mptr; // pointer for persistent_db_data
        } reg;

        __int64 ptr; // xptr for deletion, locking and unlocking

        struct {
            __int64 ptr;
            __int64 swapped;
            int offs;
        } swap_data;

        struct {
        	__int64 ts;    // timestamp of snapshot, used to find persistent heap file
        	int type_of_snp; // 1 or 0, to select name for file mapping
        } snp_info;

        sm_blk_stat stat; // sm block statistics

        char data[2 + MAX_RESOURCE_NAME_LENGTH]; // first byte->lock mode, second byte->resource type, other bytes->resource name
 
    } data;
};


/// the following parameters are related to kernel<-->transaction protocol
#define  ERR_SYMBOL     ((char)254)
#define  DELIM_SYMBOL   ((char)250)
#define  EOD_SYMBOL     ((char)253)
#define  EOALL_SYMBOL   ((char)252) 

// definitions for governor

enum commands {CREATE_NEW_SESSION = 110, STOP = 501, REGISTER_NEW_SESSION = 121, REGISTER_DB = 122, RUNTIME_CONFIG = 600, IS_RUN_SM = 888 };


#endif
