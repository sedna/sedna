/*
 * File:  base.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _BASE_H
#define _BASE_H

#include <string>
#include <vector>

#include "sedna.h"

#include "utils.h"
#include "uprocess.h"

#define SEDNA_DATA_STRUCTURES_VER 2

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

#define PAGE_SIZE										65536
#define PAGE_BIT_MASK									(__uint32)0xFFFF0000
#define PAGE_REVERSE_BIT_MASK							(__uint32)0x0000FFFF
//#define PAGE_SIZE										4096
//#define PAGE_BIT_MASK									0xFFFFF000
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

#ifdef _WIN32
#define VMM_REGION_SEARCH_LEFT_BOUND                    ((__uint32)0x20000000)
#define VMM_REGION_SEARCH_RIGHT_BOUND                   ((__uint32)0x80000000)
#else
#define VMM_REGION_SEARCH_LEFT_BOUND                    ((__uint32)0x40000000)
#define VMM_REGION_SEARCH_RIGHT_BOUND                   ((__uint32)0xB0000000)
#endif

#define PH_SIZE                                         ((__uint32)0x6400000)
#define VMM_REGION_MIN_SIZE                             ((__uint32)0x4000000)
#define VMM_REGION_MAX_SIZE                             ((__uint32)0x40000000)



#ifdef _WIN32
#define SESSION_EXE "se_trn.exe"
#define SEDNA_GLOBAL_MEMORY_MAPPING                     "SEDNA_GLOBAL_MEMORY_MAPPING"
#else 
#define SESSION_EXE "se_trn"
#define SEDNA_GLOBAL_MEMORY_MAPPING                     "/SEDNA_GLOBAL_MEMORY_MAPPING"
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
extern char *CHARISMA_BUFFER_SHARED_MEMORY_NAME;

extern global_name CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME;
extern global_name CHARISMA_ITFE_SHARED_MEMORY_NAME;

global_name SM_TO_VMM_CALLBACK_SEM1_BASE_STR(transaction_id id, const char* db_name, char* buf, int size);
global_name SM_TO_VMM_CALLBACK_SEM2_BASE_STR(transaction_id id, const char* db_name, char* buf, int size);

extern global_name VMM_SM_SEMAPHORE_STR;
extern global_name INDIRECTION_TABLE_SEMAPHORE_STR;
extern global_name VMM_SM_EXCLUSIVE_MODE_SEM_STR;
extern global_name PERS_HEAP_SEMAPHORE_STR;
extern global_name METADATA_SEMAPHORE_STR;
extern global_name INDEX_SEMAPHORE_STR;
#ifdef SE_ENABLE_FTSEARCH
extern global_name FT_INDEX_SEMAPHORE_STR;
#endif
#ifdef SE_ENABLE_TRIGGERS
extern global_name TRIGGER_SEMAPHORE_STR;
#endif

//sm's SSMMsg shared memory name
global_name CHARISMA_SSMMSG_SM_ID(const char* db_name, char* buf, int size);
#define SM_NUMBER_OF_SERVER_THREADS						1

//notify gov that sm is closed completely
global_name CHARISMA_SM_SMSD_ID(const char* db_name, char* buf, int size);


//notify gov that session is closed completely
global_name SESS_SHUTDOWN_SEMAPHORE_STR(UPID id, char* buf, int size);

global_name CHARISMA_GOV_SESSION_STOP_ID(UPID s_id, char* buf, int size);


//gov's SSMMsg shared memory name 
extern global_name CHARISMA_SSMMSG_GOV_ID;
#define GOV_NUMBER_OF_SERVER_THREADS              1

//notify stop_serv that server shutdown completely
extern global_name CHARISMA_STOP_GOV;

//gov's semaphore to block the main thread (it must be up to shut down gov)
extern global_name CHARISMA_GOV_WAIT_FOR_SHUTDOWN;
//extern global_name CHARISMA_GOV_SYNC_SES_TABLE;
//extern global_name CHARISMA_GOV_SYNC_DB_TABLE;

global_name CHARISMA_SM_IS_READY(const char* db_name, char* buf, int size);

extern global_name CHARISMA_GOVERNOR_IS_READY;

extern global_name PHYS_LOG_SHARED_MEM_NAME;
extern global_name PHYS_LOG_PROTECTION_SEMAPHORE_NAME;	

extern global_name CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME;
extern global_name CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME;

extern global_name CHARISMA_SM_WAIT_FOR_SHUTDOWN;

extern global_name CHARISMA_CHECKPOINT_SEM;
extern global_name SEDNA_CHECKPOINT_FINISHED_SEM;
extern global_name CHARISMA_LOGICAL_OPERATION_ATOMICITY;
extern global_name CHARISMA_WAIT_FOR_CHECKPOINT;
extern global_name CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG;

extern global_name CHARISMA_SYNC_TRN_IDS_TABLE;

extern global_name CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME;

extern global_name GOVERNOR_SHARED_MEMORY_NAME;

extern global_name SEDNA_LOCK_MANAGER_SEM;

extern global_name SE_EVENT_LOG_SHARED_MEMORY_NAME;
extern global_name SE_EVENT_LOG_SEMAPHORES_NAME;


global_name SEDNA_TRANSACTION_LOCK(session_id s_id, const char* db_name,  char* buf, int size);

#define SEDNA_DETERMINE_VMM_REGION						"SEDNA_DETERMINE_VMM_REGION"
//#define CHARISMA_DB_NAME                                "CHARISMA_DB_NAME"
#define CONNECTION_SOCKET_HANDLE                        "CONNECTION_SOCKET_HANDLE"
#define SEDNA_SERVER_MODE                               "SEDNA_SERVER_MODE"

#define SEDNA_LOAD_METADATA_TRANSACTION					"SEDNA_LOAD_METADATA_TRANSACTION"

#define SECURITY_METADATA_DOCUMENT						"db_security_data"
#define INITIAL_SECURITY_METADATA_FILE_NAME				"sedna_auth_md.xml"

#define CHARISMA_MAX_TRNS_NUMBER                        10
#define MAX_SESSIONS_NUMBER								10
#define MAX_DBS_NUMBER									10
#define STRMAXSIZE   4000000000lu


void set_global_names();
void set_global_names(const char *db_name, bool must_exist = false);
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
        int num; // number of potentially allocated blocks in call to bm_enter_exclusive_mode

        void *mptr; // pointer for persistent_db_data

        __int64 ptr; // xptr for deletion, locking and unlocking

        struct {
            __int64 ptr;
            __int64 swapped;
            int offs;
        } swap_data;

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

struct gov_sess_struct
{
   int idfree; //0->not used 1->session in progress 2->session finished
   int stop; //1->stop command; 0->not stop
};


struct gov_dbs_struct
{
   char db_name[SE_MAX_DB_NAME_LENGTH + 1];
   int is_stop; //0->indicates that sm is working, 1->indicates that sm want to stop
   UPID sm_pid;
};

struct gov_header_struct
{
   int is_server_stop;//0->indicates that sedna operates;//1->indicates that sedna want to stop
   int lstnr_port_number;
   UPID gov_pid;
};

#define GOV_SHM_SIZE (sizeof(gov_header_struct) + MAX_DBS_NUMBER*sizeof(gov_dbs_struct) + MAX_SESSIONS_NUMBER*sizeof(gov_sess_struct))

#endif
