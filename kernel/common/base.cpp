/*
 * File:  base.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <fstream>
#include "base.h"
#include "uhdd.h"
#include "uprocess.h"
#include "d_printf.h"


using namespace std;

#ifdef _WIN32
void  *LAYER_ADDRESS_SPACE_START_ADDR     = NULL; //((void*)0x30000000);
void  *LAYER_ADDRESS_SPACE_BOUNDARY       = NULL; //((void*)0x60000000);
void  *PH_ADDRESS_SPACE_START_ADDR        = NULL; //((void*)0x2BC00000);
__uint32 LAYER_ADDRESS_SPACE_START_ADDR_INT = 0; //((__uint32)0x30000000);
__uint32 LAYER_ADDRESS_SPACE_BOUNDARY_INT   = 0; //((__uint32)0x60000000);
__uint32 PH_ADDRESS_SPACE_START_ADDR_INT    = 0; //((__uint32)0x2BC00000);

__uint32 LAYER_ADDRESS_SPACE_SIZE           = 0; //0x30000000;
#else
void  *LAYER_ADDRESS_SPACE_START_ADDR     = NULL; //((void*)0x60000000);
void  *LAYER_ADDRESS_SPACE_BOUNDARY       = NULL; //((void*)0x90000000);
void  *PH_ADDRESS_SPACE_START_ADDR        = NULL; //((void*)0x59C00000);
__uint32 LAYER_ADDRESS_SPACE_START_ADDR_INT = 0; //((__uint32)0x60000000);
__uint32 LAYER_ADDRESS_SPACE_BOUNDARY_INT   = 0; //((__uint32)0x90000000);
__uint32 PH_ADDRESS_SPACE_START_ADDR_INT    = 0; //((__uint32)0x59C00000);

__uint32 LAYER_ADDRESS_SPACE_SIZE           = 0; //0x30000000;
#endif



#ifdef _WIN32
/// Windows
#define _CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME	"CharismaSMCallbackSharedMemory"
#define _CHARISMA_ITFE_SHARED_MEMORY_NAME			"CharismaITFESharedMemory"

#define _CHARISMA_PH_SHARED_MEMORY_NAME				"CharismaPHSharedMemory"
#define _CHARISMA_BUFFER_SHARED_MEMORY_NAME			"CharismaBufferSharedMemory"

#define _SM_TO_VMM_CALLBACK_SEM1_BASE_STR			"CharismaSMtoVMMcallbackSem1_"
#define _SM_TO_VMM_CALLBACK_SEM2_BASE_STR			"CharismaSMtoVMMcallbackSem2_"

#define _VMM_SM_SEMAPHORE_STR						"CharismaVMM_SM_Semaphore"
#define _INDIRECTION_TABLE_SEMAPHORE_STR			"CharismaIndirectionTableSemaphore"
#define _VMM_SM_EXCLUSIVE_MODE_SEM_STR				"CharismaVMM_SM_ExclusiveModeSem"
#define _PERS_HEAP_SEMAPHORE_STR					"CharismaPersHeapSemaphore"
#define _METADATA_SEMAPHORE_STR						"CharismaMetadataSemaphore"
#define _INDEX_SEMAPHORE_STR					    "CharismaIndexSemaphore"
#ifdef SE_ENABLE_FTSEARCH
#define _FT_INDEX_SEMAPHORE_STR					    "CharismaFTIndexSemaphore"
#endif
#define _CHARISMA_SSMMSG_SM_ID						"CHARISMASSMMsgSMId"

#define _CHARISMA_SM_SMSD_ID						"CHARISMASMSMSDID"



#define _CHARISMA_SSMMSG_GOV_ID						"CHARISMASSMMsgGOVId"
#define _CHARISMA_SSMMSG_LSTNR_SERVER_ID			"CHARISMA_SSMMSG_LSTNR_SERVER_ID"
#define _CHARISMA_GOV_LSTNR_SEM_ID					"CHARISMA_GOV_LSTNR_SEM_ID"

#define _CHARISMA_STOP_GOV							"CHARISMA_STOP_GOV"
#define _CHARISMA_GOV_WAIT_FOR_SHUTDOWN				"CHARISMA_GOV_WAIT_FOR_SHUTDOWN"
#define _CHARISMA_GOV_SYNC_SES_TABLE				"CHARISMA_GOV_SYNC_SES_TABLE"
#define _CHARISMA_GOV_SYNC_DB_TABLE					"CHARISMA_GOV_SYNC_DB_TABLE"

#define _CHARISMA_SM_IS_READY						"CHARISMA_SM_IS_READY_"

#define _CHARISMA_GOVERNOR_IS_READY					"CHARISMA_GOVERNOR_IS_READY"
#define _CHARISMA_LISTENER_IS_READY					"CHARISMA_LISTENER_IS_READY"

#define _PHYS_LOG_SHARED_MEM_NAME					"CHARISMA_PHYS_LOG_SHARED_MEM_NAME"

#define _PHYS_LOG_PROTECTION_SEMAPHORE_NAME			"CHARISMA_PHYS_LOG_PROTECTION_SEMAPHORE_NAME"

#define _CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME		"CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME"
#define _CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME	"CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME"

#define _SESS_SHUTDOWN_SEMAPHORE_STR                "CharismaSessionShutdownSemaphore"


#define _CHARISMA_SM_WAIT_FOR_SHUTDOWN				"CHARISMA_SM_WAIT_FOR_SHUTDOWN"

#define _CHARISMA_CHECKPOINT_SEM					"CHARISMA_CHECKPOINT_SEM"
#define _SEDNA_CHECKPOINT_FINISHED_SEM				"SEDNA_CHECKPOINT_FINISHED_SEM"
#define _CHARISMA_LOGICAL_OPERATION_ATOMICITY		"CHARISMA_LOGICAL_OPERATION_ATOMICITY"    
#define _CHARISMA_WAIT_FOR_CHECKPOINT				"CHARISMA_WAIT_FOR_CHECKPOINT"
#define _CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG		"CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG"

#define _CHARISMA_SYNC_TRN_IDS_TABLE				"CHARISMA_SYNC_TRN_IDS_TABLE"

#define _CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME		"CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME"

#define _GOVERNOR_SHARED_MEMORY_NAME				"GOVERNOR_SHARED_MEMORY_NAME"

#define _SEDNA_LOCK_MANAGER_SEM						"SEDNA_LOCK_MANAGER_SEM"
#define _SEDNA_TRANSACTION_LOCK						"SEDNA_TRANSACTION_LOCK"

#define _SE_EVENT_LOG_SHARED_MEMORY_NAME			"SE_EVENT_LOG_SHARED_MEMORY_NAME"
#define _SE_EVENT_LOG_SEMAPHORES_NAME				"SE_EVENT_LOG_SEMAPHORES_NAME"


#else
/// UNIX
#define _CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME	'b'
#define _CHARISMA_ITFE_SHARED_MEMORY_NAME			'c'

#define _CHARISMA_PH_SHARED_MEMORY_NAME				NULL
#define _CHARISMA_BUFFER_SHARED_MEMORY_NAME			"/CharismaBufferSharedMemory"

#define _SM_TO_VMM_CALLBACK_SEM1_BASE_STR
#define _SM_TO_VMM_CALLBACK_SEM2_BASE_STR

#define _VMM_SM_SEMAPHORE_STR						'e'
#define _INDIRECTION_TABLE_SEMAPHORE_STR			'f'
#define _VMM_SM_EXCLUSIVE_MODE_SEM_STR				'g'
#define _PERS_HEAP_SEMAPHORE_STR					'h'
#define _METADATA_SEMAPHORE_STR						'i'
#define _INDEX_SEMAPHORE_STR					    'j'

#define _CHARISMA_SSMMSG_SM_ID						'k'

#define _CHARISMA_SM_SMSD_ID						'l'



#define _CHARISMA_SSMMSG_GOV_ID						'r'
#define _CHARISMA_SSMMSG_LSTNR_SERVER_ID			's'
#define _CHARISMA_GOV_LSTNR_SEM_ID					't'

#define _CHARISMA_STOP_GOV							'u'
#define _CHARISMA_GOV_WAIT_FOR_SHUTDOWN				'v'
#define _CHARISMA_GOV_SYNC_SES_TABLE				'x'
#define _CHARISMA_GOV_SYNC_DB_TABLE					'y'

#define _CHARISMA_SM_IS_READY						'z'

#define _CHARISMA_GOVERNOR_IS_READY					'A'
#define _CHARISMA_LISTENER_IS_READY					'B'

#define _PHYS_LOG_SHARED_MEM_NAME					'C'
#define _PHYS_LOG_PROTECTION_SEMAPHORE_NAME			'D' 

#define _CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME		'E'
#define _CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME	'F'

#define _SESS_SHUTDOWN_SEMAPHORE_STR				'J'


#define _CHARISMA_SM_WAIT_FOR_SHUTDOWN				'K'

#define _CHARISMA_CHECKPOINT_SEM					'L'
#define _CHARISMA_LOGICAL_OPERATION_ATOMICITY		'M'
#define _CHARISMA_WAIT_FOR_CHECKPOINT				'N'
#define _CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG		'O'

#define _CHARISMA_SYNC_TRN_IDS_TABLE				'P'

#define _CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME		'R'

#define _GOVERNOR_SHARED_MEMORY_NAME				'S'

#define _SEDNA_LOCK_MANAGER_SEM						'T'
#define _SEDNA_TRANSACTION_LOCK						'U'
#ifdef SE_ENABLE_FTSEARCH
#define _FT_INDEX_SEMAPHORE_STR					    'V'
#endif

#define _SE_EVENT_LOG_SHARED_MEMORY_NAME			'X'
#define _SE_EVENT_LOG_SEMAPHORES_NAME				'Y'
#define _SEDNA_CHECKPOINT_FINISHED_SEM				'Z'


#endif



global_name CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME;
global_name CHARISMA_ITFE_SHARED_MEMORY_NAME;

char *CHARISMA_PH_SHARED_MEMORY_NAME;
char *CHARISMA_BUFFER_SHARED_MEMORY_NAME;

global_name VMM_SM_SEMAPHORE_STR;
global_name INDIRECTION_TABLE_SEMAPHORE_STR;
global_name VMM_SM_EXCLUSIVE_MODE_SEM_STR;
global_name PERS_HEAP_SEMAPHORE_STR;
global_name METADATA_SEMAPHORE_STR;
global_name INDEX_SEMAPHORE_STR;
#ifdef SE_ENABLE_FTSEARCH
global_name FT_INDEX_SEMAPHORE_STR;
#endif



global_name CHARISMA_SSMMSG_GOV_ID;
global_name CHARISMA_SSMMSG_LSTNR_SERVER_ID;
global_name CHARISMA_GOV_LSTNR_SEM_ID;

global_name CHARISMA_STOP_GOV;
global_name CHARISMA_GOV_WAIT_FOR_SHUTDOWN;
global_name CHARISMA_GOV_SYNC_SES_TABLE;
global_name CHARISMA_GOV_SYNC_DB_TABLE;

global_name CHARISMA_GOVERNOR_IS_READY;
global_name CHARISMA_LISTENER_IS_READY;

global_name PHYS_LOG_SHARED_MEM_NAME;


global_name PHYS_LOG_PROTECTION_SEMAPHORE_NAME;

global_name CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME;
global_name CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME;

global_name CHARISMA_SM_WAIT_FOR_SHUTDOWN;

global_name CHARISMA_CHECKPOINT_SEM;
global_name SEDNA_CHECKPOINT_FINISHED_SEM;
global_name CHARISMA_LOGICAL_OPERATION_ATOMICITY;
global_name CHARISMA_WAIT_FOR_CHECKPOINT;
global_name CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG;

global_name CHARISMA_SYNC_TRN_IDS_TABLE;


global_name CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME;

global_name GOVERNOR_SHARED_MEMORY_NAME;

global_name SEDNA_LOCK_MANAGER_SEM;

global_name SE_EVENT_LOG_SHARED_MEMORY_NAME;
global_name SE_EVENT_LOG_SEMAPHORES_NAME;



char SEDNA_DATA[SEDNA_DATA_VAR_SIZE];
bool is_init_sedna_data = false;
FILE* res_os = stdout; //otput stream of transaction results (result of the user's query)


global_name SM_TO_VMM_CALLBACK_SEM1_BASE_STR(transaction_id id, const char* db_name, char* buf, int size)
{
#ifdef _WIN32
    string tmp = string(_SM_TO_VMM_CALLBACK_SEM1_BASE_STR) + db_name + int2string(id);
    if (tmp.length() > size - 1) throw USER_EXCEPTION(SE1009);
    strcpy(buf, tmp.c_str());
    return buf;
#else
    string path = string(SEDNA_DATA) + "/data/" + db_name + "_files/" + db_name + ".ph";
    key_t key = ftok(path.c_str(), id);
    if (key == (key_t)-1)
        throw USER_ENV_EXCEPTION("Error obtaining key_t", false);
    return key;
#endif
}

global_name SM_TO_VMM_CALLBACK_SEM2_BASE_STR(transaction_id id, const char* db_name, char* buf, int size)
{
#ifdef _WIN32
    string tmp = string(_SM_TO_VMM_CALLBACK_SEM2_BASE_STR) + db_name + int2string(id);
    if (tmp.length() > size - 1) throw USER_EXCEPTION(SE1009);
    strcpy(buf, tmp.c_str());
    return buf;
#else
    string path = string(SEDNA_DATA) + "/data/" + db_name + "_files/" + db_name + ".ph.bu";
    key_t key = ftok(path.c_str(), id);
    if (key == (key_t)-1)
        throw USER_ENV_EXCEPTION("Error obtaining key_t", false);
    return key;
#endif
}

global_name CHARISMA_SSMMSG_SM_ID(const char* db_name, char* buf, int size)
{
#ifdef _WIN32
    string tmp = string(_CHARISMA_SSMMSG_SM_ID) + db_name;
    if (tmp.length() > size - 1) throw USER_EXCEPTION(SE1009);
    strcpy(buf, tmp.c_str());
    return buf;
#else
    string path = string(SEDNA_DATA) + "/data/" + db_name + "_files/" + db_name + ".data";
    key_t key = ftok(path.c_str(), _CHARISMA_SSMMSG_SM_ID);
    if (key == (key_t)-1)
        throw USER_ENV_EXCEPTION("Error obtaining key_t", false);
    return key;
#endif
}

global_name CHARISMA_SM_SMSD_ID(const char* db_name, char* buf, int size)
{
#ifdef _WIN32
    string tmp = string(_CHARISMA_SM_SMSD_ID) + db_name;
    if (tmp.length() > size - 1) throw USER_EXCEPTION(SE1009);
    strcpy(buf, tmp.c_str());
    return buf;
#else
    string path = string(SEDNA_DATA) + "/data/" + db_name + "_files/" + db_name + ".data";
    key_t key = ftok(path.c_str(), _CHARISMA_SM_SMSD_ID);
    if (key == (key_t)-1)
        throw USER_ENV_EXCEPTION("Error obtaining key_t", false);
    return key;
#endif
}


global_name SESS_SHUTDOWN_SEMAPHORE_STR(UPID id, char* buf, int size)
{
#ifdef _WIN32
    string tmp = string(_SESS_SHUTDOWN_SEMAPHORE_STR) + int2string(id);
    if (tmp.length() > size - 1) throw USER_EXCEPTION(SE1009);
    strcpy(buf, tmp.c_str());
    return buf;
#else
    string path = string(SEDNA_DATA);
    key_t key = ftok(path.c_str(), id);
    if (key == (key_t)-1)
        throw USER_ENV_EXCEPTION("Error obtaining key_t", false);
    return key;
#endif
}


global_name CHARISMA_SM_IS_READY(const char* db_name, char* buf, int size)
{
#ifdef _WIN32
    string tmp = string(_CHARISMA_SM_IS_READY) + db_name;
    if (tmp.length() > size - 1) throw USER_EXCEPTION(SE1009);
    strcpy(buf, tmp.c_str());
    return buf;
#else
    string path = string(SEDNA_DATA) + "/data/" + db_name + "_files/" + db_name + ".data";
    key_t key = ftok(path.c_str(), _CHARISMA_SM_IS_READY);
    if (key == (key_t)-1)
        throw USER_ENV_EXCEPTION("Error obtaining key_t", false);
    return key;
#endif
}

global_name SEDNA_TRANSACTION_LOCK(session_id s_id, const char* db_name, char* buf, int size)
{
#ifdef _WIN32
    string tmp = string(_SEDNA_TRANSACTION_LOCK) + string(db_name) + int2string(s_id);
    if (tmp.length() > size - 1) throw USER_EXCEPTION(SE1009);
    strcpy(buf, tmp.c_str());
    return buf;
#else
    string path = string(SEDNA_DATA) + "/cfg/" + db_name + "_cfg.xml";
    key_t key = ftok(path.c_str(), s_id);
    if (key == (key_t)-1)
        throw USER_ENV_EXCEPTION("Error obtaining key_t", false);
    return key;
#endif
}


#define WIN_GN_INIT1(c)		tmp = string(_##c) + db_name;									\
                     		c = new char[tmp.length() + 1];									\
                     		strcpy((char*)c, tmp.c_str());

#define WIN_GN_INIT2(c)		tmp = string(_##c);												\
                     		c = new char[tmp.length() + 1];									\
                     		strcpy((char*)c, tmp.c_str());

#define WIN_GN_INIT3(c)		if (_##c)														\
                            {																\
                                tmp = string(_##c) + db_name;								\
                     		    c = new char[tmp.length() + 1];								\
                     		    strcpy((char*)c, tmp.c_str());								\
                            }																\
                            else c = NULL;

#define UNIX_GN_INIT1(c)	key = ftok(path1.c_str(), _##c);								\
                      		if (key == (key_t)-1)											\
                       			throw USER_ENV_EXCEPTION("Error obtaining key_t", false);	\
                       		c = key;

#define UNIX_GN_INIT2(c)	key = ftok(path2.c_str(), _##c);								\
                      		if (key == (key_t)-1)											\
                            {																\
                                d_perror("ftok");											\
                       			throw USER_ENV_EXCEPTION("Error obtaining key_t", false);	\
                            }																\
                       		c = key;

#define UNIX_GN_INIT3(c)	if (_##c)														\
                            {																\
                                tmp = string(_##c) + db_name;								\
                     		    c = new char[tmp.length() + 1];								\
                     		    strcpy((char*)c, tmp.c_str());								\
                            }																\
                            else c = NULL;


void set_global_names()
{
    set_sedna_data();

    //s_printf2("SEDNA_DATA = %s\n", SEDNA_DATA);

#ifdef _WIN32
    string tmp;

    WIN_GN_INIT2(CHARISMA_SSMMSG_GOV_ID);
    WIN_GN_INIT2(CHARISMA_SSMMSG_LSTNR_SERVER_ID);
    WIN_GN_INIT2(CHARISMA_GOV_LSTNR_SEM_ID);

    WIN_GN_INIT2(CHARISMA_STOP_GOV);
    WIN_GN_INIT2(CHARISMA_GOV_SYNC_SES_TABLE);

    WIN_GN_INIT2(CHARISMA_GOVERNOR_IS_READY);
    WIN_GN_INIT2(CHARISMA_LISTENER_IS_READY);

    WIN_GN_INIT2(CHARISMA_GOV_WAIT_FOR_SHUTDOWN);
    
    WIN_GN_INIT2(GOVERNOR_SHARED_MEMORY_NAME);

    WIN_GN_INIT2(SE_EVENT_LOG_SHARED_MEMORY_NAME);
    WIN_GN_INIT2(SE_EVENT_LOG_SEMAPHORES_NAME);

#else
    key_t key;
    string path2 = string(SEDNA_DATA);


    UNIX_GN_INIT2(CHARISMA_SSMMSG_GOV_ID);
    UNIX_GN_INIT2(CHARISMA_SSMMSG_LSTNR_SERVER_ID);
    UNIX_GN_INIT2(CHARISMA_GOV_LSTNR_SEM_ID);

    UNIX_GN_INIT2(CHARISMA_STOP_GOV);
    UNIX_GN_INIT2(CHARISMA_GOV_SYNC_SES_TABLE);
    UNIX_GN_INIT2(CHARISMA_GOV_SYNC_DB_TABLE); 

    UNIX_GN_INIT2(CHARISMA_GOVERNOR_IS_READY);
    UNIX_GN_INIT2(CHARISMA_LISTENER_IS_READY);

    UNIX_GN_INIT2(CHARISMA_GOV_WAIT_FOR_SHUTDOWN);

    UNIX_GN_INIT2(GOVERNOR_SHARED_MEMORY_NAME);

    UNIX_GN_INIT2(SE_EVENT_LOG_SHARED_MEMORY_NAME);
    UNIX_GN_INIT2(SE_EVENT_LOG_SEMAPHORES_NAME);
#endif
}

void set_global_names(const char *db_name, bool must_exist)
{
    string tmp;
#ifdef _WIN32
    WIN_GN_INIT1(CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME);
    WIN_GN_INIT1(CHARISMA_ITFE_SHARED_MEMORY_NAME);

    WIN_GN_INIT3(CHARISMA_BUFFER_SHARED_MEMORY_NAME);
    WIN_GN_INIT3(CHARISMA_PH_SHARED_MEMORY_NAME);

    WIN_GN_INIT1(VMM_SM_SEMAPHORE_STR);
    WIN_GN_INIT1(INDIRECTION_TABLE_SEMAPHORE_STR);
    WIN_GN_INIT1(VMM_SM_EXCLUSIVE_MODE_SEM_STR);
    WIN_GN_INIT1(PERS_HEAP_SEMAPHORE_STR);
    WIN_GN_INIT1(METADATA_SEMAPHORE_STR);
    WIN_GN_INIT1(INDEX_SEMAPHORE_STR);
#ifdef SE_ENABLE_FTSEARCH
    WIN_GN_INIT1(FT_INDEX_SEMAPHORE_STR);
#endif

    WIN_GN_INIT1(PHYS_LOG_SHARED_MEM_NAME);
    WIN_GN_INIT1(PHYS_LOG_PROTECTION_SEMAPHORE_NAME);

    WIN_GN_INIT1(CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME);
    WIN_GN_INIT1(CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME);

    WIN_GN_INIT1(CHARISMA_CHECKPOINT_SEM);
    WIN_GN_INIT1(SEDNA_CHECKPOINT_FINISHED_SEM);
    WIN_GN_INIT1(CHARISMA_LOGICAL_OPERATION_ATOMICITY);
    WIN_GN_INIT1(CHARISMA_WAIT_FOR_CHECKPOINT);
    WIN_GN_INIT1(CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG);

    WIN_GN_INIT1(CHARISMA_SYNC_TRN_IDS_TABLE);

    WIN_GN_INIT1(CHARISMA_SM_WAIT_FOR_SHUTDOWN);

    WIN_GN_INIT1(CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME);

    WIN_GN_INIT1(SEDNA_LOCK_MANAGER_SEM);
#else
    key_t key;
    string path1 = string(SEDNA_DATA) + "/data/" + db_name + "_files/" + db_name + ".data";
    if (must_exist)
    {
        //////////// CHECK IF THE DATABASE ALREADY EXISTS //////////////////////
        string path2 = string(SEDNA_DATA);
        d_printf2("path1 = %s\n", path1.c_str());
        d_printf2("path2 = %s\n", path2.c_str());
        if (!(uIsFileExist(path1.c_str(), __sys_call_error) && uIsFileExist(path2.c_str(), __sys_call_error)))
            throw USER_EXCEPTION2(SE4200, db_name);
        ////////////////////////////////////////////////////////////////////////
    }

    UNIX_GN_INIT1(CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME);
    UNIX_GN_INIT1(CHARISMA_ITFE_SHARED_MEMORY_NAME);

    UNIX_GN_INIT3(CHARISMA_BUFFER_SHARED_MEMORY_NAME);
    UNIX_GN_INIT3(CHARISMA_PH_SHARED_MEMORY_NAME);

    UNIX_GN_INIT1(VMM_SM_SEMAPHORE_STR);
    UNIX_GN_INIT1(INDIRECTION_TABLE_SEMAPHORE_STR);
    UNIX_GN_INIT1(VMM_SM_EXCLUSIVE_MODE_SEM_STR);
    UNIX_GN_INIT1(PERS_HEAP_SEMAPHORE_STR);
    UNIX_GN_INIT1(METADATA_SEMAPHORE_STR);
    UNIX_GN_INIT1(INDEX_SEMAPHORE_STR);
#ifdef SE_ENABLE_FTSEARCH
    UNIX_GN_INIT1(FT_INDEX_SEMAPHORE_STR);
#endif


    UNIX_GN_INIT1(PHYS_LOG_SHARED_MEM_NAME);
    UNIX_GN_INIT1(PHYS_LOG_PROTECTION_SEMAPHORE_NAME);

    UNIX_GN_INIT1(CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME);
    UNIX_GN_INIT1(CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME);

    UNIX_GN_INIT1(CHARISMA_CHECKPOINT_SEM);
    UNIX_GN_INIT1(SEDNA_CHECKPOINT_FINISHED_SEM);
    UNIX_GN_INIT1(CHARISMA_LOGICAL_OPERATION_ATOMICITY);
    UNIX_GN_INIT1(CHARISMA_WAIT_FOR_CHECKPOINT);
    UNIX_GN_INIT1(CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG);

    UNIX_GN_INIT1(CHARISMA_SM_WAIT_FOR_SHUTDOWN);

    UNIX_GN_INIT1(CHARISMA_SYNC_TRN_IDS_TABLE);

    UNIX_GN_INIT1(CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME);

    UNIX_GN_INIT1(SEDNA_LOCK_MANAGER_SEM);
#endif
}

void set_sedna_data()
{
  if (is_init_sedna_data)
     return;


  char proc_buf[U_MAX_PATH + 1];
  std::string proc_path = uGetImageProcPath(proc_buf, __sys_call_error);
  std::string sedna_cfg_file;
  bool is_inside_lib = true;

  //copy default values
#ifdef _WIN32
  strcpy(SEDNA_DATA, (proc_path + "\\..").c_str());
#else
  strcpy(SEDNA_DATA, "/var/lib/sedna");
#endif

#ifdef _WIN32
  sedna_cfg_file = proc_path + "\\.." + "\\etc\\sednaconf.xml";
#else
  sedna_cfg_file = proc_path + "/.." + "/etc/sednaconf.xml";
#endif
  d_printf2("sedna_cfg_file=%s\n", sedna_cfg_file.c_str());
  std::fstream fs(sedna_cfg_file.c_str());
  std::string cfg_text;
  if (fs.is_open())  
  {//exist sednaconf.xml in etc directory
     d_printf1("exist sednaconf.xml in local etc\n");
     char buf;
     while (!fs.eof())
     {
        fs.get(buf);
        if (!fs.eof()) cfg_text += buf;
     }
     fs.close();

     strcpy(SEDNA_DATA, get_sedna_data_path(cfg_text).c_str());
     is_inside_lib = true;
  }
  else
  {
#ifndef _WIN32 //UNIX
     sedna_cfg_file = "/etc/sednaconf.xml";
     fstream fs_(sedna_cfg_file.c_str());
     if (fs_.is_open())  
     {//exist sednaconf.xml in etc directory

        char buf;
        while (!fs_.eof())
        {
           fs_.get(buf);
           if (!fs_.eof()) cfg_text += buf;
        }
        fs_.close();

        strcpy(SEDNA_DATA, get_sedna_data_path(cfg_text).c_str());
        is_inside_lib = true;
     }
#endif       
  }

  d_printf2("SEDNA_DATA=%s\n", SEDNA_DATA);

#ifndef _WIN32
  if (is_inside_lib)
  {
      USECURITY_ATTRIBUTES sa = U_SEDNA_DIRECTORY_ACCESS_PERMISSIONS_MASK;
      if (uMkDir(SEDNA_DATA, &sa, __sys_call_error) == 0)
          throw USER_EXCEPTION2(SE4300, SEDNA_DATA);
  }
#endif  

  is_init_sedna_data = true;
}
