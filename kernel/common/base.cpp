/*
 * File:  base.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <fstream>
#include "common/base.h"
#include "common/u/uhdd.h"
#include "common/u/uprocess.h"
#include "common/errdbg/d_printf.h"
#include "common/u/uutils.h"


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



#define _GOVERNOR_SHARED_MEMORY_NAME			 0
#define _CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME	 (_GOVERNOR_SHARED_MEMORY_NAME + 1)
#define _CHARISMA_ITFE_SHARED_MEMORY_NAME		 (_CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME + MAX_DBS_NUMBER)

#ifdef _WIN32
#define _CHARISMA_PH_SHARED_MEMORY_NAME			 (_CHARISMA_ITFE_SHARED_MEMORY_NAME + MAX_DBS_NUMBER)
#define _CHARISMA_BUFFER_SHARED_MEMORY_NAME		 (_CHARISMA_PH_SHARED_MEMORY_NAME + MAX_DBS_NUMBER)
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
#define _METADATA_SEMAPHORE_STR				 (_PERS_HEAP_SEMAPHORE_STR + MAX_DBS_NUMBER)
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
#define _CHARISMA_LOGICAL_OPERATION_ATOMICITY		 (_SEDNA_CHECKPOINT_FINISHED_SEM + MAX_DBS_NUMBER)  
#define _CHARISMA_WAIT_FOR_CHECKPOINT			 (_CHARISMA_LOGICAL_OPERATION_ATOMICITY + MAX_DBS_NUMBER)
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
char *CHARISMA_BUFFER_SHARED_MEMORY_NAME;
char *SEDNA_GLOBAL_MEMORY_MAPPING;

global_name VMM_SM_SEMAPHORE_STR;
global_name INDIRECTION_TABLE_SEMAPHORE_STR;
global_name VMM_SM_EXCLUSIVE_MODE_SEM_STR;
global_name PERS_HEAP_SEMAPHORE_STR;
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
global_name CHARISMA_LOGICAL_OPERATION_ATOMICITY;
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


FILE* res_os = stdout; //otput stream of transaction results (result of the user's query)


global_name SM_TO_VMM_CALLBACK_SEM1_BASE_STR(session_id id, int os_primitives_id_min_bound, char* buf, int size)
{
#ifdef _WIN32
    string tmp = string("SEDNA_GN_PREFIX") +  int2string(os_primitives_id_min_bound + _SM_TO_VMM_CALLBACK_SEM1_BASE_STR + id);
    if (tmp.length() > size - 1) throw USER_EXCEPTION(SE1009);
    strcpy(buf, tmp.c_str());
    return buf;
#else
    return os_primitives_id_min_bound + _SM_TO_VMM_CALLBACK_SEM1_BASE_STR + id;
#endif
}

global_name SM_TO_VMM_CALLBACK_SEM2_BASE_STR(session_id id, int os_primitives_id_min_bound, char* buf, int size)
{
#ifdef _WIN32
    string tmp = string("SEDNA_GN_PREFIX") +  int2string(os_primitives_id_min_bound + _SM_TO_VMM_CALLBACK_SEM2_BASE_STR + id);
    if (tmp.length() > size - 1) throw USER_EXCEPTION(SE1009);
    strcpy(buf, tmp.c_str());
    return buf;
#else
    return os_primitives_id_min_bound + _SM_TO_VMM_CALLBACK_SEM2_BASE_STR + id;
#endif
}


global_name SEDNA_TRANSACTION_LOCK(session_id id, int os_primitives_id_min_bound, char* buf, int size)
{
#ifdef _WIN32
    string tmp = string("SEDNA_GN_PREFIX") + int2string(os_primitives_id_min_bound + _SEDNA_TRANSACTION_LOCK + id);

    if (tmp.length() > size - 1) throw USER_EXCEPTION(SE1009);
    strcpy(buf, tmp.c_str());
    return buf;
#else
    return os_primitives_id_min_bound + _SEDNA_TRANSACTION_LOCK + id;
#endif
}

global_name CHARISMA_SSMMSG_SM_ID(int db_id, int os_primitives_id_min_bound, char* buf, int size )
{
#ifdef _WIN32
    string tmp = string("SEDNA_GN_PREFIX") + int2string(os_primitives_id_min_bound + _CHARISMA_SSMMSG_SM_ID + db_id);

    if (tmp.length() > size - 1) throw USER_EXCEPTION(SE1009);
    strcpy(buf, tmp.c_str());
    return buf;
#else
    return os_primitives_id_min_bound + _CHARISMA_SSMMSG_SM_ID + db_id;
#endif
}


#define WIN_GN_INIT1(c)		c = new char[128];/*enough memory for prefix and string representation of numeric id*/ 		\
                     		strcpy((char*)c, "SEDNA_GN_PREFIX");								\
                                strcat((char*)c, _itoa(os_primitives_id_min_bound + _##c, buf, 10));

#define WIN_GN_INIT2(c)		c = new char[128];/*enough memory for prefix and string representation of numeric id*/ 		\
                     		strcpy((char*)c, "SEDNA_GN_PREFIX");								\
                                strcat((char*)c, _itoa(os_primitives_id_min_bound + _##c + id, buf, 10));

#define WIN_GN_INIT4(c)		c = new char[128];/*enough memory for prefix and string representation of numeric id*/ 		\
                     		strcpy((char*)c, _##c);								\
                                strcat((char*)c, _itoa(os_primitives_id_min_bound, buf, 10));

/*
#define WIN_GN_INIT1(c)		tmp = string(_##c) + db_name;									\
                     		c = se_new char[tmp.length() + 1];									\
                     		strcpy((char*)c, tmp.c_str());

#define WIN_GN_INIT2(c)		tmp = string(_##c);												\
                     		c = se_new char[tmp.length() + 1];									\
                     		strcpy((char*)c, tmp.c_str());

#define WIN_GN_INIT3(c)		if (_##c)														\
                            {																\
                                tmp = string(_##c) + db_name;								\
                     		    c = se_new char[tmp.length() + 1];								\
                     		    strcpy((char*)c, tmp.c_str());								\
                            }																\
                            else c = NULL;
*/

#define UNIX_GN_INIT1(c)	c = os_primitives_id_min_bound + _##c;

#define UNIX_GN_INIT2(c)	c = os_primitives_id_min_bound + _##c + id;

#define UNIX_GN_INIT3(c)	if (_##c != NULL)														\
                                {																\
                     		    c = new char[128+sizeof(_##c)];													\
                                    strcpy((char*)c, _##c);\
                     		    strcat((char*)c, u_itoa(os_primitives_id_min_bound +id, buf, 10));											\
                                }																\
                                else c = NULL;

#define UNIX_GN_INIT4(c)	c = new char[128];/*enough memory for prefix and string representation of numeric id*/ 		\
                     		strcpy((char*)c, _##c);								\
                                strcat((char*)c, u_itoa(os_primitives_id_min_bound, buf, 10));


/*
#define UNIX_GN_INIT3(c)	if (_##c)														\
                            {																\
                                tmp = string(_##c) + db_name;								\
                     		    c = se_new char[tmp.length() + 1];								\
                     		    strcpy((char*)c, tmp.c_str());								\
                            }																\
                            else c = NULL;
*/

void set_global_names(int os_primitives_id_min_bound)
{
    char buf[256];
#ifdef _WIN32
    WIN_GN_INIT1(GOVERNOR_SHARED_MEMORY_NAME);
    WIN_GN_INIT1(CHARISMA_GOVERNOR_IS_READY);
    WIN_GN_INIT1(SE_EVENT_LOG_SHARED_MEMORY_NAME);
    WIN_GN_INIT1(SE_EVENT_LOG_SEMAPHORES_NAME);
    WIN_GN_INIT4(SEDNA_GLOBAL_MEMORY_MAPPING);
#else
    UNIX_GN_INIT1(GOVERNOR_SHARED_MEMORY_NAME);
    UNIX_GN_INIT1(CHARISMA_GOVERNOR_IS_READY);
    UNIX_GN_INIT1(SE_EVENT_LOG_SHARED_MEMORY_NAME);
    UNIX_GN_INIT1(SE_EVENT_LOG_SEMAPHORES_NAME);
    UNIX_GN_INIT4(SEDNA_GLOBAL_MEMORY_MAPPING);
#endif
}

void set_global_names(int os_primitives_id_min_bound, int id)
{
    char buf[256];
#ifdef _WIN32
    WIN_GN_INIT2(CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME);
    WIN_GN_INIT2(CHARISMA_ITFE_SHARED_MEMORY_NAME);

    WIN_GN_INIT2(CHARISMA_BUFFER_SHARED_MEMORY_NAME);
    WIN_GN_INIT2(CHARISMA_PH_SHARED_MEMORY_NAME);

    WIN_GN_INIT2(VMM_SM_SEMAPHORE_STR);
    WIN_GN_INIT2(INDIRECTION_TABLE_SEMAPHORE_STR);
    WIN_GN_INIT2(VMM_SM_EXCLUSIVE_MODE_SEM_STR);
    WIN_GN_INIT2(PERS_HEAP_SEMAPHORE_STR);
    WIN_GN_INIT2(METADATA_SEMAPHORE_STR);
    WIN_GN_INIT2(INDEX_SEMAPHORE_STR);
#ifdef SE_ENABLE_FTSEARCH
    WIN_GN_INIT2(FT_INDEX_SEMAPHORE_STR);
#endif
#ifdef SE_ENABLE_TRIGGERS
    WIN_GN_INIT2(TRIGGER_SEMAPHORE_STR);
#endif
    WIN_GN_INIT2(PHYS_LOG_SHARED_MEM_NAME);
    WIN_GN_INIT2(PHYS_LOG_PROTECTION_SEMAPHORE_NAME);

    WIN_GN_INIT2(CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME);
    WIN_GN_INIT2(CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME);

    WIN_GN_INIT2(CHARISMA_CHECKPOINT_SEM);
    WIN_GN_INIT2(SEDNA_CHECKPOINT_FINISHED_SEM);
    WIN_GN_INIT2(CHARISMA_LOGICAL_OPERATION_ATOMICITY);
    WIN_GN_INIT2(CHARISMA_WAIT_FOR_CHECKPOINT);
    WIN_GN_INIT2(CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG);

    WIN_GN_INIT2(CHARISMA_SYNC_TRN_IDS_TABLE);

    WIN_GN_INIT2(CHARISMA_SM_WAIT_FOR_SHUTDOWN);

    WIN_GN_INIT2(CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME);

    WIN_GN_INIT2(SEDNA_LOCK_MANAGER_SEM);
    WIN_GN_INIT2(CHARISMA_SM_SMSD_ID);
    WIN_GN_INIT2(CHARISMA_SM_IS_READY);
#else

    UNIX_GN_INIT2(CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME);
    UNIX_GN_INIT2(CHARISMA_ITFE_SHARED_MEMORY_NAME);

    UNIX_GN_INIT3(CHARISMA_BUFFER_SHARED_MEMORY_NAME);
    UNIX_GN_INIT3(CHARISMA_PH_SHARED_MEMORY_NAME);

    UNIX_GN_INIT2(VMM_SM_SEMAPHORE_STR);
    UNIX_GN_INIT2(INDIRECTION_TABLE_SEMAPHORE_STR);
    UNIX_GN_INIT2(VMM_SM_EXCLUSIVE_MODE_SEM_STR);
    UNIX_GN_INIT2(PERS_HEAP_SEMAPHORE_STR);
    UNIX_GN_INIT2(METADATA_SEMAPHORE_STR);
    UNIX_GN_INIT2(INDEX_SEMAPHORE_STR);
#ifdef SE_ENABLE_FTSEARCH
    UNIX_GN_INIT2(FT_INDEX_SEMAPHORE_STR);
#endif
#ifdef SE_ENABLE_TRIGGERS
    UNIX_GN_INIT2(TRIGGER_SEMAPHORE_STR);
#endif

    UNIX_GN_INIT2(PHYS_LOG_SHARED_MEM_NAME);
    UNIX_GN_INIT2(PHYS_LOG_PROTECTION_SEMAPHORE_NAME);

    UNIX_GN_INIT2(CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME);
    UNIX_GN_INIT2(CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME);

    UNIX_GN_INIT2(CHARISMA_CHECKPOINT_SEM);
    UNIX_GN_INIT2(SEDNA_CHECKPOINT_FINISHED_SEM);
    UNIX_GN_INIT2(CHARISMA_LOGICAL_OPERATION_ATOMICITY);
    UNIX_GN_INIT2(CHARISMA_WAIT_FOR_CHECKPOINT);
    UNIX_GN_INIT2(CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG);

    UNIX_GN_INIT2(CHARISMA_SYNC_TRN_IDS_TABLE);
    UNIX_GN_INIT2(CHARISMA_SM_WAIT_FOR_SHUTDOWN);

    UNIX_GN_INIT2(CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME);

    UNIX_GN_INIT2(SEDNA_LOCK_MANAGER_SEM);
    UNIX_GN_INIT2(CHARISMA_SM_SMSD_ID);
    UNIX_GN_INIT2(CHARISMA_SM_IS_READY);
#endif
}
