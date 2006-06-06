/*
 * File:  ugc.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "ugc.h"
#include "base.h"
#include "ushm.h"
#include "usem.h"
#include "ummap.h"
#include "d_printf.h"



#define SEMAPHORE_CLEANUP(name)					if (USemaphoreOpen(&sem, name, __sys_call_error) == 0)						\
                                                {																			\
                                                    USemaphoreRelease(sem, __sys_call_error);								\
                                                    d_printf1("Semaphore cleanup    : "#name"\n");							\
                                                }

#define SHAREDMEM_CLEANUP(name, size)			if (uOpenShMem(&shm, name, size, __sys_call_error) == 0)					\
                                                {																			\
                                                    uReleaseShMem(shm, __sys_call_error);									\
                                                    d_printf1("Shared memory cleanup: "#name"\n");							\
                                                 }

#define SEMAPHORE_CLEANUP2(name, name2)			if (USemaphoreOpen(&sem, name, __sys_call_error) == 0)						\
                                                {																			\
                                                    USemaphoreRelease(sem, __sys_call_error);								\
                                                    d_printf1("Semaphore cleanup    : "#name2"\n");							\
                                                }

#define SHAREDMEM_CLEANUP2(name, name2, size)	if (uOpenShMem(&shm, name, size, __sys_call_error) == 0)					\
                                                {																			\
                                                    uReleaseShMem(shm, __sys_call_error);									\
                                                    d_printf1("Shared memory cleanup: "#name2"\n");							\
                                                }

#define FILE_MAPPING_CLEANUP(name)				if (!U_INVALID_FILEMAPPING(map = uOpenFileMapping(U_INVALID_FD, 0, name, __sys_call_error)))	\
                                                {																			\
                                                    uReleaseFileMapping(map, name, __sys_call_error);						\
                                                    d_printf1("Filemapping cleanup    : "#name"\n");						\
                                                }



void gov_ugc(bool background_off_from_background_on)
{
#ifdef _WIN32
#else
    USemaphore sem;
    UShMem shm;
    UMMap map;
    char buf[1024];

    if (background_off_from_background_on) return;

    d_printf1("Starting CLEANUP\n");
    SEMAPHORE_CLEANUP(CHARISMA_GOV_WAIT_FOR_SHUTDOWN);
    SEMAPHORE_CLEANUP(CHARISMA_SSMMSG_GOV_ID);
    SHAREDMEM_CLEANUP(CHARISMA_SSMMSG_GOV_ID, 8192);
    SEMAPHORE_CLEANUP(CHARISMA_STOP_GOV);
    SEMAPHORE_CLEANUP(CHARISMA_GOVERNOR_IS_READY);

    SEMAPHORE_CLEANUP(SE_EVENT_LOG_SEMAPHORES_NAME);
    SHAREDMEM_CLEANUP(SE_EVENT_LOG_SHARED_MEMORY_NAME, sizeof(event_log_msg));

    SHAREDMEM_CLEANUP(GOVERNOR_SHARED_MEMORY_NAME, GOV_SHM_SIZE);
    FILE_MAPPING_CLEANUP(SEDNA_GLOBAL_MEMORY_MAPPING);
 
    //relase _SESS_SHUTDOWN_SEMAPHORE_STR for each session
    for (int i = 0; i < MAX_SESSIONS_NUMBER; i++)
        SEMAPHORE_CLEANUP2(SESS_SHUTDOWN_SEMAPHORE_STR(i, buf, 1024), SESS_SHUTDOWN_SEMAPHORE_STR);


    d_printf1("CLEANUP completed\n\n");
#endif
}

void sm_ugc(bool background_off_from_background_on, const char* db_name)
{
#ifdef _WIN32
#else
    USemaphore sem;
    UShMem shm;
    UMMap map;
    char buf[1024];

    if (background_off_from_background_on) return;

    d_printf1("Starting CLEANUP\n");
    SEMAPHORE_CLEANUP(CHARISMA_SM_WAIT_FOR_SHUTDOWN);
    SEMAPHORE_CLEANUP(VMM_SM_SEMAPHORE_STR);
    SEMAPHORE_CLEANUP(VMM_SM_EXCLUSIVE_MODE_SEM_STR);
    SEMAPHORE_CLEANUP(INDIRECTION_TABLE_SEMAPHORE_STR);
    SEMAPHORE_CLEANUP(METADATA_SEMAPHORE_STR);
    SEMAPHORE_CLEANUP(INDEX_SEMAPHORE_STR);
#ifdef SE_ENABLE_FTSEARCH
    SEMAPHORE_CLEANUP(FT_INDEX_SEMAPHORE_STR);
#endif
    SHAREDMEM_CLEANUP(CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME, 8);
    SHAREDMEM_CLEANUP(CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME, 1024 * 1024/*CHARISMA_LOGICAL_LOG_SHARED_MEM_SIZE*/);
    SHAREDMEM_CLEANUP(CHARISMA_ITFE_SHARED_MEMORY_NAME, 8);
    SHAREDMEM_CLEANUP(PHYS_LOG_SHARED_MEM_NAME, /*PHYS_LOG_SHARED_MEM_SECTORS_NUM*/256*512);
    SEMAPHORE_CLEANUP(PHYS_LOG_PROTECTION_SEMAPHORE_NAME);
    SEMAPHORE_CLEANUP(CHARISMA_CHECKPOINT_SEM);
    SEMAPHORE_CLEANUP(CHARISMA_LOGICAL_OPERATION_ATOMICITY);
    SEMAPHORE_CLEANUP(CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME);
    SEMAPHORE_CLEANUP(CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG);
    SEMAPHORE_CLEANUP(CHARISMA_WAIT_FOR_CHECKPOINT);
    SEMAPHORE_CLEANUP(SEDNA_CHECKPOINT_FINISHED_SEM);
    SEMAPHORE_CLEANUP(SEDNA_LOCK_MANAGER_SEM);
    SEMAPHORE_CLEANUP(PERS_HEAP_SEMAPHORE_STR);
    SHAREDMEM_CLEANUP(CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME, 8);
    SEMAPHORE_CLEANUP(CHARISMA_SYNC_TRN_IDS_TABLE);
    SEMAPHORE_CLEANUP2(CHARISMA_SSMMSG_SM_ID(db_name, buf, 1024), CHARISMA_SSMMSG_SM_ID);
    SHAREDMEM_CLEANUP2(CHARISMA_SSMMSG_SM_ID(db_name, buf, 1024), CHARISMA_SSMMSG_SM_ID, 8192);
    SEMAPHORE_CLEANUP2(CHARISMA_SM_SMSD_ID(db_name, buf, 1024), CHARISMA_SM_SMSD_ID);
    SEMAPHORE_CLEANUP2(CHARISMA_SM_IS_READY(db_name, buf, 1024), CHARISMA_SM_IS_READY);
    FILE_MAPPING_CLEANUP(CHARISMA_BUFFER_SHARED_MEMORY_NAME);
    /// releasing sm/vmm semaphores
    for (int i = 0; i < CHARISMA_MAX_TRNS_NUMBER; i++)
    {
        SEMAPHORE_CLEANUP2(SM_TO_VMM_CALLBACK_SEM1_BASE_STR(i, db_name, buf, 1024), SM_TO_VMM_CALLBACK_SEM1_BASE_STR);
        SEMAPHORE_CLEANUP2(SM_TO_VMM_CALLBACK_SEM2_BASE_STR(i, db_name, buf, 1024), SM_TO_VMM_CALLBACK_SEM2_BASE_STR);
    }

    for (int i = 0; i < MAX_SESSIONS_NUMBER; i++)
    {
        SEMAPHORE_CLEANUP2(SEDNA_TRANSACTION_LOCK(i, db_name, buf, 1024), SEDNA_TRANSACTION_LOCK);
    } 


    d_printf1("CLEANUP completed\n\n");

    std::string buf_dir = SEDNA_DATA + std::string("/data/") + db_name + std::string("_files/buf");
#endif
}

void cdb_ugc(const char *db_name)
{
#ifdef _WIN32
#else
    sm_ugc(false, db_name);
#endif
}

