/*
 * File:  ugc.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "common/ugc.h"
#include "common/base.h"
#include "common/u/ushm.h"
#include "common/u/usem.h"
#include "common/u/ummap.h"
#include "common/u/uevent.h"
#include "common/errdbg/d_printf.h"
#include "common/config.h"
#include "common/SSMMsg.h"


#define EVENT_CLEANUP(name)						if (UEventOpen(&ev, name, __sys_call_error) == 0)						\
                                                {																			\
                                                    UEventCloseAndUnlink(&ev, __sys_call_error);								\
                                                    d_printf1("Event cleanup    : "#name"\n");							\
                                                }

#define SEMAPHORE_CLEANUP(name)					if (USemaphoreOpen(&sem, name, __sys_call_error) == 0)						\
                                                {																			\
                                                    USemaphoreRelease(sem, __sys_call_error);								\
                                                    d_printf1("Semaphore cleanup    : "#name"\n");							\
                                                }

#define SHAREDMEM_CLEANUP(name)			        if (uOpenShMem(&shm, name, __sys_call_error) == 0)					        \
                                                {																			\
                                                    uReleaseShMem(&shm, name, __sys_call_error);									\
                                                    d_printf1("Shared memory cleanup: "#name"\n");							\
                                                 }

#define SEMAPHORE_CLEANUP2(name, name2)			if (USemaphoreOpen(&sem, name, __sys_call_error) == 0)						\
                                                {																			\
                                                    USemaphoreRelease(sem, __sys_call_error);								\
                                                    d_printf1("Semaphore cleanup    : "#name2"\n");							\
                                                }

#define FILE_MAPPING_CLEANUP(name)				if (!U_INVALID_FILEMAPPING(map = uOpenFileMapping(U_INVALID_FD, 0, name, __sys_call_error)))	\
                                                {																			\
                                                    uReleaseFileMapping(map, name, __sys_call_error);						\
                                                    d_printf1("Filemapping cleanup    : "#name"\n");						\
                                                }

void gov_ugc(bool background_off_from_background_on, int os_primitives_bound)
{
#ifdef _WIN32
#else
    USemaphore sem;
    UShMem shm;
    UMMap map;
    char buf[1024];

    if (background_off_from_background_on) return;

    d_printf1("Starting CLEANUP\n");
    SEMAPHORE_CLEANUP(CHARISMA_GOVERNOR_IS_READY);

    SEMAPHORE_CLEANUP(SE_EVENT_LOG_SEMAPHORES_NAME);
    SHAREDMEM_CLEANUP(SE_EVENT_LOG_SHARED_MEMORY_NAME);

    SHAREDMEM_CLEANUP(GOVERNOR_SHARED_MEMORY_NAME);
    SHAREDMEM_CLEANUP(SEDNA_GLOBAL_MEMORY_MAPPING);

    /// releasing semaphores depending on sessions
    for (int i = 0; i < MAX_SESSIONS_NUMBER; i++)
    {
        SEMAPHORE_CLEANUP2(SM_TO_VMM_CALLBACK_SEM1_BASE_STR(i, buf, 1024), SM_TO_VMM_CALLBACK_SEM1_BASE_STR);
        SEMAPHORE_CLEANUP2(SM_TO_VMM_CALLBACK_SEM2_BASE_STR(i, buf, 1024), SM_TO_VMM_CALLBACK_SEM2_BASE_STR);
        SEMAPHORE_CLEANUP2(SEDNA_TRANSACTION_LOCK(i, buf, 1024), SEDNA_TRANSACTION_LOCK);
    } 
 
    d_printf1("CLEANUP completed\n\n");
#endif
}

void sm_ugc(bool background_off_from_background_on, int db_id, int os_primitives_bound)
{
#ifdef _WIN32
#else
    USemaphore sem;
    UShMem shm;
    UMMap map;
    UEvent ev;
    char buf[1024];

    if (background_off_from_background_on) return;

    d_printf1("Starting CLEANUP\n");
    SEMAPHORE_CLEANUP(CHARISMA_SM_WAIT_FOR_SHUTDOWN);
    SEMAPHORE_CLEANUP(VMM_SM_SEMAPHORE_STR);
    SEMAPHORE_CLEANUP(VMM_SM_EXCLUSIVE_MODE_SEM_STR);

    SEMAPHORE_CLEANUP(CATALOG_NAMETABLE_SEMAPHORE_STR);
    SEMAPHORE_CLEANUP(CATALOG_MASTER_SEMAPHORE_STR);

//    SEMAPHORE_CLEANUP(METADATA_SEMAPHORE_STR);
//    SEMAPHORE_CLEANUP(INDEX_SEMAPHORE_STR);
#ifdef SE_ENABLE_FTSEARCH
    //SEMAPHORE_CLEANUP(FT_INDEX_SEMAPHORE_STR);
#endif
#ifdef SE_ENABLE_TRIGGERS
    //SEMAPHORE_CLEANUP(TRIGGER_SEMAPHORE_STR);
#endif
    SHAREDMEM_CLEANUP(CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME);
    SHAREDMEM_CLEANUP(CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME);
    SHAREDMEM_CLEANUP(SEDNA_LFS_SHARED_MEM_NAME);
    //SEMAPHORE_CLEANUP(CHARISMA_CHECKPOINT_SEM);
    SEMAPHORE_CLEANUP(SEDNA_TRNS_FINISHED);
    SEMAPHORE_CLEANUP(CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME);
    SEMAPHORE_CLEANUP(CHARISMA_DB_RECOVERED_BY_LOGICAL_LOG);
    //SEMAPHORE_CLEANUP(CHARISMA_WAIT_FOR_CHECKPOINT);
    SEMAPHORE_CLEANUP(SEDNA_CHECKPOINT_FINISHED_SEM);
    SEMAPHORE_CLEANUP(SEDNA_LFS_SEM_NAME);

	EVENT_CLEANUP(SNAPSHOT_CHECKPOINT_EVENT);
	EVENT_CLEANUP(TRY_ADVANCE_SNAPSHOT_EVENT);

    SHAREDMEM_CLEANUP(CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME);
    //SEMAPHORE_CLEANUP(CHARISMA_SM_SMSD_ID);
    SEMAPHORE_CLEANUP(CHARISMA_SM_IS_READY);

	SSMMsg::ipc_cleanup(CHARISMA_SSMMSG_SM_ID(db_id, buf, 1024));

	SHAREDMEM_CLEANUP(CHARISMA_BUFFER_SHARED_MEMORY_NAME);


    d_printf1("CLEANUP completed\n\n");

//    std::string buf_dir = SEDNA_DATA + std::string("/data/") + db_name + std::string("_files/buf");
#endif
}

void cdb_ugc(int db_id, int os_primitives_bound)
{
#ifdef _WIN32
#else
    sm_ugc(false, db_id, os_primitives_bound);
#endif
}

