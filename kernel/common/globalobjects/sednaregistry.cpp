#include "sednaregistry.h"

#include "common/sedna.h"
#include "common/base.h"

#include "u/ugnames.h"

#include <string>
#include <map>

#define SINGLETON_GLOBAL_NAME(NAME) \
  __GLOBAL_NAME_REGISTRY_ENTRY(NAME, NULL, 1)
  
#define DATABASE_GLOBAL_NAME(NAME) \
  __GLOBAL_NAME_REGISTRY_ENTRY(NAME,  "DB", (MAX_DBS_NUMBER))
  
#define SESSION_GLOBAL_NAME(NAME) \
  __GLOBAL_NAME_REGISTRY_ENTRY(NAME, "SESSION", (UPPER_SESSIONS_NUM_BOUND))

// This parameter should not be exactly right, its just an upper bound
#define GLOBAL_NAME_COUNT 32

/*      If you want to add a new global IPC object, do it here.
        We are going to extract basenames automaticly, so don't
        try anything unusual below and preserve markers. */

static UGlobalNamesRegistryItem globalNameRegistry[] =
{
/* Event log communication memory / semaphore pair */
SINGLETON_GLOBAL_NAME(EVENT_LOG_SHM),
SINGLETON_GLOBAL_NAME(EVENT_LOG_SEM),

/* Buffer memory */
DATABASE_GLOBAL_NAME(BUFFER_MEMORY_SHM),

/* SM comminucation channel */
DATABASE_GLOBAL_NAME(SM_TALK_SHM),
DATABASE_GLOBAL_NAME(SM_TALK_SEM),

/* lfs state & buffer in shared memory */
DATABASE_GLOBAL_NAME(LFS_SHM),
DATABASE_GLOBAL_NAME(LFS_SEM),

/* logical log state & buffer in shared memory */
DATABASE_GLOBAL_NAME(LOGICAL_LOG_SHM),
DATABASE_GLOBAL_NAME(LOGICAL_LOG_SEM),

/* signals that a checkpoint must be activated or snapshots must be advanced */
DATABASE_GLOBAL_NAME(NEW_JOB_4_CHECKPOINT_THREAD_EVENT),

/* to wait for checkpoint to finish */
DATABASE_GLOBAL_NAME(CHECKPOINT_FINISHED_SEM),
{},
};

#define GLOBAL_NAME_BUFFER_LEN ((GLOBAL_NAME_COUNT + UPPER_SESSIONS_NUM_BOUND + MAX_DBS_NUMBER) * MAX_GLOBAL_NAME_LEN)

static char globalNameBuffer[GLOBAL_NAME_BUFFER_LEN];
static char * globalNameBufferPtr = globalNameBuffer;

static int databaseId = 0;
static int sessionId = 0;

global_name createSednaGlobalName(const char* globalNameBase)
{
    UGlobalNamesRegistryItem * regItem = globalNameRegistry;
    int objectId = 0;

    do {
        if (strcmp(regItem->basename, globalNameBase) == 0) {
            break;
        };

        regItem++;
    } while (regItem->basename != NULL);

    if (regItem->basename == NULL) {
        U_ASSERT(false);
        throw SYSTEM_EXCEPTION("Internal error (not registered global object was about to be created)");
    };

    if (regItem->tag > -1) {
        return globalNameBuffer + regItem->tag;
    };

    if (regItem->prefix == NULL) {
        objectId = 0;
    } else if (regItem->prefix[0] == 'D') {
        objectId = databaseId;
    } else if (regItem->prefix[0] == 'S') {
        objectId = sessionId;
    } else {
        U_ASSERT(false);
        throw SYSTEM_EXCEPTION("Internal error (not registered global object was about to be created)");
    };

    const char * result = UCreateGlobalNameFromRegistry(regItem, objectId,
        globalNameBufferPtr, GLOBAL_NAME_BUFFER_LEN - (globalNameBufferPtr - globalNameBuffer));

    globalNameBufferPtr += strlen(result) + 1;

    regItem->tag = (int) (globalNameBufferPtr - globalNameBuffer);

    return result;
};

static
void sednaException(const char * message) {
    throw SYSTEM_EXCEPTION(message);
};

void initSednaGlobalNameRegistry(int osObjectsMinBound, int databaseId, int sessionId)
{
    UInitGlobalNamesRegistry(
      globalNameRegistry, NULL, &sednaException,
      osObjectsMinBound, osObjectsMinBound + (GLOBAL_NAME_COUNT * UPPER_SESSIONS_NUM_BOUND * MAX_DBS_NUMBER));


};

void releaseSednaGlobalNameRegistry()
{
    UReleaseGlobalNamesRegistry();
    globalNameBufferPtr = globalNameBuffer;
};



//        {"SEMAP_BUFMGR_EXCL_MODE",                      POLICY_INSTANCE_PER_DB()}, /* regulates the exclusive mode entering by TRN */

//    {"SEMAP_CATALOG_NAMETABLES",        POLICY_INSTANCE_PER_DB()}, /* catalog nametables lock */
//    {"SEMAP_CATALOG_METADATA",          POLICY_INSTANCE_PER_DB()}, /* catalog metadata lock */

//        {"SEMAP_VMM_INIT",                                      POLICY_INSTANCE_PER_DB()}, /* VMM initialisation is serialised with this sem */
//        {"SHMEM_VMM_CALLBACK_PARAMS",           POLICY_INSTANCE_PER_DB()}, /* parameters passed to VMM calback */
//        {"EVENT_VMM_CALLBACK",                          POLICY_INSTANCE_PER_SESSION()}, /* VMM callback thread waits on it */
//        {"EVENT_VMM_CALLBACK_COMPLETED",        POLICY_INSTANCE_PER_SESSION()}, /* sem for callback thread to signal call completion */

//        {"SEMAP_LOCKMGR",                                       POLICY_INSTANCE_PER_DB()}, /* serialises requests to lock manager (in SM) */
//        {"EVENT_LOCK_GRANTED",                          POLICY_INSTANCE_PER_SESSION()}, /* if transaction request for a lock on DB entity is not satisfied immediately trn waits until the event is signalled (hence if transaction enters the wait state, it can't become a victim for the deadlock-resolution process) */
//    {"SEMAP_CHECKPOINT_FINISHED",       POLICY_INSTANCE_PER_DB()}, /* to wait for checkpoint to finish */

//        {"SEMAP_TRN_REGULATION",                        POLICY_INSTANCE_PER_DB()}, /* currently if checkpoint is active no updater transactions are allowed and vice-versa (earlier mutual exclusion applied to micro-ops but not transactions) */
//        {"EVENT_NEW_JOB_4_CHECKPOINT_THREAD", POLICY_INSTANCE_PER_DB()}, /* signals that a checkpoint must be activated or snapshots must be advanced */
//        {"EVENT_READONLY_TRN_COMPLETED",        POLICY_INSTANCE_PER_DB()}, /* signals read-only transaction completion */



//        {"EVENT_SM_SHUTDOWN_COMMAND",           POLICY_INSTANCE_PER_DB()}, /* signaled by SSMMsg thread in SM when shutdown command arrives via messaging interface */

//        {"EVENT_RECOVERY_COMPLETED",            POLICY_INSTANCE_PER_DB()}, /* signaled when se_rcv completes the recovery */

//        {"SEMAP_TRNS_TABLE",                            POLICY_INSTANCE_PER_DB()}, /* synchronises access to transactions table in SM */
//        {"EVENT_SM_READY",                                      POLICY_INSTANCE_PER_DB()}, /* used to signal initialisation completion when starting SM in the background mode */
//        {"EVENT_GOV_READY",                                     POLICY_SINGLETON()}, /* used to signal initialisation completion when starting GOV in the background mode */

        /* %} */
//        {NULL}
//};
