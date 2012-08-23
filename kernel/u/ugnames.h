#if (_MSC_VER>1000)
#pragma once
#endif

#ifndef UGNAMES_H_INCLUDED
#define UGNAMES_H_INCLUDED

#ifndef EXTERNC
#ifdef __cplusplus
#define EXTERNC extern "C" 
#else
#define EXTERNC
#endif
#endif

#include "u/u.h"

#define GOBJECT_EVENT 1
#define GOBJECT_SEM 2
#define GOBJECT_SEM_ARRAY 3
#define GOBJECT_SHARED_MEM 4

#define GLOBAL_NAME_BUFFER_DECL(NAME) char NAME[128]

struct gobj_info_t
{
    int type;
    void * data;
    unsigned arg1;
};

typedef void (* UGlobalObjectEvent) (const char * name, struct gobj_info_t data);

typedef struct UGlobalGarbageCollector_tag_ {
    UGlobalObjectEvent onCleanup;
    UGlobalObjectEvent onCreate;
    UGlobalObjectEvent onDestroy;
} UGlobalGarbageCollector;

EXTERNC
UGlobalGarbageCollector * UGlobalObjectsGC;

/**
 * Set up the base directory for global name foundry.
 * All global names are generated in the system specific way.
 *
 * The resulting object name will have the form of : "globalName.instance",
 * e.g. buffer_shared_memory.7.14 where "buffer_shared_memory" is a global name
 * and "7.14" is instance id.
 *
 * This is important because we may need to create global names of subordinated instances
 * (e.g. sessions for a database) from the database.
 * 
 * For example, database "14" can create object "event_lock.3.14.0"
 * using "event_lock.3" as name and "14.0" as instance id.
 *
 * This is safe as far as every object only exists at one possible level
 * (session, database or global).
 *
 * Windows names are in the form of:
 * BaseDirHash/ObjectName
 * e.g. 10237874/buffer_shared_memory.7.14
 *
 * Note, that Windows object names must not contain "\"
 * 
 * Posix names are in the form of:
 * BaseDir/ObjectName
 * e.g. /var/sedna/data/buffer_shared_memory.7.14
 *
 * IPC names are created using ftok function with real
 * basedir as the first parameter and ObjectName hash function as the second one.
 *
 * We should be aware, that hash functions for both path on Windows
 * and ftok parameter may collide. The first collision can be checked in
 * debug mode safely, so that it can always be fixed. And the second one may
 * be checked only in runtime.
 *
 * Both of these checks are TODO!
 * 
 * NOTE: A hack: due to some reasons, there is a way to pass unfair global name.
 * If you want a name to be passed as is,
 * it should be started with | (pipe) symbol,
 * which is then removed
 */


EXTERNC
void uSetGlobalNameGeneratorBase(const char* _basedir, const char* _instance);

/* Create a Posix IPC or windows name from the global name. */
EXTERNC
const char *UGetNameFromGlobalName(global_name globalName, char *buf, size_t bufSize);

#ifndef _WIN32
/* Create a SYS 5 IPC key from the global name. */ 
EXTERNC
key_t USys5IPCKeyFromGlobalName(global_name globalName);

#endif /* WIN32 */

#endif
