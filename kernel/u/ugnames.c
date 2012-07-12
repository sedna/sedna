#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#include "ugnames.h"

/* {% globals */

UGlobalGarbageCollector * UGlobalObjectsGC = NULL;
static const char * basedir = "/tmp";
static const char * instance = "0";
static unsigned int basedir_hash = 0;

/* }% */

#ifdef _WIN32
#define IPC_PRIVATE 0
#ifndef snprintf
#define snprintf _snprintf
#endif
#else
#include <sys/types.h>
#include <sys/ipc.h>
#endif

#include "aux/commutil.h"

void uSetGlobalNameGeneratorBase(const char* _basedir, const char * _instance)
{
    if (_basedir != NULL)
    {
        basedir = _basedir;
    }

    if (_instance != NULL)
    {
        instance = _instance;
    }

    basedir_hash = strhash(basedir);
}

const char* UGetNameFromGlobalName(global_name globalName, char* buf, size_t bufSize)
{
    if (globalName == NULL)
    {
        return NULL;
    };

#ifdef _WIN32
    snprintf(buf, bufSize, "Global\\sedna-%08x/%s.%s", basedir_hash, globalName, instance);
#elif (defined(FreeBSD) || defined(DARWIN))
    snprintf(buf, bufSize, "/tmp/%s.%s", basedir, globalName, instance);
#else
    snprintf(buf, bufSize, "/sedna-%08x-%s.%s", basedir_hash, globalName, instance);
#endif /* _WIN32 */

    return buf;
}

#ifndef _WIN32
key_t USys5IPCKeyFromGlobalName(global_name globalName)
{
    if (globalName == NULL) {
        return IPC_PRIVATE;
    } else {
        // TODO: generate method to check for global name hash collision in debug
        return ftok(basedir, strhash(globalName));
    }
}
#endif /* _WIN32 */
