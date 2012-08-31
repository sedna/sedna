#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#include "ugnames.h"

/* {% globals */

UGlobalGarbageCollector * UGlobalObjectsGC = NULL;
static const char * basedir = "/tmp";
static const char * instance[10] = {"SE", "x", "x"};
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

#include "auxiliary/commutil.h"

void uSetGlobalNameGeneratorBase(const char* _basedir)
{
    if (_basedir != NULL)
    {
        basedir = _basedir;
    }

    basedir_hash = strhash(basedir);
}

void uSetGlobalNameInstanceId(int level, const char* iid)
{
    U_ASSERT(level >= 0 && level <= 3);

    instance[level] = iid;
}

const char* UGetNameFromGlobalName(global_name globalName, char* buf, size_t bufSize)
{
    if (globalName.name == NULL)
    {
        return NULL;
    };

    if (globalName.name[0] == '|')
    {
        strncpy(buf, globalName.name + 1, bufSize);
        return buf;
    };

    {
        // TODO : We may prebuild all strings for global names

        GLOBAL_NAME_BUFFER_DECL(instanceName);
        int instanceNameLen = sizeof instanceName;

        unsigned i;
        for (i = 0; i <= globalName.hlevel; ++i) {
            snprintf(instanceName, instanceNameLen, ".%s", instance[i]);
        };

#ifdef _WIN32
        snprintf(buf, bufSize, "sedna-%08x/%s%s", basedir_hash, globalName.name, instanceName);
#elif (defined(FreeBSD) || defined(DARWIN))
    //    snprintf(buf, bufSize, "/tmp/x%08x.%s.%s", basedir_hash, globalName, instance);
        snprintf(buf, bufSize, "%s/%s%s", basedir, globalName.name, instanceName);
#else
        snprintf(buf, bufSize, "/sedna-%08x-%s%s", basedir_hash, globalName.name, instanceName);
#endif /* _WIN32 */

        return buf;
    }
}

#ifndef _WIN32
key_t USys5IPCKeyFromGlobalName(global_name globalName)
{
    if (globalName.name == NULL) {
        return IPC_PRIVATE;
    } else {
        GLOBAL_NAME_BUFFER_DECL(tmpName);

        UGetNameFromGlobalName(globalName, tmpName, sizeof tmpName);

        // TODO: generate method to check for global name hash collision in debug
        return ftok(basedir, strhash(tmpName));
    }
}
#endif /* _WIN32 */
