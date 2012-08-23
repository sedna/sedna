/*
 * File:  sm_functions.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "smtypes.h"

#include "u/umutex.h"
#include "common/errdbg/exceptions.h"

#include <string>

using namespace std;

static uMutexType giantLockMutex;
static bool isGiantLockInitialized = false;

void InitGiantLock()
{
    if (isGiantLockInitialized)
        throw SYSTEM_EXCEPTION("giant lock already initialised");

    if (uMutexInit(&giantLockMutex,__sys_call_error) != 0)
        throw SYSTEM_EXCEPTION("giant lock mutex not initialised");

    isGiantLockInitialized = true;
}

void DestroyGiantLock()
{
    if (isGiantLockInitialized)
        uMutexDestroy(&giantLockMutex, NULL);
}

void ObtainGiantLock()
{
    if (!isGiantLockInitialized || uMutexLock(&giantLockMutex, __sys_call_error)!=0)
        throw SYSTEM_EXCEPTION("failed to obtain giant lock");
}

void ReleaseGiantLock()
{
    if (!isGiantLockInitialized || uMutexUnlock(&giantLockMutex, __sys_call_error)!=0)
        throw SYSTEM_EXCEPTION("failed to release giant lock");
}

void set_layer_parameters(lsize_t layer_size)
{
    LAYER_ADDRESS_SPACE_SIZE = layer_size;
}

void recreate_tmp_file()
{
    // truncate tmp file up to zero size
    if (uSetEndOfFile(tmp_file_handler, 0, U_FILE_BEGIN, __sys_call_error) == 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot truncate tmp file");

    // update master block
    mb->tmp_file_cur_size = 0;
    mb->free_tmp_blocks = XNULL;

    extend_tmp_file((int)MBS2PAGES(databaseOptions->tmpFileSize.initial));
}
