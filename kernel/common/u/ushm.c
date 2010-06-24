/*
 * File:  ushm.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/u/ushm.h"
#include "common/u/ummap.h"
#include "common/errdbg/d_printf.h"
#include "common/u/ugnames.h"

int uCreateShMem(UShMem *id, global_name name, size_t size, USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun)
{
    UMMap mmap = uCreateFileMapping(U_INVALID_FD, size, name, sa, fun);

#ifdef _WIN32
    if (mmap.map == NULL)
        return 1;
#else
    if (mmap.map == -1)
        return 1;
#endif

    id->id = mmap.map;
    id->size = mmap.size;

    return 0;
}

int uOpenShMem(UShMem *id, global_name name, size_t size, sys_call_error_fun fun)
{
    UMMap mmap = uOpenFileMapping(U_INVALID_FD, size, name, fun);

#ifdef _WIN32
    if (mmap.map == NULL)
        return 1;
#else
    if (mmap.map == -1)
        return 1;
#endif

    id->id = mmap.map;
    id->size = mmap.size;

    return 0;
}

int uReleaseShMem(UShMem id, global_name name, sys_call_error_fun fun)
{
    UMMap mmap = {}; /* nullifies to_file */
    int res;

    mmap.map = id.id;
    mmap.size = id.size;
    res = uReleaseFileMapping(mmap, name, fun);

    return res;
}

int uCloseShMem(UShMem id, sys_call_error_fun fun)
{
    UMMap mmap = {}; /* nullifies to_file */
    int res;

    mmap.map = id.id;
    mmap.size = id.size;
    res = uCloseFileMapping(mmap, fun);

    return res;
}

void* uAttachShMem(UShMem id, void *ptr, size_t size, sys_call_error_fun fun)
{
    UMMap mmap = {}; /* nullifies to_file */
    void *res;

    mmap.map = id.id;
    mmap.size = id.size;
    res = uMapViewOfFile(mmap, ptr, size, 0, fun);

    return res;
}

int uDettachShMem(UShMem id, void * ptr, sys_call_error_fun fun)
{
    UMMap mmap = {}; /* nullifies to_file */
    int res;

    mmap.map = id.id;
    mmap.size = id.size;
    res = uUnmapViewOfFile(mmap, ptr, id.size, fun);

    return res;
}
