/*
 * File:  gmm.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>

#include "common/sedna.h"

#include "common/gmm.h"
#include "common/base.h"
#include "common/u/uutils.h"
#include "common/u/uprocess.h"

static void *global_memory;
UMMap global_memory_mapping;

void create_global_memory_mapping(int os_primitives_id_min_bound)
{
    global_memory_mapping = uCreateFileMapping(U_INVALID_FD, PAGE_SIZE, SEDNA_GLOBAL_MEMORY_MAPPING, NULL, __sys_call_error);
    if (U_INVALID_FILEMAPPING(global_memory_mapping))
        throw USER_EXCEPTION(SE4074);

    global_memory = uMapViewOfFile(global_memory_mapping, NULL, PAGE_SIZE, 0, __sys_call_error);
    if (global_memory == NULL)
        throw USER_EXCEPTION(SE4078);

    memset(global_memory, '\0', PAGE_SIZE);
    *(t_layer*)global_memory = INVALID_LAYER;
}

void release_global_memory_mapping()
{
    if (uUnmapViewOfFile(global_memory_mapping, global_memory, PAGE_SIZE, __sys_call_error) == -1)
        throw USER_EXCEPTION(SE4079);

    if (uReleaseFileMapping(global_memory_mapping, SEDNA_GLOBAL_MEMORY_MAPPING, __sys_call_error) == -1)
        throw USER_EXCEPTION(SE4076);
}

void open_global_memory_mapping(int err_code)
{
    global_memory_mapping = uOpenFileMapping(U_INVALID_FD, PAGE_SIZE, SEDNA_GLOBAL_MEMORY_MAPPING, __sys_call_error);
    if (U_INVALID_FILEMAPPING(global_memory_mapping))
        throw USER_EXCEPTION(err_code);
}

void close_global_memory_mapping()
{
    if (uCloseFileMapping(global_memory_mapping, __sys_call_error) == -1)
        throw USER_EXCEPTION(SE4077);
}

UMMap get_global_memory_mapping()
{
    return global_memory_mapping;
}
