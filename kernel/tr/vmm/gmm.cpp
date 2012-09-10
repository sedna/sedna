/*
 * File:  gmm.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>

#include "common/sedna.h"

#include "tr/vmm/gmm.h"
#include "common/base.h"
#include "u/uutils.h"
#include "u/uprocess.h"

static void *global_memory;
UShMem global_memory_mapping;

void open_global_memory_mapping(int err_code)
{
    if (uOpenShMem(&global_memory_mapping, SEDNA_GLOBAL_MEMORY_MAPPING, __sys_call_error) != 0)
        throw USER_EXCEPTION(err_code);
}

void close_global_memory_mapping()
{
    if (uCloseShMem(&global_memory_mapping, __sys_call_error) != 0)
        throw USER_EXCEPTION(SE4077);
}
