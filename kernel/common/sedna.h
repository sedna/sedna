/*
 * File:  sedna.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _SEDNA_H
#define _SEDNA_H


#include "common/u/u.h"
#include "common/u/uni.h"
#include "sp_defs.h"
#include "common/mmgr/se_alloc.h"
#include "common/errdbg/event_log.h"

#ifdef __cplusplus
#include <cstddef>
#include "common/errdbg/exceptions.h"
#endif


/*
 * SEDNA_DATA contains path to the Sedna's data directory
 */
#define SEDNA_DATA_VAR_SIZE								1024
SE_EXTERN_C char* SEDNA_DATA;

/*
 * Pointer to the governor's shared memory.
 */
SE_EXTERN_C void* sedna_gov_shm_ptr;


SE_EXTERN_C 
int set_sedna_data(char*, sys_call_error_fun fun);

SE_EXTERN_C 
void DumpFaultInfo();


#define SEDNA_SOFT_FAULT_BASE_MSG \
    fprintf(stderr, "SEDNA Message: FATAL ERROR\n"); \
    DumpFaultInfo(); \
    fprintf(stderr, "System error. This error means system malfunction.\n")

#define SEDNA_SOFT_FAULT_FINALIZER \
    fflush(stderr); \
    uExitProcess(1, __sys_call_error)

#endif /*_SEDNA_H */
