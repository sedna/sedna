/*
 * File:  sedna.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _SEDNA_H
#define _SEDNA_H


#include "u.h"
#include "sp_defs.h"
#include "se_alloc.h"
#include "event_log.h"


#ifdef __cplusplus
extern "C" {
#endif

/*
 * SEDNA_DATA contains path to Sedna data directory
 */
#define SEDNA_DATA_VAR_SIZE								1024
extern char SEDNA_DATA[SEDNA_DATA_VAR_SIZE];


#ifdef __cplusplus
}
#endif


#define SEDNA_SOFT_FAULT_BASE_MSG \
    fprintf(stderr, "SEDNA Message: FATAL ERROR\n"); \
    fprintf(stderr, "System error. This error means system malfunction.\n")

#define SEDNA_SOFT_FAULT_FINALIZER \
    fflush(stderr); \
    uExitProcess(1)



#endif
