/*
 * File:  vmmtrace.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <stdio.h>
#include "common/xptr/xptr.h"

#include "tr/vmm/vmmtrace.h"

#ifdef VMM_TRACE

static FILE* trace_file = NULL;
bool __vmm_trace = false;

void vmm_trace_start(FILE * file) {
    if (file == NULL) {
        trace_file = fopen(VMM_DEFAULT_TRACE_FILE, "a");
        U_ASSERT(trace_file != NULL);
    } else {
        trace_file = file;
    }
    __vmm_trace = true;
}

void vmm_trace_stop() {
    __vmm_trace = false;
    fclose(trace_file);
}

inline static void vmm_trace_xptr(const char cmd, const xptr& p)
{
    t_layer layer = p.layer;
    char istmp = 'D';
    const char * op;

    if (layer >= TMP_LAYER_STARTS_WITH) {
        layer -= TMP_LAYER_STARTS_WITH;
        istmp = 'T';
    }

    switch (cmd) {
        case 'R' : op = "CHECKP"; break;
        case 'W' : op = "VMM_SM"; break;
        case 'A' : op = "ALLOC_"; break;
        case 'U' : op = "UNSWAP"; break;
        case 'S' : op = "SWAP__"; break;
        case 'E' : op = "DELETE"; break;
        default  : op = "ERRORN";
    }

    fprintf(trace_file, "%s %c %03x:0x%08x\n", op, istmp, layer, p.getOffs());
}

void vmm_trace_checkp(const xptr& p)
{
    vmm_trace_xptr('R', p);
}

void vmm_trace_signal_modification(const xptr& p)
{
    vmm_trace_xptr('W', p);
}

void vmm_trace_alloc_block(const xptr& p, const xptr& s)
{
    vmm_trace_xptr('A', p);
    if (s != XNULL) { vmm_trace_xptr('S', s); }
}

void vmm_trace_unswap(const xptr& p, const xptr& s)
{
    vmm_trace_xptr('U', p);
    if (s != XNULL) { vmm_trace_xptr('S', s); }
}

void vmm_trace_callback(const xptr& s)
{
    fprintf(trace_file, "CALLBACK\n");
    vmm_trace_xptr('S', s);
}

void vmm_trace_delete_block(const xptr& p)
{
    vmm_trace_xptr('E', p);
}

#endif /* VMM_TRACE */
