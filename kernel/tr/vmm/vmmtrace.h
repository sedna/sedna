/*
* File:  vmmtrace.h
* Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#ifndef _VMM_TRACE_H
#define _VMM_TRACE_H

// #define VMM_TRACE

#define VMM_DEFAULT_TRACE_FILE "vmm_trace.txt"

#ifdef VMM_TRACE

extern bool __vmm_trace;

void vmm_trace_start(FILE * file);
void vmm_trace_stop();

void vmm_trace_checkp(const xptr& p);
void vmm_trace_signal_modification(const xptr& p);
void vmm_trace_alloc_block(const xptr& p, const xptr& s);
void vmm_trace_unswap(const xptr& p, const xptr& s);
void vmm_trace_callback(const xptr& s);
void vmm_trace_delete_block(const xptr& p);

#define IF_TRACE_ON(f) if (__vmm_trace) { f; }

#define VMM_TRACE_CHECKP(p)                    IF_TRACE_ON(vmm_trace_checkp(p))
#define VMM_TRACE_SIGNAL_MODIFICATION(p)       IF_TRACE_ON(vmm_trace_signal_modification(p));
#define VMM_TRACE_ALLOC_BLOCK(p, s)            IF_TRACE_ON(vmm_trace_alloc_block(p, s));
#define VMM_TRACE_UNSWAP(p, s)                 IF_TRACE_ON(vmm_trace_unswap(p, s));
#define VMM_TRACE_CALLBACK(s)                  IF_TRACE_ON(vmm_trace_callback(s));
#define VMM_TRACE_DELETE_BLOCK(p)              IF_TRACE_ON(vmm_trace_delete_block(p));

#else

inline static void vmm_trace_start(void * a) {};
inline static void vmm_trace_stop() {};

#define VMM_TRACE_CHECKP(p)
#define VMM_TRACE_SIGNAL_MODIFICATION(p)
#define VMM_TRACE_ALLOC_BLOCK(p, s)
#define VMM_TRACE_UNSWAP(p, s)
#define VMM_TRACE_CALLBACK(s)
#define VMM_TRACE_DELETE_BLOCK(p)

#endif /*VMM_TRACE*/

#endif /* _VMM_TRACE_H */
