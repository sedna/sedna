/*
 * File:  PPBase.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/xqops/PPFunCall.h"


namespace executor_globals
{
    char mem_str_buf[MAX_ATOMIC_LEX_REPR_SIZE + 1];
    char mem_str_buf2[MAX_ATOMIC_LEX_REPR_SIZE + 1];

    /* Used to throw XQUERY_EXCEPTION which contains line information. */
    TLS_VAR_DECL
    PPIterator* __current_physop = NULL;

    /* If turned on each operation collects various profile statistics */
    TLS_VAR_DECL
    volatile bool profiler_mode = false;
 
    /* Counts depth of the physical plan in runtime */
    TLS_VAR_DECL
    volatile int current_stack_depth = 0;

    /* FIXME: make this TLS_VAR_DECL when we start to use threads */
    op_str_buf tmp_op_str_buf;

    /* Physical operations execution stack.
     * It's being filled only when debug mode is turned on by
     * the client application. */
    std::deque<operation_info> pp_stack(10);

    void on_kernel_statement_end()
    {
        tmp_op_str_buf.reset();
        __current_physop = NULL;
        current_stack_depth = 0;
        profiler_mode = false;
        pp_stack.clear();
    }
}
