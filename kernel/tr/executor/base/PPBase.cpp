/*
 * File:  PPBase.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/xqops/PPFunCall.h"


namespace tr_globals
{
    char mem_str_buf[MAX_ATOMIC_LEX_REPR_SIZE + 1];
    char mem_str_buf2[MAX_ATOMIC_LEX_REPR_SIZE + 1];

    char e_string_buf[PAGE_SIZE];

    /* Used to throw XQUERY_EXCEPTION which contains line information. */
    TLS_VAR_DECL
    PPIterator* __current_physop = NULL;

    /* Used to interrupt execution due to timeout. */
    TLS_VAR_DECL
    volatile bool is_timer_fired = false;
    
    /* Counts depth of the physical plan in runtime */
    extern TLS_VAR_DECL
    volatile unsigned int current_stack_depth = 0;

    /* FIXME: make this TLS_VAR_DECL when we start to use threads */
    op_str_buf tmp_op_str_buf;
}
