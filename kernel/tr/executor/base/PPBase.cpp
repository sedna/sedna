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

#define CHECK_SNPRINTF_RET(VAL, BUF, BUF0) \
    if (VAL < 0) { return NULL; } \
    if (VAL >= EXCEPTION_BUFFER_SIZE) { return BUF0; }; \
    BUF += VAL;

const char* SednaXQueryException::createMessage(char* buffer) const
{
    const char * buf0 = buffer;
    size_t len;

    len = snprintf(buffer, EXCEPTION_BUFFER_SIZE, "SEDNA Message: ERROR %s \n%s\n",
        user_error_code_entries[internal_code].code,
        user_error_code_entries[internal_code].descr);

    CHECK_SNPRINTF_RET((int)len, buffer, buf0);

    if (err_msg[0] != 0) {
        len = snprintf(buffer, EXCEPTION_BUFFER_SIZE - (buffer - buf0),
            "Details: %s\n", err_msg);
        CHECK_SNPRINTF_RET((int)len, buffer, buf0);
    }

    if (xquery_line != 0)
    {
        len = snprintf(buffer, EXCEPTION_BUFFER_SIZE - (buffer - buf0),
            "Query line: %d, column: %d \n", xquery_line, xquery_col);
        CHECK_SNPRINTF_RET((int)len, buffer, buf0);
    }

#if (EL_DEBUG == 1)
    len = snprintf(buffer, EXCEPTION_BUFFER_SIZE - (buffer - buf0),
        "Position: [%s, %s, %d]\n", file, function, line);
    CHECK_SNPRINTF_RET((int)len, buffer, buf0);
#endif
    return buf0;
}
