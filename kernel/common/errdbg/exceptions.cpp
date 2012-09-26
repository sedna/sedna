/*
 * File:  exceptions.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "u/uprocess.h"

#include "common/sedna.h"
#include "common/errdbg/exceptions.h"
#include "common/errdbg/event_log.h"

const char * SednaSystemException::description = "FATAL ERROR\n System error. This error means system malfunction.";
const char * SednaSystemEnvException::description = "FATAL ERROR\n"
      "Environment error. This error is caused by environment (operating system) and "
      "it means that the system cannot continue execution anymore.";

char SednaException::error_buffer[EXCEPTION_BUFFER_SIZE] = {};

#define CHECK_SNPRINTF_RET(VAL, BUF, BUF0) \
    if (VAL < 0) { return NULL; } \
    if (VAL >= EXCEPTION_BUFFER_SIZE) { return BUF0; }; \
    BUF += VAL;

const char* SednaException::createMessage(char* buffer) const
{
    const char * buf0 = buffer;
    int len;

    len = snprintf(buffer, EXCEPTION_BUFFER_SIZE,
        "SEDNA Message: %s\n"
        "Details: %s\n"
#if (EL_DEBUG == 1)
        "Position: [%s, %s, %d]\n"
#endif
        , global_description, err_msg
#if (EL_DEBUG == 1)
        , file, function, line
#endif
    );

    return buf0;
}

const char* SednaUserException::createMessage(char* buffer) const
{
    const char * buf0 = buffer;
    int len;

    len = snprintf(buffer, EXCEPTION_BUFFER_SIZE,
        "SEDNA Message: ERROR %s\n%s",
        error_name, error_descr);

    CHECK_SNPRINTF_RET(len, buffer, buf0);

    if (err_msg[0] != '\0') {
        len = snprintf(buffer, EXCEPTION_BUFFER_SIZE - (buffer-buf0),
            "Details: %s\n", err_msg);

        CHECK_SNPRINTF_RET(len, buffer, buf0);
    };

#if (EL_DEBUG == 1)
    len = snprintf(buffer, EXCEPTION_BUFFER_SIZE - (buffer-buf0),
        "Position: [%s, %s, %d]\n", file, function, line);

    CHECK_SNPRINTF_RET(len, buffer, buf0);
#endif

    return buf0;
}

const char* SednaUserEnvException::createMessage(char* buffer) const
{
    const char * buf0 = buffer;

    SednaUserException::createMessage(buffer);
    int len = strlen(buffer);

    CHECK_SNPRINTF_RET(len, buffer, buf0);

    if (explanation[0] != '\0') {
        len = snprintf(buffer, EXCEPTION_BUFFER_SIZE - (buffer-buf0),
            "System error: %s\n", explanation);
    };

    return buf0;
}

void sedna_soft_fault(const SednaException &e,  int component)
{
    SEDNA_SOFT_FAULT_BASE_MSG;

    char log_message[EXCEPTION_BUFFER_SIZE];

    sedna_soft_fault_log(e.getMsg(log_message), component);

    SEDNA_SOFT_FAULT_FINALIZER;
}

void sedna_soft_fault(const char* s, int  component)
{
    SEDNA_SOFT_FAULT_BASE_MSG;

    fprintf(stderr, "Details: %s\n", s);
    sedna_soft_fault_log(s, component);

    SEDNA_SOFT_FAULT_FINALIZER;
}

