/*
 * File:  exceptions.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "common/errdbg/exceptions.h"
#include "common/u/uprocess.h"
#include "common/errdbg/event_log.h"


void sedna_soft_fault(const SednaException &e,  int component)
{
    SEDNA_SOFT_FAULT_BASE_MSG;

	char log_message[SE_SOFT_FAULT_LOG_CONTENT_LEN];
    int log_message_len = e.getDescription().length();

    if (log_message_len != 0)
        fprintf(stderr, "Details: %s\n", e.getDescription().c_str());
    if(log_message_len < SE_SOFT_FAULT_LOG_CONTENT_LEN)
        strcpy(log_message, e.getDescription().c_str());
    else
        strcpy(log_message, "Failed to record exception description into the log\n");
#if (EL_DEBUG == 1)
    fprintf(stderr, "Position: [%s:%s:%d]\n", e.getFile().c_str(), e.getFunction().c_str(), e.getLine());
	sprintf(log_message+(log_message_len),"\nPosition: [%s:%s:%d]\n", e.getFile().c_str(), e.getFunction().c_str(), e.getLine());
#endif
	
	sedna_soft_fault_log(log_message, component);
    SEDNA_SOFT_FAULT_FINALIZER;
}

void sedna_soft_fault(const char* s, int  component)
{
    SEDNA_SOFT_FAULT_BASE_MSG;
    fprintf(stderr, "Details: %s\n", s);
	sedna_soft_fault_log(s, component);

    SEDNA_SOFT_FAULT_FINALIZER;
}

