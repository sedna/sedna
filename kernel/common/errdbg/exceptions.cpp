/*
 * File:  exceptions.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "exceptions.h"
#include "uprocess.h"


void sedna_soft_fault(const SednaException &e)
{
    SEDNA_SOFT_FAULT_BASE_MSG;

    if (e.getDescription().length() != 0)
        fprintf(stderr, "Details: %s\n", e.getDescription().c_str());
#if (EL_DEBUG == 1)
    fprintf(stderr, "Position: [%s:%s:%d]\n", e.getFile().c_str(), e.getFunction().c_str(), e.getLine());
#endif

    SEDNA_SOFT_FAULT_FINALIZER;
}

void sedna_soft_fault(const char* s)
{
    SEDNA_SOFT_FAULT_BASE_MSG;
    fprintf(stderr, "Details: %s\n", s);
    SEDNA_SOFT_FAULT_FINALIZER;
}

void sedna_soft_fault()
{
    SEDNA_SOFT_FAULT_BASE_MSG;
    SEDNA_SOFT_FAULT_FINALIZER;
}

