/*
 * File:  exceptions.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <stdio.h>
#include "exceptions.h"
#include "uprocess.h"


void sedna_soft_fault(const SednaException &e)
{
    fprintf(stderr, "SEDNA Message: FATAL ERROR\n");
    fprintf(stderr, "System error. This error means system malfunction.\n");
    if (e.getDescription().length() != 0)
        fprintf(stderr, "Details: %s\n", e.getDescription().c_str());
#if (EL_DEBUG == 1)
    fprintf(stderr, "Position: [%s:%s:%d]\n", e.getFile().c_str(), e.getFunction().c_str(), e.getLine());
#endif
    fflush(stderr);
    uExitProcess(1);
}

void sedna_soft_fault()
{
    fprintf(stderr, "SEDNA Message: FATAL ERROR\n");
    fprintf(stderr, "System error. This error means system malfunction.\n");
    fflush(stderr);
    uExitProcess(1);

}

