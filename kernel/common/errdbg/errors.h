#ifndef _ERRORS_H_
#define _ERRORS_H_

#include "u/u.h"

SE_EXTERN_C void DumpFaultInfo();

#define SEDNA_SOFT_FAULT_BASE_MSG \
    fprintf(stderr, "SEDNA Message: FATAL ERROR\n"); \
    DumpFaultInfo(); \
    fprintf(stderr, "System error. This error means system malfunction.\n")

#define SEDNA_SOFT_FAULT_FINALIZER \
    fflush(stderr); \
    uExitProcess(1, __sys_call_error)

#endif /* _ERRORS_H_ */
