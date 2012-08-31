#ifndef _ERRORS_H_
#define _ERRORS_H_

#include "u/u.h"

SE_EXTERN_C void   __sys_call_error(const char *filename, int lineno, const char *funcname, const char *sys_call, const void* arg);
SE_EXTERN_C void   __u_call_error(const char *filename, int lineno, const char *funcname, const char *message);

#define CHECK_ENV(F, CODE, ERROR) if (0 != F) { throw SYSTEM_ENV_EXCEPTION(ERROR); }

SE_EXTERN_C void DumpFaultInfo();

#define SEDNA_SOFT_FAULT_BASE_MSG \
    fprintf(stderr, "SEDNA Message: FATAL ERROR\n"); \
    DumpFaultInfo(); \
    fprintf(stderr, "System error. This error means system malfunction.\n")

#define SEDNA_SOFT_FAULT_FINALIZER \
    fflush(stderr); \
    uExitProcess(1, __sys_call_error)

#endif /* _ERRORS_H_ */
