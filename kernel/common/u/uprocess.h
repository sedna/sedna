/*
 * File:  uprocess.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _UPROCESS_H
#define _UPROCESS_H


#include "common/u/u.h"
#include "common/u/uthread.h"
#include "common/u/usecurity.h"


#ifdef _WIN32
#define U_DETACHED_PROCESS			DETACHED_PROCESS

typedef HANDLE UPHANDLE;
typedef DWORD  UPID;
#else
#include <signal.h>
#define U_DETACHED_PROCESS			1

typedef int    UPHANDLE;
typedef pid_t  UPID;
#endif



#ifdef __cplusplus
extern "C" {
#endif

/* return value 0 indicates success */
int uSetEnvironmentVariable(const char* name, const char* value, sys_call_error_fun fun);
int uGetEnvironmentVariable(const char* name, char* buf, int size, sys_call_error_fun fun);

/* return value 0 indicates success */
int uGetCurProcessWorkingSetSize(usize_t *MinWorkingSetSize, usize_t *MaxWorkingSetSize, sys_call_error_fun fun);
int uSetCurProcessWorkingSetSize(usize_t MinWorkingSetSize, usize_t MaxWorkingSetSize, sys_call_error_fun fun);

/* return value 0 indicates success */
int uCreateProcess(
           char *command_line,		/* command line string */
           bool inherit_handles,	/* handle inheritance option */
           const char *cur_dir,		/* current directory name */
           UFlag flags,
           UPHANDLE *process_handle,
           UTHANDLE *thread_handle,
           UPID *process_id,
           UTID *thread_id,
           USECURITY_ATTRIBUTES* sa,
           sys_call_error_fun fun
    );

/* return value 0 indicates success */
int uTerminateProcess(UPID pid, UPHANDLE h, int exit_code, sys_call_error_fun fun);
/* momently terminates current process */
void uExitProcess(int exit_code, sys_call_error_fun fun);

UPID uGetCurrentProcessId(sys_call_error_fun fun);
/* return value: 1 - exists, 0 - does not exist, -1 - error */
int uIsProcessExist(UPID pid, UPHANDLE h, sys_call_error_fun fun);

int uOpenProcess(UPID pid, UPHANDLE /*out*/ *h, sys_call_error_fun fun);

int uCloseProcess(UPHANDLE h, sys_call_error_fun fun);

int uWaitForChildProcess(UPID pid, UPHANDLE h, int *status, sys_call_error_fun fun);
int uWaitForProcess(UPID pid, UPHANDLE h, sys_call_error_fun fun);

int uNonBlockingWaitForChildProcess(UPID pid);


extern char *program_name_argv_0;
/* The result if written to buf. The size of the buf should be
 * not less than U_MAX_PATH + 1. The function return buf. */
char* uGetImageProcPath(char *buf, sys_call_error_fun fun);

#ifdef __cplusplus
}
#endif


#endif
