/*
 * File:  uprocess.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _UPROCESS_H
#define _UPROCESS_H


#include "u/u.h"
#include "u/uthread.h"
#include "u/usecurity.h"


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
int uSetEnvironmentVariable(const char* name, const char* value, char** buffer, sys_call_error_fun fun);
int uGetEnvironmentVariable(const char* name, char* buf, uint32_t size, sys_call_error_fun fun);

/* return value 0 indicates success */
int uGetCurProcessWorkingSetSize(size_t *MinWorkingSetSize, size_t *MaxWorkingSetSize, sys_call_error_fun fun);
int uSetCurProcessWorkingSetSize(size_t MinWorkingSetSize, size_t MaxWorkingSetSize, sys_call_error_fun fun);

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

/*  uIsProcessExist, uOpenProcess
 *  Return values: 0 - upon successfull completition, 
 *                -1 - process doesn't exist, 
 *                -2 - some error occurred
 */
int uIsProcessExist(UPID pid, UPHANDLE h, sys_call_error_fun fun);
int uOpenProcess(UPID pid, UPHANDLE /*out*/ *h, sys_call_error_fun fun);

int uCloseProcessHandle(UPHANDLE h, sys_call_error_fun fun);

int uWaitForChildProcess(UPID pid, UPHANDLE h, int *status, sys_call_error_fun fun);
int uWaitForProcess(UPID pid, UPHANDLE h, sys_call_error_fun fun);

int uNonBlockingWaitForChildProcess(UPID pid);


extern char *program_name_argv_0;

/*  The result if written to buf. The size of the buf should be
 *  not less than U_MAX_PATH + 1. The function return buf. 
 */

char* uGetImageProcPath(char* buf, sys_call_error_fun fun);

#ifdef __cplusplus
}
#endif 

#endif /* _UPROCESS_H */
