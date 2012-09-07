/*
 * File:  os_exceptions.h
 * Copyright (C) 2010 ISP RAS
  *The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef _OS_EXCEPTIONS_H
#define _OS_EXCEPTIONS_H

#include "common/sedna.h"
#include "auxiliary/commutil.h"

extern "C" int TRmain(int argc, char **argv);
extern "C" int IsAccessViolationNonFatal(void *addr, void *context);

#ifdef _WIN32

#define OS_EXCEPTIONS_INSTALL_HANDLER

#else
void unix_sigsegv_signal_handler(int signo, siginfo_t *info, void *cxt);

extern jmp_buf access_violation_env;

#define OS_EXCEPTIONS_INSTALL_HANDLER   {   struct sigaction sigsegv_act; \
                                            memset(&sigsegv_act, '\0', sizeof(struct sigaction)); \
                                            sigsegv_act.sa_sigaction = unix_sigsegv_signal_handler; \
                                            sigsegv_act.sa_flags = SA_SIGINFO; \
                                            if (sigaction(SIGSEGV, &sigsegv_act, NULL) == -1) throw USER_EXCEPTION(SE1033); \
                                            if (setjmp(access_violation_env) != 0) throw SYSTEM_EXCEPTION("Access violation or stack overflow");   }

#endif /* _WIN32 */
#endif /* _OS_EXCEPTIONS_H */
