/*
 * File:  os_exceptions.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

/* os_exceptions.h now obsolete */ 

#ifndef _OS_EXCEPTIONS_H
#define _OS_EXCEPTIONS_H

#include "common/sedna.h"


#ifdef _WIN32

class win32_access_violation {};

#define OS_EXCEPTIONS_INSTALL_HANDLER       OS_exceptions_handler::install_handler();

#else
void unix_sigsegv_signal_handler(int signo, siginfo_t *info, void *cxt);

//extern jmp_buf stack_overflow_env;
extern jmp_buf access_violation_env;
extern jmp_buf vmm_is_busy_env;

#define OS_EXCEPTIONS_INSTALL_HANDLER   {   struct sigaction sigsegv_act; \
                                            memset(&sigsegv_act, '\0', sizeof(struct sigaction)); \
                                            sigsegv_act.sa_sigaction = unix_sigsegv_signal_handler; \
                                            sigsegv_act.sa_flags = SA_SIGINFO; \
                                            if (sigaction(SIGSEGV, &sigsegv_act, NULL) == -1) throw USER_EXCEPTION(SE1033); \
                                            if (setjmp(access_violation_env) != 0) throw SYSTEM_EXCEPTION("Access violation or stack overflow");   }

#endif /* _WIN32 */


class OS_exceptions_handler
{
private:
    static bool critical_section;

public:
	static void install_handler();
    static void enter_stack_overflow_critical_section();
    static void leave_stack_overflow_critical_section();
    static bool is_in_stack_overflow_critical_section() { return critical_section; }
};


#endif
