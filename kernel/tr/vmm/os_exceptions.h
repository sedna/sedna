/*
 * File:  os_exceptions.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _OS_EXCEPTIONS_H
#define _OS_EXCEPTIONS_H

#include "common/sedna.h"

#ifdef _WIN32

class win32_access_violation {};

#else

extern jmp_buf stack_overflow_env;
extern jmp_buf access_violation_env;
extern jmp_buf vmm_is_busy_env;

#endif




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
