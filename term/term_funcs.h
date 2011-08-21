/*
 * File:  term_funcs.h
 * Copyright (C) 2011 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef _TERM_FUNCS_H
#define _TERM_FUNCS_H

void term_output1(const char *buf);

void term_output2(const char *buf, const char* arg);

void term_output2(const char *buf, int arg);

void term_output3(const char *buf, const void* arg1, const void* arg2);

void term_debug_info_output(const char *msg);

int process_commandline_query();

#endif /* _TERM_FUNCS_H */
