
#ifndef _TERM_FUNCS_H
#define _TERM_FUNCS_H

void term_output1(const char *buf);

void term_output2(const char *buf, const void* arg);

void term_output3(const char *buf, const void* arg1, const void* arg2);

void term_debug_info_output(const char *msg);

int process_commandline_query();

#endif
