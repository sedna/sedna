/*
 * File:  term_ile.h
 * Copyright (C) 2007 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef ILE_H

#ifndef _WIN32
#include <unistd.h>
#else
#include <io.h>
#define isatty(fd) _isatty(fd)
#define fileno(file) _fileno(file)
#endif

#include <cstdio>

int ile_init();
void ile_deinit();
char *ile_gets(size_t * sz);
const char *get_query_from_term_buffer();

#endif

