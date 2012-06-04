/*
 * File:  hb_files.h - procedures for making db-specific file names (logical log, ph-file, data file etc.)
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _HB_FILES_H
#define _HB_FILES_H

#include "u/u.h"

// notice: this functions DO NOT add trailing zero if length of a buf string equals maxlen (strncpy semantics)
// all functions return length of the result buf string and -1 in the case of error

// make data file name and write it to buf
// return: length of str; (unsigned)-1 in case of error
unsigned hbMakeDataFileName(char *buf, unsigned int maxlen, const char *dbname);

// make llog file name and write it to buf
// return: length of str; (unsigned)-1 in case of error
unsigned hbMakeLogFileName(char *buf, unsigned int maxlen, const char *dbname, uint64_t lnum);

// retrieves db config file name
// return: length of str; (unsigned)-1 in case of error
unsigned hbMakeConfFileName(char *buf, unsigned int maxlen, const char *dbname);

#endif
