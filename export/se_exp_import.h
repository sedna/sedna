/*
 * File: se_exp_import.h
 * Copyright (C) 2013 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef _SE_EXP_IMPORT_H
#define _SE_EXP_IMPORT_H

int import(const char *path,
           const char *url,
           const char *db_name,
           const char *login,
           const char *password,
           int sec_import,
           int idx_skip);

#endif /* _SE_EXP_IMPORT_H */
