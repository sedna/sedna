/*
 * File: se_exp_export.h
 * Copyright (C) 2013 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef _SE_EXP_EXPORT_H
#define _SE_EXP_EXPORT_H

int export(const char *path,
           const char *url,
           const char *db_name,
           const char *login,
           const char *password,
           int ro,
           int idx_skip);

#endif /* _SE_EXP_EXPORT_H */
