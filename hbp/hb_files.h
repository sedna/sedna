/*
 * File:  hb_main.h - Procedures for file copying, making directory, etc.
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef HB_FILES_H
#define HB_FILES_H

// prepare distance directory (make hot-backup directory with current timestamp)
int hbPrepareDistance(const char *hb_dir_name, const char *hb_db_name);

// copy all files (vmm.dat, ph-file, cfg-file, log-files, data-file)
int hbCopyFile(char *file_path);

// makes cleanup of hot-backup files in case of failure
void hbMakeCleanup();

#endif
