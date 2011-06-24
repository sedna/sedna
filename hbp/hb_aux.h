/*
 * File:  hb_aux.h - Auxillary hot-backup procedures (parsing command line)
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _HB_AUX_H
#define _HB_AUX_H

extern int   hb_checkpoint;
extern char hb_dir_name[];
extern char hb_db_name[];
extern int hb_port;
extern char hb_address[];
extern int hb_timestamp;
extern int hb_mkdir;
extern char hb_incr_mode[];

// this function parses command line and fetches parameters
void hbParseCommandLine(int argc, char **argv); // parses command line

// this function tries to parse sednaconf to find port number
void hbGetDefaultValues();

#endif

