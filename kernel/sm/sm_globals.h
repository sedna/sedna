/*
 * File:  sm_globals.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _SM_GLOBALS_H
#define _SM_GLOBALS_H

#include "sedna.h"
#include <string>
#include "argtable.h"
#include "usem.h"
#include "uthread.h"



/*******************************************************************************
********************************************************************************
  GLOBAL VARIABLES
********************************************************************************
*******************************************************************************/

// number of memory buffers
extern int bufs_num;

// max number of transactions allowed by SM
extern int max_trs_num;

//phys log extending portion
extern int phys_log_ext_portion;

//phys log size (retrieved from cfg file)
extern int phys_log_size;

// database name
extern char db_name[];

// path to db files
extern char *db_files_path;

void setup_globals();

void setup_sm_globals();

void unregister_sm_on_gov();

void register_sm_on_gov();



//variables for parsing command line
extern int sm_help;
extern int sm_version;

extern int background_mode; 
extern int __bufs_num__;
extern int __max_trs_num__;

extern int write_phys_log;

struct CfgParserContext
{
   std::string tag_name;
   std::string content;
};

extern const size_t narg;

extern arg_rec sm_argtable[];

/* global variables for checkpoint */

extern USemaphore wait_for_checkpoint;
extern USemaphore checkpoint_sem;
extern USemaphore concurrent_ops_sem;
extern USemaphore wait_for_recovery;

extern UTHANDLE  checkpoint_thread_dsc;

extern bool shutdown_checkpoint_thread;
#endif
