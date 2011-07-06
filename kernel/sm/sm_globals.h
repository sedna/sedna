/*
 * File:  sm_globals.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _SM_GLOBALS_H
#define _SM_GLOBALS_H

#include <string>

#include "common/sedna.h"

#include "common/u/usem.h"
#include "common/u/uthread.h"
#include "common/SSMMsg.h"
#include "common/config.h"


/*******************************************************************************
********************************************************************************
  GLOBAL VARIABLES 
********************************************************************************
*******************************************************************************/

/* Some of these variables SM specific, some of them are shared with CDB */
namespace sm_globals {
    extern int    bufs_num;                                /* Number of pages to allocate for buffer memory */
    extern double upd_crt;                                 /* Advance snapshot criterion */
    extern int    max_log_files;                           /* Maximum log files */
    extern int    tmp_file_initial_size;                   /* Temp file initial size */
    extern char   db_name[SE_MAX_DB_NAME_LENGTH + 1];      /* Must be set with database name */
    extern char   db_files_path[U_MAX_PATH + 1];           /* Must be set with path to the database files (dbname_files folder) */
    extern int    background_mode;
}

/*******************************************************************************
********************************************************************************
  SM Specific Functions
********************************************************************************
*******************************************************************************/

/* Must be called after parse_sm_command_line */
void         setup_sm_globals          (gov_config_struct* cfg, int db_id);
/* Parses command line SM's arguments */
void         parse_sm_command_line     (int argc, char** argv);
/* Must be called after parse_sm_command_line */
std::string  construct_sm_command_line (char** argv);

void         register_sm_on_gov();

#endif /* _SM_GLOBALS_H */
