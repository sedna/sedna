/*
 * File:  event_log.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

/* 
 * Architecture of the event log: 
 * Event logger is represented as a daemon thread in a main process (in the 
 * governor). Other processes should call init function before writing to log 
 * (otherwise the message would be written to stderr). 
 * Event log writing macros are thread safe.
 */

#ifndef _EVENT_LOG_H
#define _EVENT_LOG_H

#include "common/base.h"
#include "common/globalobjects/globalnames.h"

/* Error level codes */
#define EL_DBG      8    /* debugging messages */
#define EL_LOG      7    /* server operational messages */
#define EL_COMM     6    /* client communication problems */
#define EL_INFO	    5    /* informative messages (like current values of some parameters) */
#define EL_WARN	    4    /* warning - inform that something is going not as well as it could be */
#define EL_ERROR    3    /* user error (corresponds to SednaUserException) */
#define EL_SYS      2    /* system call error */
#define EL_FATAL    1    /* fatal error - perform soft fault (corresponds to SednaSystemException) */


#define SE_EVENT_LOG_FILENAME_LEN      128
#define SE_EVENT_LOG_FUNCNAME_LEN      128
#define SE_EVENT_LOG_CONTENT_LEN       1024

#define SE_SOFT_FAULT_LOG_DIR          "fault-"
#define SE_LAST_SOFT_FAULT_DIR         "fault-last"
#define SE_ASSERT_FAILED_FILE_NAME     "assert_failed"
#define SE_SOFT_FAULT_LOG_CONTENT_LEN  1024

/* Component codes */
#define EL_UNK      0
#define EL_CDB      1
#define EL_DDB      2
#define EL_GOV      3
#define EL_RC       4
#define EL_SM       5
#define EL_SMSD     6
#define EL_STOP     7
#define EL_TRN      8
#define EL_RCV      9


/*----------
 * Error reporting API for short messages (less than SE_EVENT_LOG_CONTENT_LEN)
 * to be used in this way:
 *      elog(EL_ERROR,
 *           ("Not enough resources to complete the query (mem_free = %d, mem_used = %d)", mem_free, mem_used));
 *----------
 */
#define elog(elevel, rest) \
        ((elevel) <= event_log_elevel \
         ? ((event_log_initialized) ? (event_log_short_msg_macro((elevel), \
                                                                 __FILE__, \
                                                                 __LINE__, \
                                                                 __SE_FUNCTION__, \
                                                                 (event_log_short_msg_param rest))) \
                                    : (event_log_short_write_to_stderr rest)) \
         : 0)

/*----------
 * Error reporting API for long messages to be used in this way:
 *      elog_long(EL_LOG, "User query: ", query);
 *----------
 */
#define elog_long(elevel, short_str, long_str) \
        ((elevel) <= event_log_elevel \
         ? ((event_log_initialized) ? (event_log_long_msg((elevel), \
                                                          __FILE__, \
                                                          __LINE__, \
                                                          __SE_FUNCTION__, \
                                                          (short_str), \
                                                          (long_str))) \
                                    : (event_log_long_write_to_stderr((short_str), (long_str)))) \
         : 0)



#ifdef __cplusplus
extern "C" {
#endif

/*
 * Event log set up parameters
 */
extern int event_log_elevel;            /* current event log error level */
extern int event_log_location;          /* output the originator of record (GOV, SM, etc.)*/
extern int event_log_detailed_location; /* output filename, function, line */
extern int event_log_recommended_size;  /* after exceeding this size the log file rotate is done */
extern int event_log_truncate;          /* delete file after log rotation? */

extern int event_log_initialized;

/* ============================================================================
 * Client functions for logging short messages
 * ============================================================================
 */
int event_log_short_msg_macro(int elevel,
                              const char *filename,
                              int lineno,
                              const char *funcname,
                              int content_len);

int event_log_short_msg_param(const char *s, 
                              ...);

int event_log_short_msg(int elevel, 
                        const char *filename, 
                        int lineno, 
                        const char *funcname, 
                        const char *s, 
                        ...);

int event_log_short_write_to_stderr(const char *s, ...);


/* ============================================================================
 * Client functions for logging long messages
 * ============================================================================
 */
int event_log_long_msg(int elevel,
                       const char *filename,
                       int lineno,
                       const char *funcname,
                       const char *short_str,
                       const char *long_str);

int event_log_long_write_to_stderr(const char *short_str, const char *long_str);


/* ============================================================================
 * Init/release functions
 * ============================================================================
 */

/*
 * Start/shutdown event logging daemon 
 * (call these functions in the 'main' process)
 * 
 */
int event_logger_start_daemon(int elevel, global_name shm_name, global_name sems_name);
int event_logger_shutdown_daemon(global_name shm_name);

/*
 * Init/release event log 
 * (call these functions in the 'client' processes)
 */
int event_logger_init(int component, const char* component_detail, global_name shm_name, global_name sems_name);
int event_logger_release();


/* ============================================================================
 * Set/get functions
 * ============================================================================
 */

/*
 * Set session id for trn process
 */
int event_logger_set_sid(int sid);

/*
 * Set transaction id for trn process
 */
int event_logger_set_trid(int trid);

/*
* Converts level provided from the command line/configuration file to
* the one of the known by event log (EL_LOG, EL_ERRORS, etc).
*/
int el_convert_log_level(int level);

/*
 * Creates a separate file with soft fault log (log_message) for the component (component).
 */
void sedna_soft_fault_log(const char* log_message, int  component);

/*
 *  Sedna soft fault function. 
 *  !! two sedna_soft_fault functions are defined in exceptions.h 
 */

void sedna_soft_fault(int component);

#ifdef __cplusplus
}
#endif


#endif /*_EVENT_LOG_H*/
