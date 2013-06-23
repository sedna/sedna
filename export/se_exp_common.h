/*
 * File: se_exp_common.h
 * Copyright (C) 2013 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef _SE_EXP_COMMON_H
#define _SE_EXP_COMMON_H

#include <stdio.h>
#include <stdlib.h>
#include "libsedna.h"
#include <inttypes.h>


#ifndef _WIN32
#define SE_EXP_PATH_SEP            '/'
#else
#define SE_EXP_PATH_SEP            '\\'
#endif


#define STR_BUF_INIT_SIZE          500
#define PATH_BUF_SIZE              1024
#define Q_BUF_INIT_SIZE            1
#define RESULT_PORTION_SIZE        1024    //TO HOLD BL QUERIES
#define DOCS_FILENAME_SIZE         20
#define RF_PORTION_SIZE            512
#define DELIMITER                  "&\n"   //se_trn style "\n\\"
#define PATH_SIZE                  1100    // depends on ARG_SIZE

#define EXP_LOG_FILE_NAME          "export_log.txt"
#define IMP_LOG_FILE_NAME          "import_log.txt"
#define CR_COL_QUERY_FILE          "create_collections.xquery"
#define LOAD_DOCS_QUERY_FILE       "load_docs.xquery"
#define CR_SEC_QUERY_FILE          "create_security.xquery"
#define CR_INDEXES_QUERY_FILE      "create_indexes.xquery"
#define CR_FTINDEXES_QUERY_FILE    "create_ftindexes.xquery"
#define DB_SECURITY_DOC            "$db_security_data"
#define DB_SECURITY_DOC_NAME_TMP   "$db_security_data_exp"

#define CHECK_FULL_TEXT_SEARCH     "create_ftindexes.xquery"
#define CHECK_SECURITY             "$db_security_data.xml"

#define SEDNA_FEATURE_ENABLED       1
#define SEDNA_FEATURE_DISABLED      0

typedef struct str_buf_t {
        char *buf;
        size_t size;
        size_t d_size;
} str_buf_t;


typedef struct qbuf_t {
        char **buf;
        size_t size;
        size_t d_size;
} qbuf_t;

extern int exp_verbose;
#define FTRACE(x) do {fprintf x; if (exp_verbose==1) {FILE *log=stdout; fprintf x;} } while (0)
#define ETRACE(x) do {fprintf x; {FILE *log=stdout; fprintf x;} } while (0)


//function bulkloads XML file with filename to Sedna with docname
int bulkload_xml(struct SednaConnection *conn,const char *filename,const char *docname, FILE* log);

//function executes XQuery expression in query and outputs the result to file f
int execute_query(struct SednaConnection *conn, const char *query, FILE* f, FILE* log);

// functions executes retireve query, allocates buffer and returns it via result reference
// function returns error status
int execute_retrieve_query(char** result, struct SednaConnection *conn, const char *query, FILE* log);

// function executes the query with Sedna.
// the result is pushed to qbuf array
int fill_qbuf(struct SednaConnection *conn, qbuf_t* qbuf, const char *query, FILE* log);

// function adds string to rp_buf structure
int rp_add_data(str_buf_t *rp_buf, const char *data);

// function adds string to &qbuf structure
// if required the realloc is performed
// new_buf_size is in items, not bytes! 
int qadd(qbuf_t *qbuf, const char *q);

//function splits '/'-separated query to the array of single queries 
int split_query(char *query, qbuf_t *qbuf); 

// function reads the requested file and returns the buffer with the query 
char* read_query(char *filename); 

// function writes '/'-separated script from array of single queries 
int write_xquery_script(qbuf_t *qbuf,const char * filename);

// function executes '/'-separated script of XQuery expressions
int execute_multiquery(struct SednaConnection *conn, char *query, FILE* log); 

// function checks in a new transaction that sedna feature is enabled
int check_sedna_feature(struct SednaConnection *conn, const char *query, FILE* log);

// function checks whether the sedna error was fatal or not
int getSednaErrorStatus(const char *errorMessage);


#endif /* _SE_EXP_COMMON_H */
