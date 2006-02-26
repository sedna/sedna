#include <stdio.h>
#include <stdlib.h>
#include "libsedna.h"

#ifndef _WIN32
 // for linux file system
#define SE_EXP_PATH_SEP '/'
#else
 // for windows file system
#define SE_EXP_PATH_SEP '\\'
#endif



#define STR_BUF_INIT_SIZE 500
#define Q_BUF_INIT_SIZE 1
#define RESULT_PORTION_SIZE 1024 //TO HOLD BL QUERIES
#define DOCS_FILENAME_SIZE 20
#define RF_PORTION_SIZE 512
#define DELIMITER "\n\\"
#define PATH_SIZE 1100 // depends on ARG_SIZE

#define EXP_LOG_FILE_NAME        "export_log.txt"
#define IMP_LOG_FILE_NAME        "import_log.txt"
#define CR_COL_QUERY_FILE        "create_collections.xquery"
#define LOAD_DOCS_QUERY_FILE     "load_docs.xquery"
#define CR_SEC_QUERY_FILE        "create_security.xquery"
#define CR_INDEXES_QUERY_FILE    "create_indexes.xquery"
#define DB_SECURITY_DOC          "db_security_data"
#define DB_SECURITY_DOC_TMP      "db_security_data_exp"

typedef struct str_buf_t {
	char *buf;
	int size;
	int d_size;
} str_buf_t;


typedef struct qbuf_t {
	char **buf;
	int size;
	int d_size;
} qbuf_t;

extern int exp_verbose;
#define FTRACE(x) {fprintf x; if (exp_verbose==1) {FILE *log=stdout; fprintf x;} } 
#define ETRACE(x) {fprintf x; {FILE *log=stdout; fprintf x;} } 


//function bulkloads XML file with filename to Sedna with docname
int bulkload_xml(struct SednaConnection *conn,const char *filename,const char *docname, FILE* log);

//function executes XQuery expression in query and outputs the result to file f
int execute_query(struct SednaConnection *conn, const char *query, FILE* f, FILE* log);

//function executes XQuery expression in query and returns allocated string with result
char* execute_query_str(struct SednaConnection *conn, const char *query, FILE* log);

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









