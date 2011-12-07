/*
 * File:  libsedna.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */
#ifndef _LIBBASE_H
#define _LIBBASE_H

#ifdef __cplusplus
extern "C"
{
#endif

#ifdef _WIN32
#include <windows.h>
#else
#endif

#include <stdio.h>
#include "sp_defs.h"


#define QUERY_EXECUTION_TIME                       1024
#define BULK_LOAD_PORTION                         (SE_SOCKET_MSG_BUF_SIZE-5)


#define SEDNA_OPERATION_SUCCEEDED                 (-1)

#define SEDNA_SESSION_OPEN                          1
#define SEDNA_SESSION_CLOSED                        2
#define SEDNA_AUTHENTICATION_FAILED               (-3)
#define SEDNA_OPEN_SESSION_FAILED                 (-4)
#define SEDNA_CLOSE_SESSION_FAILED                (-5)

#define SEDNA_QUERY_SUCCEEDED                       6
#define SEDNA_QUERY_FAILED                        (-7)

#define SEDNA_UPDATE_SUCCEEDED                      8
#define SEDNA_UPDATE_FAILED                       (-9)

#define SEDNA_BULK_LOAD_SUCCEEDED                  10
#define SEDNA_BULK_LOAD_FAILED                   (-11)

#define SEDNA_BEGIN_TRANSACTION_SUCCEEDED          12
#define SEDNA_BEGIN_TRANSACTION_FAILED           (-13)

#define SEDNA_ROLLBACK_TRANSACTION_SUCCEEDED       14
#define SEDNA_ROLLBACK_TRANSACTION_FAILED        (-15)

#define SEDNA_COMMIT_TRANSACTION_SUCCEEDED         16
#define SEDNA_COMMIT_TRANSACTION_FAILED          (-17)

#define SEDNA_NEXT_ITEM_SUCCEEDED                  18
#define SEDNA_NEXT_ITEM_FAILED                   (-19)

#define SEDNA_NO_ITEM                            (-20)
#define SEDNA_RESULT_END                         (-21)

#define SEDNA_DATA_CHUNK_LOADED                    23

#define SEDNA_ERROR                              (-24)


#define SEDNA_TRANSACTION_ACTIVE                   25
#define SEDNA_NO_TRANSACTION                       26

#define SEDNA_CONNECTION_OK                        27
#define SEDNA_CONNECTION_CLOSED                    28
#define SEDNA_CONNECTION_FAILED                  (-29)

#define SEDNA_AUTOCOMMIT_OFF                       30
#define SEDNA_AUTOCOMMIT_ON                        31

#define SEDNA_SET_ATTRIBUTE_SUCCEEDED              32
#define SEDNA_GET_ATTRIBUTE_SUCCEEDED              33

#define SEDNA_RESET_ATTRIBUTES_SUCCEEDED           34
    
#define SEDNA_BOUNDARY_SPACE_PRESERVE_OFF          35
#define SEDNA_BOUNDARY_SPACE_PRESERVE_ON           36

#define SEDNA_CDATA_PRESERVE_OFF                   37
#define SEDNA_CDATA_PRESERVE_ON                    38

    
    enum SEattr {SEDNA_ATTR_AUTOCOMMIT, 
                 SEDNA_ATTR_SESSION_DIRECTORY, 
                 SEDNA_ATTR_DEBUG, 
                 SEDNA_ATTR_BOUNDARY_SPACE_PRESERVE_WHILE_LOAD, 
                 SEDNA_ATTR_CONCURRENCY_TYPE, 
                 SEDNA_ATTR_QUERY_EXEC_TIMEOUT,
                 SEDNA_ATTR_LOG_AMOUNT,
                 SEDNA_ATTR_MAX_RESULT_SIZE,
                 SEDNA_ATTR_CDATA_PRESERVE_WHILE_LOAD};
    
    typedef void (*debug_handler_t)(enum se_debug_info_type, const char *msg_body);
    
    struct conn_bulk_load
    {
        char bulk_load_started;
        char doc_name[SE_MAX_DOCUMENT_NAME_LENGTH+1];
        char col_name[SE_MAX_COLLECTION_NAME_LENGTH+1];
    };
    
    struct SednaConnection
    {
        char url[SE_HOSTNAMELENGTH + 1];
        char db_name[SE_MAX_DB_NAME_LENGTH + 1];
        char login[SE_MAX_LOGIN_LENGTH + 1];
        char password[SE_MAX_PASSWORD_LENGTH + 1];
        char session_directory[SE_MAX_DIR_LENGTH+1];
#ifdef _WIN32
        SOCKET socket;
#else
        int socket;
#endif
        int last_error;
        char last_error_msg[SE_SOCKET_MSG_BUF_SIZE];
        char query_time[QUERY_EXECUTION_TIME];

        char socket_keeps_data;
        char first_next;
        char result_end;
        char in_query;
        struct conn_bulk_load cbl;

        int isInTransaction;
        int isConnectionOk;

        char autocommit;

        int local_data_length;
        int local_data_offset;
        char local_data_buf[SE_SOCKET_MSG_BUF_SIZE];

        struct msg_struct msg;
        
        debug_handler_t debug_handler;
        
        char boundary_space_preserve;
        char cdata_preserve;
        int query_timeout;
        int max_result_size;
    };

#ifdef _WIN32
#define SEDNA_CONNECTION_INITIALIZER {"", "", "", "", "", INVALID_SOCKET, -1, "", "", 0, 0, 0, 0, {0, "", ""}, SEDNA_NO_TRANSACTION, SEDNA_CONNECTION_CLOSED, 1, 0, 0, "", {0, 0, ""}, NULL, 0, 0, 0, 0}
#else
#define SEDNA_CONNECTION_INITIALIZER {"", "", "", "", "", -1, -1, "", "", 0, 0, 0, 0, {0, "", ""}, SEDNA_NO_TRANSACTION, SEDNA_CONNECTION_CLOSED, 1, 0, 0, "", {0, 0, ""}, NULL, 0, 0, 0, 0}
#endif

    int SEconnect(struct SednaConnection *conn, const char *host, const char *db_name, const char *login, const char *password);

    int SEclose(struct SednaConnection *conn);

    int SEbegin(struct SednaConnection *conn);

    int SErollback(struct SednaConnection *conn);

    int SEcommit(struct SednaConnection *conn);

/*query_file - file with a query*/
    int SEexecuteLong(struct SednaConnection *conn, const char* query_file_path);

    int SEexecute(struct SednaConnection *conn, const char *query);

/*returns number of bytes actually read to the buffer*/
/* 0 - if there is no data to read*/
/* negative if error (use SEgetLastErrorMsg then))*/

    int SEgetData(struct SednaConnection *conn, char *buf, int bytes_to_read);

/* returns SEDNA_DATA_SENT if chunk of data was sent successfully*/
/* SEDNA_ERROR if there was errors*/
    int SEloadData(struct SednaConnection *conn, const char *buf, int bytes_to_load, const char *doc_name, const char *col_name);

/* returns SEDNA_BULK_LOAD_FAILED or SEDNA_BULK_LOAD_SUCCEEDED (or SEDNA_ERROR)*/
    int SEendLoadData(struct SednaConnection *conn);

    int SEnext(struct SednaConnection *conn);

    int SEgetLastErrorCode(struct SednaConnection *conn);

    const char *SEgetLastErrorMsg(struct SednaConnection *conn);

    int SEconnectionStatus(struct SednaConnection *conn);

    int SEtransactionStatus(struct SednaConnection *conn);

    const char *SEshowTime(struct SednaConnection *conn);

    int SEsetConnectionAttr(struct SednaConnection *conn, enum SEattr attr, const void* attrValue, int attrValueLength);

    int SEgetConnectionAttr(struct SednaConnection *conn, enum SEattr attr, void* attrValue, int* attrValueLength);
    
    int SEresetAllConnectionAttr(struct SednaConnection *conn);

	void SEsetDebugHandler(struct SednaConnection *conn, debug_handler_t _debug_handler_);

#ifdef __cplusplus
}
#endif

#endif
