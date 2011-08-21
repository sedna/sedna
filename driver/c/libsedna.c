/*
 * File:  libsedna.c
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "libsedna.h"
#include "common/errdbg/error_codes.h"
#include "common/errdbg/d_printf.h"
#include "common/sp.h"
#include "common/u/uutils.h"
#include "common/u/usocket.h"
#include "common/u/uhdd.h"

#ifdef _MSC_VER
#pragma comment(lib,"Advapi32.lib")
#pragma comment(lib,"WS2_32.lib")
#endif

/******************************************************************************
 * Internal Driver Functions
 *****************************************************************************/

static void setServerErrorMsg(struct SednaConnection *conn, struct msg_struct msg)
{
    int length;
    if (msg.length <= 0) return;
    net_int2int(&(conn->last_error), msg.body);
    net_int2int(&length, msg.body + 5);
    if (length <= 0) return;
    memcpy(conn->last_error_msg, msg.body + 9, length);
    conn->last_error_msg[length] = '\0';
}

static void setDriverErrorMsg(struct SednaConnection *conn, int error_code, const char* details)
{
    conn->last_error = error_code;
    strcpy(conn->last_error_msg, "SEDNA Message: ERROR ");        
    strcat(conn->last_error_msg, user_error_code_entries[conn->last_error].code);
    strcat(conn->last_error_msg, "\n");
    strcat(conn->last_error_msg, user_error_code_entries[conn->last_error].descr);
    if (details != NULL)
    {
        strcat(conn->last_error_msg, "\nDetails: ");
        strcat(conn->last_error_msg, details);
    }
}

static void clearLastError(struct SednaConnection *conn)
{
    conn->last_error = SEDNA_OPERATION_SUCCEEDED;
}

static void release(struct SednaConnection *conn)
{
    ushutdown_close_socket(conn->socket, NULL);
    uSocketCleanup(NULL);
}

static void connectionFailure(struct SednaConnection *conn, int error_code, const char* details, struct msg_struct* msg)
{
    if (msg != NULL)
        setServerErrorMsg(conn, *msg);
    else
        setDriverErrorMsg(conn, error_code, details);
    conn->isInTransaction = SEDNA_NO_TRANSACTION;
    conn->isConnectionOk = SEDNA_CONNECTION_FAILED;
}

/* returns 1 if the document is currently loading [into the collection], 0 otherwise */
static char isBulkLoadStarted(struct SednaConnection *conn)
{
    return conn->cbl.bulk_load_started;
}
/* returns 1 if this document is currently loading [into this collection], 0 otherwise */
static char isBulkLoadOf(struct SednaConnection *conn, const char* doc_name, const char* col_name)
{
    if ((conn->cbl.bulk_load_started) && (strcmp(conn->cbl.doc_name, doc_name) == 0))
    {
        if (col_name)
        {
            if(0 == strcmp(conn->cbl.col_name, col_name)) return 1;
            else return 0;
        }
        else 
            return 1;
    }
    return 0;
}

/* sets bulk load of the document [into the collection] is started */
static void setBulkLoadStarted(struct SednaConnection *conn, const char* doc_name, const char* col_name)
{
    conn->cbl.bulk_load_started = 1;
    strcpy(conn->cbl.doc_name, doc_name);
    if (col_name)
        strcpy(conn->cbl.col_name, col_name);
}

/* sets bulk load of the document [into the collection] is finished */
static void setBulkLoadFinished(struct SednaConnection *conn)
{
    conn->cbl.bulk_load_started = 0;
    strcpy(conn->cbl.doc_name, "");
    strcpy(conn->cbl.col_name, "");
}

static int begin_handler(struct SednaConnection *conn)
{
    /* send 210 - BeginTransaction*/
    conn->msg.instruction = se_BeginTransaction;
    conn->msg.length = 0;
    if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3006, "Connection was broken while trying to begin transaction", NULL);
        return SEDNA_ERROR;
    }

    /* read 100 or 230 - BeginTransactionOk or 240 - BeginTransactionFailed*/
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, "Connection was broken while trying to begin transaction", NULL);
        return SEDNA_ERROR;
    }

    if (conn->msg.instruction == se_ErrorResponse)
    {
        setServerErrorMsg(conn, conn->msg);
        return SEDNA_BEGIN_TRANSACTION_FAILED;
    }
    else if (conn->msg.instruction == se_BeginTransactionFailed)        /* BeginTransactionFailed */
    {
        setServerErrorMsg(conn, conn->msg);
        return SEDNA_BEGIN_TRANSACTION_FAILED;
    }
    else if (conn->msg.instruction == se_BeginTransactionOk)    /* BeginTransactionOk */
    {
        conn->in_query = 0;
        conn->isInTransaction = SEDNA_TRANSACTION_ACTIVE;
        return SEDNA_BEGIN_TRANSACTION_SUCCEEDED;
    }
    else
    {
        connectionFailure(conn, SE3008, NULL, NULL);            /* "Unknown message from server" */
        return SEDNA_BEGIN_TRANSACTION_FAILED;
    }
}

static int commit_handler(struct SednaConnection *conn)
{
    conn->isInTransaction = SEDNA_NO_TRANSACTION;

    /* send 220 - CommitTransaction*/
    conn->msg.instruction = se_CommitTransaction;
    conn->msg.length = 0;
    if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3006, "Connection was broken while trying to commit transaction", NULL);
        return SEDNA_ERROR;
    }

    /* read 100 or 250 - CommitTransactionOk or 260 - CommitTransactionFailed*/
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, "Connection was broken while trying to commit transaction", NULL);
        return SEDNA_ERROR;
    }

    if (conn->msg.instruction == se_ErrorResponse)
    {
        setServerErrorMsg(conn, conn->msg);
        return SEDNA_COMMIT_TRANSACTION_FAILED;
    }
    else if (conn->msg.instruction == se_CommitTransactionFailed)    /* CommitTransactionFailed */
    {
        setServerErrorMsg(conn, conn->msg);
        return SEDNA_COMMIT_TRANSACTION_FAILED;
    }
    else if (conn->msg.instruction == se_CommitTransactionOk)   /* CommitTransactionOk */
    {
        return SEDNA_COMMIT_TRANSACTION_SUCCEEDED;
    }
    else
    {
        connectionFailure(conn, SE3008, NULL, NULL);            /* "Unknown message from server" */
        return SEDNA_COMMIT_TRANSACTION_FAILED;
    }
}

static int rollback_handler(struct SednaConnection *conn)
{
    conn->isInTransaction = SEDNA_NO_TRANSACTION;

    /* send 225 - RollbackTransaction*/
    conn->msg.instruction = se_RollbackTransaction;
    conn->msg.length = 0;
    if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3006, "Connection was broken while trying to rollback transaction", NULL);
        return SEDNA_ERROR;
    }

    /* read 100 or 255 - RollbackTransactionOk or 265 - RollbackTransactionFailed*/
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, "Connection was broken while trying to rollback transaction", NULL);
        return SEDNA_ERROR;
    }

    if (conn->msg.instruction == se_ErrorResponse)
    {
        setServerErrorMsg(conn, conn->msg);
        return SEDNA_ROLLBACK_TRANSACTION_FAILED;
    }
    else if (conn->msg.instruction == se_RollbackTransactionFailed)     /* RollbackTransactionFailed */
    {
        setServerErrorMsg(conn, conn->msg);
        return SEDNA_ROLLBACK_TRANSACTION_FAILED;
    }
    else if (conn->msg.instruction == se_RollbackTransactionOk)         /* RollbackTransactionOk */
    {
        conn->in_query = 0;
        return SEDNA_ROLLBACK_TRANSACTION_SUCCEEDED;
    }
    else
    {
        connectionFailure(conn, SE3008, NULL, NULL);            /* "Unknown message from server" */
        return SEDNA_ROLLBACK_TRANSACTION_FAILED;
    }
}

/*return 1 - clean ok*/
/*error - SEDNA_ERROR*/
static int cleanSocket(struct SednaConnection *conn)
{
    conn->local_data_length = 0;
    conn->local_data_offset = 0;
    if (!conn->socket_keeps_data)
        return 0;
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, "Connection was broken while application result retrival", NULL);
        return SEDNA_ERROR;
    }

    while ((conn->msg.instruction != se_ItemEnd) && (conn->msg.instruction != se_ResultEnd))
    {
        if (conn->msg.instruction == se_ErrorResponse)
        {
            connectionFailure(conn, 0, NULL, &(conn->msg));
            return SEDNA_ERROR;
        }
        if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3007, "Connection was broken while application result retrival", NULL);
            return SEDNA_ERROR;
        }
    }
    if (conn->autocommit)
    {
        int comm_res = commit_handler(conn);
        if(comm_res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED)
            return SEDNA_ERROR;
    }

    conn->socket_keeps_data = 0;
    if (conn->msg.instruction == se_ResultEnd)
        conn->result_end = 1;

    return 1;
}

/* Takes the data from server when execute a query 
* and decide if the query failed or succeeded
*/
static int resultQueryHandler(struct SednaConnection *conn)
{
    int _type_offset = 0;
    int url_length   = 0;
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, "Connection was broken while executing statement", NULL);
        return SEDNA_ERROR;
    }
    while (conn->msg.instruction == se_DebugInfo)
    {
        if (conn->debug_handler)
        {
            int length;
            int debug_type;
            char debug_info[SE_SOCKET_MSG_BUF_SIZE+1];
            if (conn->msg.length <= 0) 
            {
                connectionFailure(conn, SE3008, NULL, NULL);            /* "Unknown message from server" */
                return SEDNA_ERROR;
            }
            net_int2int(&debug_type, conn->msg.body);
            net_int2int(&length, conn->msg.body + 5);
            if (length <= 0)
            {
                connectionFailure(conn, SE3008, NULL, NULL);            /* "Unknown message from server" */
                return SEDNA_ERROR;
            }
            memcpy(debug_info, conn->msg.body + 9, length);
            debug_info[length] = '\0';
            conn->debug_handler(debug_type, debug_info);
        }

        if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3007, "Connection was broken while executing statement", NULL);
            return SEDNA_ERROR;
        }
    }
    if (conn->msg.instruction == se_ErrorResponse)
    {
        setServerErrorMsg(conn, conn->msg);
        conn->socket_keeps_data = 0;    /*set the flag - Socket keeps item data*/
        conn->result_end = 1;   /*set the flag - there are items*/
        conn->in_query = 0;
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        return SEDNA_QUERY_FAILED;
    }
    else if (conn->msg.instruction == se_ItemPart || conn->msg.instruction == se_ItemStart)      /* ItemPart */
    {
        if(conn->msg.instruction == se_ItemStart) 
        {
            if (conn->msg.body[2])
            {
                /* If URI is presented (protocol 4 and higher) then just skip it 
                 * 3 stands for se_ItemStart header,
                 * 1 stands for string type (0 in current implementation 
                 * 4 stands for url string length
                 */
                net_int2int(&url_length, conn->msg.body + 3 + 1); 
                _type_offset = 3 + 1 + 4 + url_length;

            }
            else 
            {
                _type_offset = 3;
            }
        }
        memcpy(conn->local_data_buf, conn->msg.body + 5 + _type_offset, conn->msg.length - 5 - _type_offset);
        conn->local_data_length = conn->msg.length - 5 - _type_offset;
        conn->local_data_offset = 0;
        conn->socket_keeps_data = 1;    /* set the flag - Socket keeps item data */
        conn->result_end = 0;           /* set the flag - there are items */
        conn->in_query = 1;

        return SEDNA_QUERY_SUCCEEDED;
    }
    else if (conn->msg.instruction == se_ItemEnd)       /* ItemEnd */
    {
        conn->local_data_length = 0;
        conn->local_data_offset = 0;
        conn->socket_keeps_data = 0;    /* set the flag - Socket does not keep item data */
        conn->result_end = 0;           /* set the flag - there are items */
        conn->in_query = 1;
        return SEDNA_QUERY_SUCCEEDED;
    }
    else if (conn->msg.instruction == se_ResultEnd)     /* ResultEnd */
    {
        conn->local_data_length = 0;
        conn->local_data_offset = 0;
        conn->socket_keeps_data = 0;    /* set the flag - Socket does not keep item data */
        conn->result_end = 1;           /* set the flag - there are no items             */
        conn->in_query = 1;
        if (conn->autocommit)
        {
            int comm_res = commit_handler(conn);
            if(comm_res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED)
                return SEDNA_ERROR;
        }
        return SEDNA_QUERY_SUCCEEDED;
    }
    else
    {
        connectionFailure(conn, SE3008, "Unknown message recieved while executing statement", NULL);            /* "Unknown message from server" */
        conn->socket_keeps_data = 0;    /*set the flag - Socket keeps item data*/
        conn->local_data_offset = 0;
        conn->local_data_length = 0;
        conn->result_end = 1;   /*set the flag - there are no items*/
        conn->in_query = 0;
        conn->isInTransaction = SEDNA_NO_TRANSACTION;

        return SEDNA_QUERY_FAILED;
    }
}

/* Send file to the client. 
 * On an ERROR - returns 1 (in this case errorcode contains code of the error), 
 *        else - returns 0 (in this case errorcode value is undefined).
 * This function is used for loading modules and bulk loading.
 */
static int 
bulkload (struct SednaConnection *conn, 
          int* errorcode) 
{
    /* Get full path and open file. */ 
    UFile file_handle = U_INVALID_FD;
    char cur_dir_abspath[SE_MAX_DIR_LENGTH+1];
    char cfile_abspath[SE_MAX_DIR_LENGTH+1];
    unsigned int already_read = 1;
    int res = 1;
    char *filename = conn->msg.body + 5;
    filename[s_min(conn->msg.length - 5, SE_SOCKET_MSG_BUF_SIZE - 6)] = '\0';

    /* Try firstly to find file in the session directory ... */
    if (uGetCurrentWorkingDirectory(cur_dir_abspath, SE_MAX_DIR_LENGTH, NULL) == NULL) {
        setDriverErrorMsg(conn, SE4602, cur_dir_abspath);
        goto BulkLoadErr;
    }
    if (uChangeWorkingDirectory(conn->session_directory, NULL) != 0) {
        setDriverErrorMsg(conn, SE4604, conn->session_directory);
        goto BulkLoadErr;
    }
    if (uGetAbsoluteFilePath(filename, cfile_abspath, SE_MAX_DIR_LENGTH, NULL) != NULL) {
    
        file_handle = uOpenFile(cfile_abspath, U_SHARE_READ, U_READ, 0, NULL);
    }

    /* restore working directory anyway */
    if (uChangeWorkingDirectory(cur_dir_abspath, NULL) != 0) {
        setDriverErrorMsg(conn, SE4604, cur_dir_abspath);
        goto BulkLoadErr;
    }

    /* ... then try current directory  ... */
    if (file_handle == U_INVALID_FD && 
        uGetAbsoluteFilePath(filename, cfile_abspath, SE_MAX_DIR_LENGTH, NULL) != NULL)  {

        file_handle = uOpenFile(cfile_abspath, U_SHARE_READ, U_READ, 0, NULL);
    }

    /* ... raise error if we still haven't found the file. */
    if(file_handle == U_INVALID_FD) {
        setDriverErrorMsg(conn, SE3017, filename);
        goto BulkLoadErr;
    }

    /* Read data from file */ 
    while ((res > 0) && (already_read != 0))
    {
        res = uReadFile(file_handle, conn->msg.body + 5, BULK_LOAD_PORTION, &already_read, NULL);
        if (res == 0) {
            setDriverErrorMsg(conn, SE3018, filename);
            goto BulkLoadErr;
        }

        if (already_read == 0) break;

        /* Send BulkLoadPortion (410) */
        conn->msg.instruction = se_BulkLoadPortion;
        conn->msg.length = 5 + already_read;
        conn->msg.body[0] = 0;
        int2net_int(already_read, conn->msg.body + 1);

        if (sp_send_msg(conn->socket, &(conn->msg)) != 0) {
            connectionFailure(conn, SE3006, "Connection was broken while application was passing bulk load portion to the server", NULL);
            uCloseFile(file_handle, NULL);
            goto SednaErr;
        }
    }

    /* Close file */
    if (!uCloseFile(file_handle, NULL)) {
        setDriverErrorMsg(conn, SE3019, NULL);
        goto SednaErr;
    }

    /* Send BulkLoadEnd (420) */
    conn->msg.instruction = se_BulkLoadEnd;
    conn->msg.length = 0;
    
    if (sp_send_msg(conn->socket, &(conn->msg)) != 0) {
        connectionFailure(conn, SE3006, 
            "Connection was broken while application was passing bulk load ending message to the server", NULL);
        goto SednaErr;
    }
    
    return 0;

BulkLoadErr:  
    conn->msg.instruction = se_BulkLoadError;
    conn->msg.length = 0;
    if (sp_send_msg(conn->socket, &(conn->msg)) != 0) {
        connectionFailure(conn, SE3006, 
            "Connection was broken while application was passing bulk load error to the server", NULL);
        goto SednaErr;
    }
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0) {
        connectionFailure(conn, SE3007, 
            "Connection was broken while application was receiving response from the server", NULL);
        goto SednaErr;
    }
    conn->isInTransaction = SEDNA_NO_TRANSACTION;
    *errorcode = SEDNA_BULK_LOAD_FAILED;
    return 1;

SednaErr:
    *errorcode = SEDNA_ERROR;
    return 1;
}


/**
 * Helper to handle debug information messages from the server.
 * Just reads them and either connection debug handler or skips.
 * On error returns -1, 0 if handled successfully.
 */
static int
handle_debug_info(struct SednaConnection *conn) 
{
    while (conn->msg.instruction == se_DebugInfo)
    {
        if (conn->debug_handler)
        {
            int length;
            int debug_type;
            char debug_info[SE_SOCKET_MSG_BUF_SIZE];
            if (conn->msg.length <= 0) 
            {
                /* "Unknown message from server" */
                connectionFailure(conn, SE3008, NULL, NULL);
                return -1;
            }
            net_int2int(&debug_type, conn->msg.body);
            net_int2int(&length, conn->msg.body + 5);
            if (length <= 0)
            {
                /* "Unknown message from server" */
                connectionFailure(conn, SE3008, NULL, NULL);
                return -1;
            }
            memcpy(debug_info, conn->msg.body + 9, length);
            debug_info[length] = '\0';
            conn->debug_handler(debug_type, debug_info);
        }

        if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3007, "Connection was broken while executing statement", NULL);
            return -1;
        }
    }
    return 0;
}

/**
 * Helper to handle result of the query/update statement execution.
 * It iterates through messages which are returned from the server after
 * it receives se_Execute or se_ExecuteLong commands.
 */
static int
handle_execute_result(struct SednaConnection *conn)
{
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, "Connection was broken while executing statement", NULL);
        return SEDNA_ERROR;
    }

    if(handle_debug_info(conn) < 0) return SEDNA_ERROR;
    
    if (conn->msg.instruction == se_ErrorResponse)
    {
        setServerErrorMsg(conn, conn->msg);
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        return SEDNA_ERROR;
    }
    else if (conn->msg.instruction == se_QuerySucceeded)
    {
        int query_result;
        query_result = resultQueryHandler(conn);
        conn->first_next = 1;
        return query_result;
    }
    else if (conn->msg.instruction == se_QueryFailed)
    {
        setServerErrorMsg(conn, conn->msg);
        conn->in_query = 0;
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        return SEDNA_QUERY_FAILED;
    }
    else if (conn->msg.instruction == se_UpdateSucceeded)
    {
        conn->in_query = 0;
        if (conn->autocommit)
        {
            int comm_res = commit_handler(conn);
            if(comm_res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED)
                return SEDNA_UPDATE_FAILED;
        }
        return SEDNA_UPDATE_SUCCEEDED;
    }
    else if (conn->msg.instruction == se_UpdateFailed)
    {
        setServerErrorMsg(conn, conn->msg);
        conn->in_query = 0;
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        return SEDNA_UPDATE_FAILED;
    }
    else if (conn->msg.instruction == se_BulkLoadFileName)
    {
        while(conn->msg.instruction == se_BulkLoadFileName)
        {
            int status = 0;
            if ( bulkload(conn, &status) != 0 ) {
                if(handle_debug_info(conn) < 0) return SEDNA_ERROR;
                return status;
            }
            if (sp_recv_msg(conn->socket, &(conn->msg)) != 0) {
                connectionFailure(conn, SE3007, "Connection was broken while obtaining bulk load result", NULL);
                return SEDNA_ERROR;
            }
            if(handle_debug_info(conn) < 0) return SEDNA_ERROR;
        }

        if (conn->msg.instruction == se_ErrorResponse)
        {
            setServerErrorMsg(conn, conn->msg);
            conn->isInTransaction = SEDNA_NO_TRANSACTION;
            return SEDNA_ERROR;
        }
        else if ((conn->msg.instruction == se_BulkLoadSucceeded) || 
                 (conn->msg.instruction == se_UpdateSucceeded))
        {
            conn->in_query = 0;
            if (conn->autocommit)
            {
                int comm_res = commit_handler(conn);
                if(comm_res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED)
                    return SEDNA_BULK_LOAD_FAILED;
            }
            return SEDNA_BULK_LOAD_SUCCEEDED;
        }
        else if ((conn->msg.instruction == se_UpdateFailed) || 
                 (conn->msg.instruction == se_BulkLoadFailed))
        {
            setServerErrorMsg(conn, conn->msg);
            conn->in_query = 0;
            conn->isInTransaction = SEDNA_NO_TRANSACTION;
            return SEDNA_BULK_LOAD_FAILED;
        }
        else
        {
            /* Unknown message from server */
            setDriverErrorMsg(conn, SE3008, NULL);
            return SEDNA_ERROR;
        }
    } 
    /* Bulk Load from Stream */
    else if (conn->msg.instruction == se_BulkLoadFromStream)
    {
        conn->in_query = 0;
        return SEDNA_UPDATE_FAILED;
    }
    else /* Unknown message from server */
    {
        connectionFailure(conn, SE3008, NULL, NULL);
        return SEDNA_ERROR;
    }
    return SEDNA_ERROR;
}

/******************************************************************************
 * Driver Functions Implementation
 *****************************************************************************/

int SEconnect(struct SednaConnection *conn, const char *url, const char *db_name, const char *login, const char *password)
{
    char host[SE_HOSTNAMELENGTH + 1];
    int port = 5050;
    size_t db_name_len = 0, login_len = 0, password_len = 0, url_len = 0, host_len = 0;
    sp_int32 body_position = 0;
    int socket_optval = 1, socket_optsize = sizeof(int);

    db_name_len = strlen(db_name);
    login_len = strlen(login);
    password_len = strlen(password);
    url_len = strlen(url);

    clearLastError(conn);

    if (db_name_len > SE_MAX_DB_NAME_LENGTH)
    {
        connectionFailure(conn, SE3023, db_name, NULL);
        return SEDNA_OPEN_SESSION_FAILED;
    }
    if (login_len > SE_MAX_LOGIN_LENGTH)
    {
        connectionFailure(conn, SE3024, login, NULL);
        return SEDNA_OPEN_SESSION_FAILED;
    }
    if (password_len > SE_MAX_PASSWORD_LENGTH)
    {
        connectionFailure(conn, SE3025, password, NULL);
        return SEDNA_OPEN_SESSION_FAILED;
    }
    if (url_len > SE_HOSTNAMELENGTH)
    {
        connectionFailure(conn, SE3026, url, NULL);
        return SEDNA_OPEN_SESSION_FAILED;
    }

    conn->isConnectionOk = SEDNA_CONNECTION_CLOSED;
    conn->isInTransaction = SEDNA_NO_TRANSACTION;

    if (uSocketInit(NULL) != 0)
    {
        connectionFailure(conn, SE3016, NULL, NULL);  /* Can't initialize socket library.*/
        return SEDNA_OPEN_SESSION_FAILED;
    }

    conn->socket = usocket(AF_INET, SOCK_STREAM, 0, NULL);
    if (conn->socket == U_INVALID_SOCKET)
    {
        connectionFailure(conn, SE3001, NULL, NULL);  /* Failed to initialize a socket.*/
        release(conn);
        return SEDNA_OPEN_SESSION_FAILED;
    }

    if (usetsockopt(conn->socket, IPPROTO_TCP, TCP_NODELAY, (char *) &socket_optval, socket_optsize, NULL) == U_SOCKET_ERROR)
    {
        connectionFailure(conn, SE3027, NULL, NULL);  /* Failed to set socket option.*/
        release(conn);
        return SEDNA_OPEN_SESSION_FAILED;
    }

    if (strstr(url, ":") != NULL)
    {
        host_len = strcspn(url, ":");
        port = atoi(url + host_len + 1);
    }
    else
    {
        host_len = url_len;
    }

    if (_strnicmp(url, "localhost", host_len) == 0)
    {
        strcpy(host, "127.0.0.1");
    }
    else
    {
        memcpy(host, url, host_len);
        host[host_len] = '\0';
    }

    if (uconnect_tcp(conn->socket, port, host, NULL) != 0)
    {
        connectionFailure(conn, SE3003, url, NULL);  /* "Failed to connect to host specified"*/
        release(conn);
        return SEDNA_OPEN_SESSION_FAILED;
    }

    /* send a message for listener,*/
    /* 110 - StartUp*/
    conn->msg.instruction = se_StartUp;
    conn->msg.length = 0;
    if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3006, "Connection was broken while sending Start up mesage to server", NULL);
        release(conn);
        return SEDNA_OPEN_SESSION_FAILED;
    }
    /* read msg. 140 - SendSessionParameters*/
    /* send protocol version, login, dbname. SessionParameters - 120*/
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, "Connection was broken while recieve se_SendSessionParameters mesage from server", NULL);
        release(conn);
        return SEDNA_OPEN_SESSION_FAILED;
    }
    if (conn->msg.instruction == se_ErrorResponse)
    {
        connectionFailure(conn, 0, NULL, &(conn->msg));
        release(conn);
        return SEDNA_OPEN_SESSION_FAILED;
    }
    else if (conn->msg.instruction == se_SendSessionParameters)
    {
        conn->msg.instruction = se_SessionParameters;   /*SessionParameters*/
        /*body contains:*/
        /*major protocol version*/
        /*minor protocol version*/
        /* login string*/
        /*dbname string                  */
        conn->msg.length = 2 + 5 + (sp_int32)login_len + 5 + (sp_int32)db_name_len;

        /* writing protocol version 3.0*/
        conn->msg.body[0] = SE_CURRENT_SOCKET_PROTOCOL_VERSION_MAJOR;
        conn->msg.body[1] = SE_CURRENT_SOCKET_PROTOCOL_VERSION_MINOR;

        /* writing login */
        conn->msg.body[2] = 0;  /* format code*/
        int2net_int((int32_t)login_len, conn->msg.body + 3);
        memcpy(conn->msg.body + 7, login, login_len);
        body_position += 7 + (sp_int32)strlen(login);

        /* writing db_name       */
        conn->msg.body[body_position] = 0;      /* format code*/
        int2net_int((int32_t)db_name_len, conn->msg.body + body_position + 1);
        body_position += 5;
        memcpy(conn->msg.body + body_position, db_name, db_name_len);

        if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3006, "Connection was broken while sending authorization data to the server", NULL);
            release(conn);
            return SEDNA_OPEN_SESSION_FAILED;
        }
    }
    else
        goto UnknownMsg;

    /* read - error or SendAuthenticationParameters - 150*/
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, "Connection was broken while recieving authorization request from the server", NULL);
        release(conn);
        return SEDNA_OPEN_SESSION_FAILED;
    }

    if (conn->msg.instruction == se_ErrorResponse)
    {
        connectionFailure(conn, 0, NULL, &(conn->msg));
        release(conn);
        return SEDNA_OPEN_SESSION_FAILED;
    }
    else if (conn->msg.instruction == se_SendAuthParameters)
    {
        /* send authentication paramaters - password. 130 - AuthenticationParameters*/
        conn->msg.instruction = se_AuthenticationParameters;    /*AuthenticationParameters*/
        conn->msg.length = 5 + (sp_int32)password_len;

        /* writing password      */
        conn->msg.body[0] = 0;  /* format code*/
        int2net_int((int32_t)password_len, conn->msg.body + 1);
        memcpy(conn->msg.body + 5, password, password_len);

        if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3006, "Connection was broken while sending authorization data to the server", NULL);
            release(conn);
            return SEDNA_OPEN_SESSION_FAILED;
        }
    }
    else
        goto UnknownMsg;

    /* read AuthenticationOk - 160 or AuthenticationFailed - 170.*/
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, "Connection was broken while recieving authorization result from the server", NULL);
        release(conn);
        return SEDNA_OPEN_SESSION_FAILED;
    }
    if (conn->msg.instruction == se_ErrorResponse)
    {
        connectionFailure(conn, 0, NULL, &(conn->msg));
        release(conn);
        return SEDNA_OPEN_SESSION_FAILED;
    }
    else if (conn->msg.instruction == se_AuthenticationFailed)
    {
        connectionFailure(conn, SE3053, NULL, NULL);  /* "Authentication failed"*/
        release(conn);
        return SEDNA_AUTHENTICATION_FAILED;
    }
    else if (conn->msg.instruction == se_AuthenticationOK)      /* AuthenticationOk*/
    {
        strcpy(conn->url, url); 
        strcpy(conn->db_name, db_name); 
        strcpy(conn->login, login);     
        strcpy(conn->password, password);       /*  Need to initialize every field */
        strcpy(conn->query_time, "not available");      /*  No time available      */
        conn->socket_keeps_data = 0;    
        conn->result_end = 0;   
        conn->in_query = 0;     
        conn->local_data_length = 0;
        conn->local_data_offset = 0;
        conn->cbl.bulk_load_started = 0;
        if(strcmp(conn->session_directory, "") == 0) /* Session directory has not been set yet */
        {
            if (uGetCurrentWorkingDirectory(conn->session_directory, SE_MAX_DIR_LENGTH, NULL) == NULL)
            {
                connectionFailure(conn, SE4602, conn->session_directory, NULL);
                release(conn);
                return SEDNA_OPEN_SESSION_FAILED;
            }
        }
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        conn->isConnectionOk = SEDNA_CONNECTION_OK;

        return SEDNA_SESSION_OPEN;
    }

UnknownMsg:
    connectionFailure(conn, SE3008, "Unknown message from server got while trying ot open a session", NULL);      /* "Unknown message from server"*/
    release(conn);
    return SEDNA_OPEN_SESSION_FAILED;

}

int SEclose(struct SednaConnection *conn)
{
    clearLastError(conn);

    if (conn->isConnectionOk == SEDNA_CONNECTION_CLOSED)
        return SEDNA_SESSION_CLOSED;
    if (conn->isConnectionOk == SEDNA_CONNECTION_FAILED)
    {
        release(conn);
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        conn->isConnectionOk = SEDNA_CONNECTION_CLOSED;
        return SEDNA_SESSION_CLOSED;
    }

    /* clean socket*/
    if (cleanSocket(conn) == SEDNA_ERROR)
    {
        release(conn);
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        conn->isConnectionOk = SEDNA_CONNECTION_CLOSED;
        return SEDNA_ERROR;
    }

    if ((conn->autocommit) && (conn->isInTransaction == SEDNA_TRANSACTION_ACTIVE))
    {
        int comm_res = commit_handler(conn);
        if(comm_res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED)
        {
            release(conn);
            return SEDNA_CLOSE_SESSION_FAILED;        
        }
    }

    /* send 500 - CloseConnection*/
    conn->msg.instruction = se_CloseConnection;
    conn->msg.length = 0;
    if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3006, "Connection was broken while trying to close session", NULL);
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        conn->isConnectionOk = SEDNA_CONNECTION_CLOSED;
        release(conn);
        return SEDNA_ERROR;
    }

    /* read 100 or 510 - CloseConnectionOk or 520 - TransactionRollbackBeforeClose*/
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, "Connection was broken while trying to close session", NULL);
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        conn->isConnectionOk = SEDNA_CONNECTION_CLOSED;
        release(conn);
        return SEDNA_ERROR;
    }

    release(conn);
    conn->isConnectionOk = SEDNA_CONNECTION_CLOSED;

    if (conn->msg.instruction == se_ErrorResponse)
    {
        setServerErrorMsg(conn, conn->msg);
        return SEDNA_CLOSE_SESSION_FAILED;
    }
    else if (conn->msg.instruction == se_TransactionRollbackBeforeClose)        /*TransactionRollbackBeforeClose*/
    {
        setServerErrorMsg(conn, conn->msg);
        return SEDNA_SESSION_CLOSED;
    }
    else if (conn->msg.instruction == se_CloseConnectionOk)     /*CloseConnectionOk*/
    {
        return SEDNA_SESSION_CLOSED;
    }
    else
    {
        connectionFailure(conn, SE3008, "Unknown message got while trying to close session", NULL);            /* "Unknown message from server" */
        return SEDNA_CLOSE_SESSION_FAILED;
    }
}

int SEbegin(struct SednaConnection *conn)
{
    if (conn->isConnectionOk == SEDNA_CONNECTION_CLOSED)
    {
        setDriverErrorMsg(conn, SE3028, NULL);        /* "Connection with server is closed or have not been established yet." */
        return SEDNA_ERROR;
    }
    if (conn->isConnectionOk != SEDNA_CONNECTION_OK)
        return SEDNA_ERROR;

    clearLastError(conn);

    if (conn->autocommit)
    {
        setDriverErrorMsg(conn, SE3029, NULL);        /* "This function call is prohibited as the connection is in the autocommit mode." */
        return SEDNA_ERROR;
    }

    /* clean socket*/
    if (cleanSocket(conn) == SEDNA_ERROR)
        return SEDNA_ERROR;

    return begin_handler(conn);
}

int SErollback(struct SednaConnection *conn)
{
    if (conn->isConnectionOk == SEDNA_CONNECTION_CLOSED)
    {
        setDriverErrorMsg(conn, SE3028, NULL);        /* "Connection with server is closed or have not been established yet." */
        return SEDNA_ERROR;
    }
    if (conn->isConnectionOk != SEDNA_CONNECTION_OK)
        return SEDNA_ERROR;

    if (conn->autocommit)
    {
        setDriverErrorMsg(conn, SE3029, NULL);        /* "This function call is prohibited as the connection is in the autocommit mode." */
        return SEDNA_ERROR;
    }

    clearLastError(conn);

    /* clean socket*/
    if (cleanSocket(conn) == SEDNA_ERROR)
        return SEDNA_ERROR;

    return rollback_handler(conn);
}

int SEcommit(struct SednaConnection *conn)
{
    if (conn->isConnectionOk == SEDNA_CONNECTION_CLOSED)
    {
        setDriverErrorMsg(conn, SE3028, NULL);        /* "Connection with server is closed or have not been established yet." */
        return SEDNA_ERROR;
    }
    if (conn->isConnectionOk != SEDNA_CONNECTION_OK)
        return SEDNA_ERROR;

    if (conn->autocommit)
    {
        setDriverErrorMsg(conn, SE3029, NULL);        /* "This function call is prohibited as the connection is in the autocommit mode." */
        return SEDNA_ERROR;
    }

    clearLastError(conn);

    /* clean socket*/
    if (cleanSocket(conn) == SEDNA_ERROR)
        return SEDNA_ERROR;
    conn->in_query = 0;

    return commit_handler(conn);
}

int SEexecuteLong(struct SednaConnection *conn, const char* query_file_path)
{
    size_t read = 0;
    FILE* query_file;

    if (conn->isConnectionOk == SEDNA_CONNECTION_CLOSED)
    {
        setDriverErrorMsg(conn, SE3028, NULL);        /* "Connection with server is closed or have not been established yet." */
        return SEDNA_ERROR;
    }
    if (conn->isConnectionOk != SEDNA_CONNECTION_OK)
        return SEDNA_ERROR;

    clearLastError(conn);

    /* clean socket*/
    if (cleanSocket(conn) == SEDNA_ERROR)
        return SEDNA_ERROR;

    /* if autocommit is on - begin transaction implicitly */
    if ((conn->autocommit) && (conn->isInTransaction == SEDNA_NO_TRANSACTION))
    {
        int begin_res = begin_handler(conn);
        if (begin_res != SEDNA_BEGIN_TRANSACTION_SUCCEEDED)
            return SEDNA_ERROR;
    }

    if(NULL == query_file_path || (query_file = fopen(query_file_path, "rb")) == NULL)
    {
        setDriverErrorMsg(conn, SE3081, NULL);        /* "Can't open file with long query to execute" */
        return SEDNA_ERROR;
    }

    while ((read < SE_SOCKET_MSG_BUF_SIZE - 6) && (!feof(query_file)))
    {
        read += fread(conn->msg.body + 6 + read, sizeof(char), SE_SOCKET_MSG_BUF_SIZE - 6 - read, query_file);
    }
    if (feof(query_file))
    {
        conn->msg.instruction = se_Execute;
        conn->msg.body[0] = 0;  /* result format code*/
        conn->msg.body[1] = 0;  /* string format*/
        int2net_int((int32_t)read, conn->msg.body + 2);
        conn->msg.length = (sp_int32)read + 6;    /* body containes: result format (sxml=1 or xml=0) - 1 byte)*/
        /* string format - 1 byte;*/
        /* string length - 4 bytes*/
        /* string*/
        if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3006, "Connection was broken while sending long query to the server", NULL);
            return SEDNA_ERROR;
        }
    }
    else                        /* Long query - pass to server in parts*/
    {
        while (1)
        {
            /*send 301 - ExecuteLong*/
            conn->msg.instruction = se_ExecuteLong;
            conn->msg.body[0] = 0;      /* result format code*/
            conn->msg.body[1] = 0;      /* string format*/
            int2net_int((int32_t)read, conn->msg.body + 2);
            conn->msg.length = (sp_int32)read + 6;        /* body containes: result format (sxml=1 or xml=0) - 1 byte)*/
            /* string format - 1 byte;*/
            /* string length - 4 bytes*/
            /* string*/
            if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
            {
                connectionFailure(conn, SE3006, "Connection was broken while sending long query to the server", NULL);
                return SEDNA_ERROR;
            }
            if (feof(query_file))
                break;
            read = fread(conn->msg.body + 6, sizeof(char), SE_SOCKET_MSG_BUF_SIZE - 6, query_file);
        }
        /*send 302 - LongQueryEnd*/
        conn->msg.instruction = se_LongQueryEnd;
        conn->msg.length = 0;

        if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3006, "Connection was broken while sending long query to the server", NULL);
            return SEDNA_ERROR;
        }
    }

    fclose(query_file);

    return handle_execute_result(conn);
}

int SEexecute(struct SednaConnection *conn, const char *query)
{
    size_t query_length = 0, i = 0;
    sp_int32 query_portion_size = 0;

    if (conn->isConnectionOk == SEDNA_CONNECTION_CLOSED)
    {
        setDriverErrorMsg(conn, SE3028, NULL);        /* "Connection with server is closed or have not been established yet." */
        return SEDNA_ERROR;
    }
    if (conn->isConnectionOk != SEDNA_CONNECTION_OK)
        return SEDNA_ERROR;

    clearLastError(conn);

    /* clean socket*/
    if (cleanSocket(conn) == SEDNA_ERROR)
        return SEDNA_ERROR;

    /* if autocommit is on - begin transaction implicitly */
    if ((conn->autocommit) && (conn->isInTransaction == SEDNA_NO_TRANSACTION))
    {
        int begin_res = begin_handler(conn);
        if (begin_res != SEDNA_BEGIN_TRANSACTION_SUCCEEDED)
            return SEDNA_ERROR;
    }

    query_length = strlen(query);
    if (query_length > SE_SOCKET_MSG_BUF_SIZE - 6)
    {
        while (i < query_length)
        {
            /*send 301 - ExecuteLong*/
            conn->msg.instruction = se_ExecuteLong;
            conn->msg.body[0] = 0;      /* result format code*/
            conn->msg.body[1] = 0;      /* string format*/
            query_portion_size = ((query_length - i) >= (SE_SOCKET_MSG_BUF_SIZE - 6)) ? (SE_SOCKET_MSG_BUF_SIZE - 6) : (sp_int32)(query_length - i);
            int2net_int(query_portion_size, conn->msg.body + 2);

            memcpy(conn->msg.body + 6, query + i, query_portion_size);

            conn->msg.length = query_portion_size + 6;  /* body containes: result format (sxml=1 or xml=0) - 1 byte)*/
            /* string format - 1 byte;*/
            /* string length - 4 bytes*/
            /* string*/
            if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
            {
                connectionFailure(conn, SE3006, "Connection was broken while sending query to the server", NULL);
                return SEDNA_ERROR;
            }

            i += query_portion_size;
        }
        /*send 302 - LongQueryEnd*/
        conn->msg.instruction = se_LongQueryEnd;
        conn->msg.length = 0;

        if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3006, "Connection was broken while sending query to the server", NULL);
            return SEDNA_ERROR;
        }
    }
    else
    {
        /*send 300 - ExecuteQuery*/
        conn->msg.instruction = se_Execute;
        conn->msg.length = (sp_int32)query_length + 6;    /* body containes: result format (sxml=1 or xml=0) - 1 byte)*/
        /* string format - 1 byte;*/
        /* string length - 4 bytes*/
        /* string*/
        conn->msg.body[0] = 0;  /* result format code*/
        conn->msg.body[1] = 0;  /* string format*/
        int2net_int((int32_t)query_length, conn->msg.body + 2);

        memcpy(conn->msg.body + 6, query, query_length);
        if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3006, "Connection was broken while sending query to the server", NULL);
            return SEDNA_ERROR;
        }
    }
    return handle_execute_result(conn);
}


int SEnext(struct SednaConnection *conn)
{
    int res = 0;

    if (conn->isConnectionOk == SEDNA_CONNECTION_CLOSED)
    {
        setDriverErrorMsg(conn, SE3028, NULL);        /* "Connection with server is closed or have not been established yet." */
        return SEDNA_ERROR;
    }
    if (conn->isConnectionOk != SEDNA_CONNECTION_OK)
        return SEDNA_ERROR;

    clearLastError(conn);

    if (!conn->in_query)
        return SEDNA_NO_ITEM;

    if (conn->result_end)
    {
        return SEDNA_RESULT_END;
    }

    if (conn->first_next)
    {
        conn->first_next = 0;
        return SEDNA_NEXT_ITEM_SUCCEEDED;
    }

    /* clean socket*/
    if (cleanSocket(conn) == SEDNA_ERROR)
        return SEDNA_ERROR;

    conn->first_next = 0;

    /*send GetNextItem - 310*/
    conn->msg.instruction = se_GetNextItem;
    conn->msg.length = 0;

    if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3006, "Connection was broken while sending Next command to the server", NULL);
        return SEDNA_ERROR;
    }

    res = resultQueryHandler(conn);
    if ((res == SEDNA_QUERY_FAILED) || (res == SEDNA_ERROR))
        return SEDNA_NEXT_ITEM_FAILED;
    else if ((res == SEDNA_QUERY_SUCCEEDED) && (conn->result_end))
        return SEDNA_RESULT_END;

    return SEDNA_NEXT_ITEM_SUCCEEDED;
}

int SEgetData(struct SednaConnection *conn, char *buf, int bytes_to_read)
{
    int buf_position = 0;
    int content_length = 0;
    char* content_offset = NULL;

    if (conn->isConnectionOk == SEDNA_CONNECTION_CLOSED)
    {
        setDriverErrorMsg(conn, SE3028, NULL);        /* "Connection with server is closed or have not been established yet." */
        return SEDNA_ERROR;
    }
    if (conn->isConnectionOk != SEDNA_CONNECTION_OK)
        return SEDNA_ERROR;

    if ((!conn->in_query) || (conn->result_end))
        return 0;

    clearLastError(conn);

    if ((bytes_to_read < 0) || (buf == NULL))
    {
        setDriverErrorMsg(conn, SE3022, NULL);   /* Invalid argument */
        conn->result_end = 1;                    /* Tell result is finished */
        conn->socket_keeps_data = 0;             /* Tell there is no data in socket */
        return SEDNA_ERROR;
    }
    while (bytes_to_read > 0)
    {
        /*there is enough data strored locally in local buf*/
        if (bytes_to_read <= (conn->local_data_length - conn->local_data_offset))
        {
            memcpy(buf, conn->local_data_buf + conn->local_data_offset, bytes_to_read);
            conn->local_data_offset += bytes_to_read;
            return bytes_to_read;
        }
        /*local stored data is not enough - need to recv from server*/
        else
        {
            memcpy(buf + buf_position, conn->local_data_buf + conn->local_data_offset, conn->local_data_length - conn->local_data_offset);
            buf_position += conn->local_data_length - conn->local_data_offset;
            bytes_to_read -= conn->local_data_length - conn->local_data_offset;
            conn->local_data_length = 0;
            conn->local_data_offset = 0;

            if (!conn->socket_keeps_data)
            {
                return buf_position;
            }

            if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
            {
                connectionFailure(conn, SE3007, "Connection was broken while getting result data from the server", NULL);
                return SEDNA_ERROR;
            }
            if (conn->msg.instruction == se_ErrorResponse)
            {
                setServerErrorMsg(conn, conn->msg);
                conn->isInTransaction = SEDNA_NO_TRANSACTION;
                conn->result_end = 1;   /* tell result is finished*/
                conn->socket_keeps_data = 0;    /* tell there is no data in socket*/
                return SEDNA_ERROR;
            }
            if (conn->msg.instruction == se_ItemPart)      /* ItemPart */
            {
                content_length = conn->msg.length - 5;
                content_offset = conn->msg.body + 5;

                if (content_length > bytes_to_read)
                {
                    memcpy(buf + buf_position, content_offset, bytes_to_read);
                    buf_position += bytes_to_read;
                    memcpy(conn->local_data_buf, content_offset + bytes_to_read, 
                        content_length - bytes_to_read);
                    conn->local_data_length = content_length - bytes_to_read;
                    return buf_position;
                }
                else
                {
                    memcpy(buf + buf_position, content_offset, content_length);
                    buf_position  += content_length;
                    bytes_to_read -= content_length;
                }
            }
            else if (conn->msg.instruction == se_ItemEnd)       /*ItemEnd*/
            {
                conn->socket_keeps_data = 0;    /* tell there is no data in socket*/
                return buf_position;
            }
            else if (conn->msg.instruction == se_ResultEnd)     /*ResultEnd*/
            {
                conn->result_end = 1;   /* tell result is finished*/
                conn->socket_keeps_data = 0;    /* tell there is no data in socket*/
                if (conn->autocommit)
                {
                    int comm_res = commit_handler(conn);
                    if(comm_res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED)
                        return SEDNA_ERROR;
                }

                return buf_position;
            }
            else
            {
                connectionFailure(conn, SE3008, "Unknown message got while getting result data from the server", NULL);            /* "Unknown message from server" */
                conn->result_end = 1;   /* tell result is finished*/
                conn->socket_keeps_data = 0;    /* tell there is no data in socket*/
                conn->isInTransaction = SEDNA_NO_TRANSACTION;
                conn->isConnectionOk = SEDNA_CONNECTION_FAILED;
                return SEDNA_ERROR;
            }
        }                       /* else */

    }                           /* while */
    return buf_position;
}

int SEloadData(struct SednaConnection *conn, const char *buf, int bytes_to_load, const char *doc_name, const char *col_name)
{
    int bl_portion_size = 0, i = 0;

    if (conn->isConnectionOk == SEDNA_CONNECTION_CLOSED)
    {
        setDriverErrorMsg(conn, SE3028, NULL);        /* "Connection with server is closed or have not been established yet." */
        return SEDNA_ERROR;
    }
    if (conn->isConnectionOk != SEDNA_CONNECTION_OK)
        return SEDNA_ERROR;

    clearLastError(conn);

    if ((bytes_to_load <= 0) || (buf == NULL) || (doc_name == NULL) || (strlen(doc_name) == 0) || ((col_name != NULL) && (strlen(col_name) == 0)))
    {
        setDriverErrorMsg(conn, SE3022, NULL);        /* "Invalid argument."*/
        conn->result_end = 1;                   /* tell result is finished*/
        conn->socket_keeps_data = 0;    /* tell there is no data in socket*/
        setBulkLoadFinished(conn);
        return SEDNA_ERROR;
    }
    /* clean socket*/
    if (cleanSocket(conn) == SEDNA_ERROR)
        return SEDNA_ERROR;

    /* if autocommit is on - begin transaction implicitly */
    if ((conn->autocommit) && (conn->isInTransaction == SEDNA_NO_TRANSACTION))
    {
        int begin_res = begin_handler(conn);
        if (begin_res != SEDNA_BEGIN_TRANSACTION_SUCCEEDED)
            return SEDNA_ERROR;
    }

    /* if bulk load of exactly this document is not started yet */
    if (!isBulkLoadStarted(conn))
    {
        char *query_str = NULL;
        size_t query_size = 0;

        /*send 300 - ExecuteQuery*/
        conn->msg.instruction = 300;
        conn->msg.body[0] = 0;  /* result format code*/
        conn->msg.body[1] = 0;  /* string format*/

        query_str = conn->msg.body + 6;
        if(conn->boundary_space_preserve)
        {
            strcpy(query_str, "declare boundary-space preserve;\n");
            strcat(query_str, "LOAD STDIN \"");
        }
        else
            strcpy(query_str, "LOAD STDIN \"");

        strcat(query_str, doc_name);
        strcat(query_str, "\"");
        if (col_name != NULL)
        {
            strcat(query_str, " \"");
            strcat(query_str, col_name);
            strcat(query_str, "\"");
        }
        query_size = strlen(query_str);

        int2net_int((int32_t)query_size, conn->msg.body + 2);
        conn->msg.length = (sp_int32)query_size + 6;      /* body containes: result format (sxml=1 or xml=0) - 1 byte)*/
        /* string format - 1 byte;*/
        /* string length - 4 bytes*/
        /* string*/

        if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3006, "Connection was broken while loading data (bulk load) the server", NULL);
            return SEDNA_ERROR;
        }
        if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3007, "Connection was broken while loading data (bulk load) the server", NULL);
            return SEDNA_ERROR;
        }

        if (conn->msg.instruction == se_ErrorResponse)
        {
            setServerErrorMsg(conn, conn->msg);
            conn->isInTransaction = SEDNA_NO_TRANSACTION;
            return SEDNA_ERROR;
        }
        else if (conn->msg.instruction != se_BulkLoadFromStream)        /*BulkLoadFromStream*/
        {
            connectionFailure(conn, SE3008, NULL, NULL);            /* "Unknown message from server" */
            return SEDNA_ERROR;
        }

        setBulkLoadStarted(conn, doc_name, col_name);
    }     /* bulk load started*/

    /* if another document is currently loading */
    if(!isBulkLoadOf(conn, doc_name, col_name))
    {
        conn->msg.instruction = se_BulkLoadError;     /*BulkLoadError*/
        int2net_int(SE4616, conn->msg.body);
        conn->msg.length = 4;

        if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3006, "Connection was broken while passing bulk load error to the server", NULL);
            return SEDNA_ERROR;
        }
        if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3007, "Connection was broken while passing bulk load error to the server", NULL);
            return SEDNA_ERROR;
        }
        setBulkLoadFinished(conn);
        setDriverErrorMsg(conn, SE4616, NULL); /* Can't load a document because the session is loading another document. Finish current loading before beginning a new one. */
        return SEDNA_ERROR;
    }

    i = 0;
    while (i < bytes_to_load)
    {
        conn->msg.instruction = se_BulkLoadPortion;     /*BulkLoadPortion*/
        conn->msg.length = 0;
        bl_portion_size = ((bytes_to_load - i) >= (SE_SOCKET_MSG_BUF_SIZE - 5)) ? (SE_SOCKET_MSG_BUF_SIZE - 5) : (bytes_to_load - i);
        int2net_int(bl_portion_size, conn->msg.body + 1);

        memcpy(conn->msg.body + 5, buf + i, bl_portion_size);
        conn->msg.length = bl_portion_size + 5; /* body containes: result format (sxml=1 or xml=0) - 1 byte)*/
        /* string format - 1 byte;*/
        /* string length - 4 bytes*/
        /* string*/
        if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3006, "Connection was broken while passing a data chunk to the server", NULL);
            return SEDNA_ERROR;
        }

        i += bl_portion_size;
    }
    return SEDNA_DATA_CHUNK_LOADED;
}

int SEendLoadData(struct SednaConnection *conn)
{
    if (conn->isConnectionOk == SEDNA_CONNECTION_CLOSED)
    {
        /* "Connection with server is closed or have not been established yet." */
        setDriverErrorMsg(conn, SE3028, NULL);
        return SEDNA_ERROR;
    }
    if (conn->isConnectionOk != SEDNA_CONNECTION_OK)
        return SEDNA_ERROR;

    clearLastError(conn);

    /* clean socket*/
    if (cleanSocket(conn) == SEDNA_ERROR)
        return SEDNA_ERROR;

    /*BulkLoadEnd*/
    conn->msg.instruction = se_BulkLoadEnd;
    conn->msg.length = 0;

    if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3006, "Connection was broken while passing bulk load ending message to the server", NULL);
        return SEDNA_ERROR;
    }

    setBulkLoadFinished(conn);

    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, "Connection was broken while passing bulk load ending message to the server", NULL);
        return SEDNA_ERROR;
    }

    if (conn->msg.instruction == se_ErrorResponse)
    {
        setServerErrorMsg(conn, conn->msg);
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        return SEDNA_ERROR;
    }
    else if ((conn->msg.instruction == se_BulkLoadSucceeded) || (conn->msg.instruction == se_UpdateSucceeded))
    {
        /*BulkLoadSucceeded*/
        conn->in_query = 0;
        if (conn->autocommit)
        {
            int comm_res = commit_handler(conn);
            if(comm_res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED)
                return SEDNA_BULK_LOAD_FAILED;
        }
        return SEDNA_BULK_LOAD_SUCCEEDED;
    }
    else if ((conn->msg.instruction == se_BulkLoadFailed) || (conn->msg.instruction == se_UpdateFailed))
    {
        /*BulkLoadFailed*/
        conn->in_query = 0;
        setServerErrorMsg(conn, conn->msg);
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        return SEDNA_BULK_LOAD_FAILED;
    }
    else
    {
        /* "Unknown message from server" */
        connectionFailure(conn, SE3008, "Unknown message got while passing bulk load ending message to the server", NULL);
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        return SEDNA_BULK_LOAD_FAILED;
    }
}

int SEgetLastErrorCode(struct SednaConnection *conn)
{
    return conn->last_error;
}

const char *SEgetLastErrorMsg(struct SednaConnection *conn)
{
    if (conn->last_error != SEDNA_OPERATION_SUCCEEDED)
        return conn->last_error_msg;
    else
        return "";
}

int SEconnectionStatus(struct SednaConnection *conn)
{
    return conn->isConnectionOk;
}

int SEtransactionStatus(struct SednaConnection *conn)
{
    return conn->isInTransaction;
}

const char *SEshowTime(struct SednaConnection *conn)
{
    if (conn->isConnectionOk == SEDNA_CONNECTION_CLOSED)
    {
        setDriverErrorMsg(conn, SE3028, NULL);        /* "Connection with server is closed or have not been established yet." */
        strcpy(conn->query_time, "not available");
        return conn->query_time;
    }
    if (conn->isConnectionOk != SEDNA_CONNECTION_OK)
    {
        strcpy(conn->query_time, "not available");
        return conn->query_time;
    }

    /* clean socket*/
    if (cleanSocket(conn) == SEDNA_ERROR)
    {
        strcpy(conn->query_time, "not available");
        return conn->query_time;
    }

    conn->msg.instruction = se_ShowTime;        /*ShowTime*/
    conn->msg.length = 0;

    clearLastError(conn);

    if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3006, "Connection was broken while obtaining execution time from the server", NULL);
        strcpy(conn->query_time, "not available");
        return conn->query_time;
    }

    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3006, "Connection was broken while obtaining execution time from the server", NULL);
        strcpy(conn->query_time, "not available");
        return conn->query_time;
    }

    if (conn->msg.instruction == se_LastQueryTime)      /*LastQueryTime*/
    {
        strncpy(conn->query_time, conn->msg.body + 5, conn->msg.length - 5);
        conn->query_time[conn->msg.length - 5] = '\0';
        return conn->query_time;
    }
    else
    {
        strcpy(conn->query_time, "not available");
        return conn->query_time;
    }
}

int SEsetConnectionAttr(struct SednaConnection *conn, enum SEattr attr, const void* attrValue, int attrValueLength)
{
    int *value;

    clearLastError(conn);

    switch (attr){
        case SEDNA_ATTR_AUTOCOMMIT:
            value = (int*) attrValue;
            if ((*value != SEDNA_AUTOCOMMIT_OFF) && (*value != SEDNA_AUTOCOMMIT_ON))
            {
                setDriverErrorMsg(conn, SE3022, NULL);        /* "Invalid argument."*/
                return SEDNA_ERROR;
            }
            conn->autocommit = (*value == SEDNA_AUTOCOMMIT_ON) ? 1: 0;
            if ((*value == SEDNA_AUTOCOMMIT_ON) && (conn->isInTransaction == SEDNA_TRANSACTION_ACTIVE))
            {
                int comm_res = commit_handler(conn);
                if(comm_res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED)
                    return SEDNA_ERROR;
            }
            return SEDNA_SET_ATTRIBUTE_SUCCEEDED;

        case SEDNA_ATTR_SESSION_DIRECTORY:
            if (attrValueLength > SE_MAX_DIR_LENGTH)
            {
                setDriverErrorMsg(conn, SE3022, NULL);        /* "Invalid argument."*/
                return SEDNA_ERROR;
            }
            strncpy(conn->session_directory, (const char *)attrValue, attrValueLength);
            conn->session_directory[attrValueLength] = '\0';
            return SEDNA_SET_ATTRIBUTE_SUCCEEDED;

        case SEDNA_ATTR_DEBUG:
            conn->msg.instruction = se_SetSessionOptions;    /*se_SetSessionOptions*/
            conn->msg.length = 9;
            value = (int*) attrValue;
            int2net_int(*value, conn->msg.body); //option type
            conn->msg.body[4] = 0;
            int2net_int(0, conn->msg.body+5); //length of the option value string = 0
            if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
            {
                connectionFailure(conn, SE3006, "Connection was broken while setting session option on the server", NULL);
                return SEDNA_ERROR;
            }
            if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
            {
                connectionFailure(conn, SE3007, "Connection was broken while setting session option on the server", NULL);
                return SEDNA_ERROR;
            }
            if (conn->msg.instruction == se_SetSessionOptionsOk)
                return SEDNA_SET_ATTRIBUTE_SUCCEEDED;
            else if (conn->msg.instruction == se_ErrorResponse)
            {
                setServerErrorMsg(conn, conn->msg);
                conn->isInTransaction = SEDNA_NO_TRANSACTION;
                return SEDNA_ERROR;
            }
            else
            {
                connectionFailure(conn, SE3008, "Unknown message got while setting session option on the server", NULL);            /* "Unknown message from server" */
                conn->isInTransaction = SEDNA_NO_TRANSACTION;
                return SEDNA_ERROR;
            }
        case SEDNA_ATTR_BOUNDARY_SPACE_PRESERVE_WHILE_LOAD:
            value = (int*) attrValue;
            if ((*value != SEDNA_BOUNDARY_SPACE_PRESERVE_OFF) && (*value != SEDNA_BOUNDARY_SPACE_PRESERVE_ON))
            {
                setDriverErrorMsg(conn, SE3022, NULL);        /* "Invalid argument."*/
                return SEDNA_ERROR;
            }
            conn->boundary_space_preserve = (*value == SEDNA_BOUNDARY_SPACE_PRESERVE_ON) ? 1: 0;
            return SEDNA_SET_ATTRIBUTE_SUCCEEDED;

        case SEDNA_ATTR_CONCURRENCY_TYPE:
            value = (int*) attrValue;
            if ((*value != SEDNA_READONLY_TRANSACTION) && (*value != SEDNA_UPDATE_TRANSACTION))
            {
                setDriverErrorMsg(conn, SE3022, NULL);        /* "Invalid argument."*/
                return SEDNA_ERROR;
            }
            // do force commit of existing transaction
            if (conn->isInTransaction == SEDNA_TRANSACTION_ACTIVE)
            {
                int comm_res = commit_handler(conn);
                if(comm_res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED)
                    return SEDNA_ERROR;
            }
            conn->msg.instruction = se_SetSessionOptions;    /*se_SetSessionOptions*/
            conn->msg.length = 9;
            int2net_int(*value, conn->msg.body); //option type
            conn->msg.body[4] = 0;
            int2net_int(0, conn->msg.body+5); //length of the option value string = 0
            if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
            {
                connectionFailure(conn, SE3006, "Connection was broken while setting session option on the server", NULL);
                return SEDNA_ERROR;
            }
            if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
            {
                connectionFailure(conn, SE3007, "Connection was broken while setting session option on the server", NULL);
                return SEDNA_ERROR;
            }
            if (conn->msg.instruction == se_SetSessionOptionsOk)
                return SEDNA_SET_ATTRIBUTE_SUCCEEDED;
            else if (conn->msg.instruction == se_ErrorResponse)
            {
                setServerErrorMsg(conn, conn->msg);
                conn->isInTransaction = SEDNA_NO_TRANSACTION;
                return SEDNA_ERROR;
            }
            else
            {
                connectionFailure(conn, SE3008, "Unknown message got while setting session option on the server", NULL);            /* "Unknown message from server" */
                conn->isInTransaction = SEDNA_NO_TRANSACTION;
                return SEDNA_ERROR;
            }

        case SEDNA_ATTR_QUERY_EXEC_TIMEOUT:
            value = (int*) attrValue;
            if (*value < 0)
            {
                setDriverErrorMsg(conn, SE3022, "Timeout value must be >= 0");        /* "Invalid argument."*/
                return SEDNA_ERROR;
            }
            conn->msg.instruction = se_SetSessionOptions;    /*se_SetSessionOptions*/
            conn->msg.length = 13;
            int2net_int(SEDNA_QUERY_EXEC_TIMEOUT, conn->msg.body); //option type
            conn->msg.body[4] = 0;
            int2net_int(4, conn->msg.body+5); //length of value - here sizeof int = 4
            int2net_int(*value, conn->msg.body+9); //value of attribute - here int
            if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
            {
                connectionFailure(conn, SE3006, "Connection was broken while setting session option on the server", NULL);
                return SEDNA_ERROR;
            }
            if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
            {
                connectionFailure(conn, SE3007, "Connection was broken while setting session option on the server", NULL);
                return SEDNA_ERROR;
            }
            if (conn->msg.instruction == se_SetSessionOptionsOk)
                return SEDNA_SET_ATTRIBUTE_SUCCEEDED;
            else if (conn->msg.instruction == se_ErrorResponse)
            {
                setServerErrorMsg(conn, conn->msg);
                conn->isInTransaction = SEDNA_NO_TRANSACTION;
                return SEDNA_ERROR;
            }
            else
            {
                connectionFailure(conn, SE3008, "Unknown message got while setting session option on the server", NULL);            /* "Unknown message from server" */
                conn->isInTransaction = SEDNA_NO_TRANSACTION;
                return SEDNA_ERROR;
            }
            conn->query_timeout = *value;

        case SEDNA_ATTR_LOG_AMOUNT:
            value = (int*) attrValue;
            if ((*value != SEDNA_LOG_LESS) && (*value != SEDNA_LOG_FULL))
            {
                setDriverErrorMsg(conn, SE3022, NULL);        /* "Invalid argument."*/
                return SEDNA_ERROR;
            }
            // do force commit of existing transaction
            if (conn->isInTransaction == SEDNA_TRANSACTION_ACTIVE)
            {
                int comm_res = commit_handler(conn);
                if(comm_res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED)
                    return SEDNA_ERROR;
            }
            conn->msg.instruction = se_SetSessionOptions;    /*se_SetSessionOptions*/
            conn->msg.length = 13;
            int2net_int(SEDNA_LOG_AMOUNT, conn->msg.body); //option type
            conn->msg.body[4] = 0;
            int2net_int(4, conn->msg.body + 5); //length of value - here sizeof int = 4
            int2net_int(*value, conn->msg.body + 9); //value of attribute - here int
            if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
            {
                connectionFailure(conn, SE3006, "Connection was broken while setting session option on the server", NULL);
                return SEDNA_ERROR;
            }
            if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
            {
                connectionFailure(conn, SE3007, "Connection was broken while setting session option on the server", NULL);
                return SEDNA_ERROR;
            }
            if (conn->msg.instruction == se_SetSessionOptionsOk)
                return SEDNA_SET_ATTRIBUTE_SUCCEEDED;
            else if (conn->msg.instruction == se_ErrorResponse)
            {
                setServerErrorMsg(conn, conn->msg);
                conn->isInTransaction = SEDNA_NO_TRANSACTION;
                return SEDNA_ERROR;
            }
            else
            {
                connectionFailure(conn, SE3008, "Unknown message got while setting session option on the server", NULL);            /* "Unknown message from server" */
                conn->isInTransaction = SEDNA_NO_TRANSACTION;
                return SEDNA_ERROR;
            }

        case SEDNA_ATTR_MAX_RESULT_SIZE:
            value = (int*) attrValue;
            if (*value < 0)
            {
                setDriverErrorMsg(conn, SE3022, "Max result size value must be > 0");        /* "Invalid argument."*/
                return SEDNA_ERROR;
            }
            conn->msg.instruction = se_SetSessionOptions;    /*se_SetSessionOptions*/
            conn->msg.length = 13;
            int2net_int(SEDNA_MAX_RESULT_SIZE, conn->msg.body); //option type
            conn->msg.body[4] = 0;
            int2net_int(4, conn->msg.body+5); //length of value - here sizeof int = 4
            int2net_int(*value, conn->msg.body+9); //value of attribute - here int
            if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
            {
                connectionFailure(conn, SE3006, "Connection was broken while setting session option on the server", NULL);
                return SEDNA_ERROR;
            }
            if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
            {
                connectionFailure(conn, SE3007, "Connection was broken while setting session option on the server", NULL);
                return SEDNA_ERROR;
            }
            if (conn->msg.instruction == se_SetSessionOptionsOk)
                return SEDNA_SET_ATTRIBUTE_SUCCEEDED;
            else if (conn->msg.instruction == se_ErrorResponse)
            {
                setServerErrorMsg(conn, conn->msg);
                conn->isInTransaction = SEDNA_NO_TRANSACTION;
                return SEDNA_ERROR;
            }
            else
            {
                connectionFailure(conn, SE3008, "Unknown message got while setting session option on the server", NULL);            /* "Unknown message from server" */
                conn->isInTransaction = SEDNA_NO_TRANSACTION;
                return SEDNA_ERROR;
            }
            conn->max_result_size = *value;

        default: 
            setDriverErrorMsg(conn, SE3022, NULL);        /* "Invalid argument."*/
            return SEDNA_ERROR;
    }

    return SEDNA_ERROR;
}

int SEgetConnectionAttr(struct SednaConnection *conn, enum SEattr attr, void* attrValue, int* attrValueLength)
{
    int value;

    clearLastError(conn);

    switch (attr){
        case SEDNA_ATTR_AUTOCOMMIT:
            value = (conn->autocommit) ? SEDNA_AUTOCOMMIT_ON: SEDNA_AUTOCOMMIT_OFF;
            memcpy(attrValue, &value, 4);
            *attrValueLength = 4;
            return SEDNA_GET_ATTRIBUTE_SUCCEEDED;
        case SEDNA_ATTR_SESSION_DIRECTORY:
            memcpy(attrValue, conn->session_directory, strlen(conn->session_directory));
            *attrValueLength = (int)strlen(conn->session_directory);
            return SEDNA_GET_ATTRIBUTE_SUCCEEDED;
        case SEDNA_ATTR_BOUNDARY_SPACE_PRESERVE_WHILE_LOAD:
            value = (conn->boundary_space_preserve) ? SEDNA_BOUNDARY_SPACE_PRESERVE_ON: SEDNA_BOUNDARY_SPACE_PRESERVE_OFF;
            memcpy(attrValue, &value, 4);
            *attrValueLength = 4;
            return SEDNA_GET_ATTRIBUTE_SUCCEEDED;
        case SEDNA_ATTR_QUERY_EXEC_TIMEOUT:
            value = (conn->query_timeout);
            memcpy(attrValue, &value, 4);
            *attrValueLength = 4;
            return SEDNA_GET_ATTRIBUTE_SUCCEEDED;
        case SEDNA_ATTR_MAX_RESULT_SIZE:
            value = (conn->max_result_size);
            memcpy(attrValue, &value, 4);
            *attrValueLength = 4;
            return SEDNA_GET_ATTRIBUTE_SUCCEEDED;
        default: 
            setDriverErrorMsg(conn, SE3022, NULL);        /* "Invalid argument."*/
            return SEDNA_ERROR;
    }

    return SEDNA_ERROR;
}

int SEresetAllConnectionAttr(struct SednaConnection *conn)
{
    conn->autocommit = 1;

    if (uGetCurrentWorkingDirectory(conn->session_directory, SE_MAX_DIR_LENGTH, NULL) == NULL)
    {
        connectionFailure(conn, SE4602, NULL, NULL);
        release(conn);
        return SEDNA_ERROR;
    }

    /* Reset all options to their default values */
    conn->msg.instruction = se_ResetSessionOptions;
    conn->msg.length = 0;
    if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3006, "Connection was broken while resetting session option on the server", NULL);
        return SEDNA_ERROR;
    }
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, "Connection was broken while resetting session option on the server", NULL);
        return SEDNA_ERROR;
    }
    if (conn->msg.instruction == se_ResetSessionOptionsOk)
        return SEDNA_RESET_ATTRIBUTES_SUCCEEDED;
    else if (conn->msg.instruction == se_ErrorResponse)
    {
        setServerErrorMsg(conn, conn->msg);
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        return SEDNA_ERROR;
    }
    else
    {
        /* "Unknown message from server" */
        connectionFailure(conn, SE3008, "Unknown message got while resetting session option on the server", NULL);
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        return SEDNA_ERROR;
    }

    return SEDNA_ERROR;
}


void SEsetDebugHandler(struct SednaConnection *conn, debug_handler_t _debug_handler_)
{
    conn->debug_handler = _debug_handler_;
}
