/*
 * File:  libsedna.c
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "libsedna.h"
#include "error_codes.h"
#include "usocket.h"
#include "uhdd.h"
#include "sp.h"
#include "uutils.h"
#include "d_printf.h"

/**********************************************************************************************************

                      internal driver functions

***********************************************************************************************************/

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
      if (col_name)
           if (strcmp(conn->cbl.col_name, col_name) == 0)
              return 1;
           else
              return 0;
      else 
          return 1;
   
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
        connectionFailure(conn, SE3007, NULL, NULL);
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
            connectionFailure(conn, SE3007, NULL, NULL);
            return SEDNA_ERROR;
        }
    }

    conn->socket_keeps_data = 0;
    if (conn->msg.instruction == se_ResultEnd)
        conn->result_end = 1;

    return 1;
}

/* takes the data from server when execute a query and decide if the query failed or succeeded*/
static int resultQueryHandler(struct SednaConnection *conn)
{
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, NULL, NULL);
        return SEDNA_ERROR;
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
    else if (conn->msg.instruction == se_ItemPart)      /* ItemPart */
    {
        memcpy(conn->local_data_buf, conn->msg.body + 5, conn->msg.length - 5);
        conn->local_data_length = conn->msg.length - 5;
        conn->local_data_offset = 0;
        conn->socket_keeps_data = 1;    /* set the flag - Socket keeps item data */
        conn->result_end = 0;           /* set the flag - there no items */
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

        return SEDNA_QUERY_SUCCEEDED;
    }
    else
    {
        connectionFailure(conn, SE3008, NULL, NULL);            /* "Unknown message from server" */
        conn->socket_keeps_data = 0;    /*set the flag - Socket keeps item data*/
        conn->local_data_offset = 0;
        conn->local_data_length = 0;
        conn->result_end = 1;   /*set the flag - there are no items*/
        conn->in_query = 0;
        conn->isInTransaction = SEDNA_NO_TRANSACTION;

        return SEDNA_QUERY_FAILED;
    }

}

static int begin_handler(struct SednaConnection *conn)
{
    /* send 210 - BeginTransaction*/
    conn->msg.instruction = se_BeginTransaction;
    conn->msg.length = 0;
    if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3006, NULL, NULL);
        return SEDNA_ERROR;
    }

    /* read 100 or 230 - BeginTransactionOk or 240 - BeginTransactionFailed*/
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, NULL, NULL);
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
        connectionFailure(conn, SE3006, NULL, NULL);
        return SEDNA_ERROR;
    }

    /* read 100 or 250 - CommitTransactionOk or 260 - CommitTransactionFailed*/
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, NULL, NULL);
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
        conn->in_query = 0;
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
        connectionFailure(conn, SE3006, NULL, NULL);
        return SEDNA_ERROR;
    }

    /* read 100 or 255 - RollbackTransactionOk or 265 - RollbackTransactionFailed*/
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, NULL, NULL);
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

static void release(struct SednaConnection *conn)
{
    /* release*/
    ushutdown_close_socket(conn->socket, NULL);
    uSocketCleanup(NULL);
}

static int execute(struct SednaConnection *conn)
{
    /* read 320 - QuerySucceeded, 330 - QueryFailed, 340 - UpdateSucceeded or 350 - UpdateFailed*/
    /* or 430 - BulkLoadFileName, 431 - BulkLoadFromStream, 100 - ErrorResponse.*/
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, NULL, NULL);
        return SEDNA_ERROR;
    }
    if (conn->msg.instruction == se_ErrorResponse)
    {
        setServerErrorMsg(conn, conn->msg);
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        return SEDNA_ERROR;
    }
    else if (conn->msg.instruction == se_QuerySucceeded)        /*QuerySucceeded*/
    {
        int query_result;
        query_result = resultQueryHandler(conn);
        conn->first_next = 1;
        return query_result;
    }
    else if (conn->msg.instruction == se_QueryFailed)   /*QueryFailed*/
    {
        setServerErrorMsg(conn, conn->msg);
        conn->in_query = 0;
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        return SEDNA_QUERY_FAILED;
    }
    else if (conn->msg.instruction == se_UpdateSucceeded)       /*UpdateSucceeded*/
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
    else if (conn->msg.instruction == se_UpdateFailed)  /*UpdateFailed*/
    {
        setServerErrorMsg(conn, conn->msg);
        conn->in_query = 0;
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        return SEDNA_UPDATE_FAILED;
    }
    else if (conn->msg.instruction == se_BulkLoadFileName)      /*BulkLoadFileName*/
    {
        /* open file. Read from file and send 410 - BulkLoadPortion. 420 - BulkLoadEnd.*/
        /*     cout << "bulk load from file " << string(msg.body+5).c_str() << " ...";*/
        UFile file_handle;
        int already_read = 1, res = 1;
        char *filename = conn->msg.body + 5;
        filename[s_min(conn->msg.length - 5, SE_SOCKET_MSG_BUF_SIZE - 6)] = '\0';
        file_handle = uOpenFile(filename, 0, U_READ, 0, NULL);
        if (file_handle == U_INVALID_FD)
        {
            /* send 400 - BulkLoadError*/
            conn->msg.instruction = se_BulkLoadError;
            conn->msg.length = 0;
            if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
            {
                connectionFailure(conn, SE3006, NULL, NULL);
                return SEDNA_ERROR;
            }
            setDriverErrorMsg(conn, SE3017, filename);
            if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
            {
                connectionFailure(conn, SE3007, NULL, NULL);
                return SEDNA_ERROR;
            }
            conn->isInTransaction = SEDNA_NO_TRANSACTION;
            return SEDNA_BULK_LOAD_FAILED;
        }

        while ((res > 0) && (already_read != 0))
        {
            res = uReadFile(file_handle, conn->msg.body + 5, BULK_LOAD_PORTION, &already_read, NULL);
            if (res == 0)
            {
                /* send 400 - BulkLoadError*/
                conn->msg.instruction = se_BulkLoadError;
                conn->msg.length = 0;
                uCloseFile(file_handle, NULL);
                if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
                {
                    connectionFailure(conn, SE3006, NULL, NULL);
                    return SEDNA_ERROR;
                }
                if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
                {
                    connectionFailure(conn, SE3007, NULL, NULL);
                    return SEDNA_ERROR;
                }
                conn->isInTransaction = SEDNA_NO_TRANSACTION;
                setDriverErrorMsg(conn, SE3018, NULL);
                return SEDNA_BULK_LOAD_FAILED;
            }

            if (already_read == 0)
                break;
            conn->msg.instruction = se_BulkLoadPortion; /*BulkLoadPortion*/
            conn->msg.length = 5 + already_read;
            conn->msg.body[0] = 0;
            int2net_int(already_read, conn->msg.body + 1);

            if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
            {
                connectionFailure(conn, SE3006, NULL, NULL);
                uCloseFile(file_handle, NULL);
                return SEDNA_ERROR;
            }
        }

        if (!uCloseFile(file_handle, NULL))
        {
            setDriverErrorMsg(conn, SE3019, NULL);
            return SEDNA_ERROR;
        }

        conn->msg.instruction = se_BulkLoadEnd; /*BulkLoadEnd*/
        conn->msg.length = 0;
        if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3006, NULL, NULL);
            return SEDNA_ERROR;
        }

        if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3007, NULL, NULL);
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
            conn->in_query = 0;
            if (conn->autocommit)
            {
               int comm_res = commit_handler(conn);
               if(comm_res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED)
                   return SEDNA_BULK_LOAD_FAILED;
            }
            return SEDNA_BULK_LOAD_SUCCEEDED;
        }
        else if ((conn->msg.instruction == se_UpdateFailed) || (conn->msg.instruction == se_BulkLoadFailed))
        {
            setServerErrorMsg(conn, conn->msg);
            conn->in_query = 0;
            conn->isInTransaction = SEDNA_NO_TRANSACTION;
            return SEDNA_BULK_LOAD_FAILED;
        }

    }                           /* Bulk Load from File*/
    else if (conn->msg.instruction == se_BulkLoadFromStream)    /*BulkLoadFromStream*/
    {
        conn->in_query = 0;
        return SEDNA_UPDATE_FAILED;
    }
    else
    {
        connectionFailure(conn, SE3008, NULL, NULL);            /* "Unknown message from server" */
        return SEDNA_ERROR;
    }
    return SEDNA_ERROR;
}

/**********************************************************************************************************

                       driver functions implementation

***********************************************************************************************************/

int SEconnect(struct SednaConnection *conn, const char *url, const char *db_name, const char *login, const char *password)
{
    char host[SE_HOSTNAMELENGTH + 1];
    int port = 5050, db_name_len = 0, login_len = 0, password_len = 0, url_len = 0;
    int body_position = 0, host_len = 0, socket_optval = 1, socket_optsize = sizeof(int);

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
        host_len = url_len;

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
        connectionFailure(conn, SE3006, NULL, NULL);
        release(conn);
        return SEDNA_OPEN_SESSION_FAILED;
    }
    /* read msg. 140 - SendSessionParameters*/
    /* send protocol version, login, dbname. SessionParameters - 120*/
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, NULL, NULL);
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
        conn->msg.length = 2 + 5 + login_len + 5 + db_name_len;

        /* writing protocol version 1.0*/
        conn->msg.body[0] = 1;
        conn->msg.body[1] = 0;

        /* writing login */
        conn->msg.body[2] = 0;  /* format code*/
        int2net_int(login_len, conn->msg.body + 3);
        memcpy(conn->msg.body + 7, login, login_len);
        body_position += 7 + strlen(login);

        /* writing db_name       */
        conn->msg.body[body_position] = 0;      /* format code*/
        int2net_int(db_name_len, conn->msg.body + body_position + 1);
        body_position += 5;
        memcpy(conn->msg.body + body_position, db_name, db_name_len);

        if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3006, NULL, NULL);
            release(conn);
            return SEDNA_OPEN_SESSION_FAILED;
        }
    }
    else
        goto UnknownMsg;

    /* read - error or SendAuthenticationParameters - 150*/
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, NULL, NULL);
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
        conn->msg.length = 5 + password_len;

        /* writing password      */
        conn->msg.body[0] = 0;  /* format code*/
        int2net_int(password_len, conn->msg.body + 1);
        memcpy(conn->msg.body + 5, password, password_len);

        if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3006, NULL, NULL);
            release(conn);
            return SEDNA_OPEN_SESSION_FAILED;
        }
    }
    else
        goto UnknownMsg;

    /* read AuthenticationOk - 160 or AuthenticationFailed - 170.*/
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, NULL, NULL);
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

        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        conn->isConnectionOk = SEDNA_CONNECTION_OK;

        return SEDNA_SESSION_OPEN;
    }

  UnknownMsg:
    connectionFailure(conn, SE3008, NULL, NULL);      /* "Unknown message from server"*/
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
             return SEDNA_CLOSE_SESSION_FAILED;        
    }

    /* send 500 - CloseConnection*/
    conn->msg.instruction = se_CloseConnection;
    conn->msg.length = 0;
    if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3006, NULL, NULL);
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        conn->isConnectionOk = SEDNA_CONNECTION_CLOSED;
        release(conn);
        return SEDNA_ERROR;
    }

    /* read 100 or 510 - CloseConnectionOk or 520 - TransactionRollbackBeforeClose*/
    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, NULL, NULL);
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
        connectionFailure(conn, SE3008, NULL, NULL);            /* "Unknown message from server" */
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

    return commit_handler(conn);
}

int SEexecuteLong(struct SednaConnection *conn, FILE * query_file)
{
    int read = 0;

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

    while ((read < SE_SOCKET_MSG_BUF_SIZE - 6) && (!feof(query_file)))
    {
        read += fread(conn->msg.body + 6 + read, sizeof(char), SE_SOCKET_MSG_BUF_SIZE - 6 - read, query_file);
    }
    if (feof(query_file))
    {
        conn->msg.instruction = se_Execute;
        conn->msg.body[0] = 0;  /* result format code*/
        conn->msg.body[1] = 0;  /* string format*/
        int2net_int(read, conn->msg.body + 2);
        conn->msg.length = read + 6;    /* body containes: result format (sxml=1 or xml=0) - 1 byte)*/
        /* string format - 1 byte;*/
        /* string length - 4 bytes*/
        /* string*/
        if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3006, NULL, NULL);
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
            int2net_int(read, conn->msg.body + 2);
            conn->msg.length = read + 6;        /* body containes: result format (sxml=1 or xml=0) - 1 byte)*/
            /* string format - 1 byte;*/
            /* string length - 4 bytes*/
            /* string*/
            if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
            {
                connectionFailure(conn, SE3006, NULL, NULL);
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
            connectionFailure(conn, SE3006, NULL, NULL);
            return SEDNA_ERROR;
        }
    }

    return execute(conn);
}

int SEexecute(struct SednaConnection *conn, const char *query)
{
    int query_length = 0, query_portion_size = 0, i = 0;

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
            query_portion_size = ((query_length - i) >= (SE_SOCKET_MSG_BUF_SIZE - 6)) ? (SE_SOCKET_MSG_BUF_SIZE - 6) : (query_length - i);
            int2net_int(query_portion_size, conn->msg.body + 2);

            memcpy(conn->msg.body + 6, query + i, query_portion_size);

            conn->msg.length = query_portion_size + 6;  /* body containes: result format (sxml=1 or xml=0) - 1 byte)*/
            /* string format - 1 byte;*/
            /* string length - 4 bytes*/
            /* string*/
            if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
            {
                connectionFailure(conn, SE3006, NULL, NULL);
                return SEDNA_ERROR;
            }

            i += query_portion_size;
        }
        /*send 302 - LongQueryEnd*/
        conn->msg.instruction = se_LongQueryEnd;
        conn->msg.length = 0;

        if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3006, NULL, NULL);
            return SEDNA_ERROR;
        }
    }
    else
    {
        /*send 300 - ExecuteQuery*/
        conn->msg.instruction = se_Execute;
        conn->msg.length = query_length + 6;    /* body containes: result format (sxml=1 or xml=0) - 1 byte)*/
        /* string format - 1 byte;*/
        /* string length - 4 bytes*/
        /* string*/
        conn->msg.body[0] = 0;  /* result format code*/
        conn->msg.body[1] = 0;  /* string format*/
        int2net_int(query_length, conn->msg.body + 2);

        memcpy(conn->msg.body + 6, query, query_length);
        if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3006, NULL, NULL);
            return SEDNA_ERROR;
        }
    }
    return execute(conn);
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

    if (conn->first_next)
    {
        conn->first_next = 0;
        return SEDNA_NEXT_ITEM_SUCCEEDED;
    }

    if (conn->result_end)
        return SEDNA_RESULT_END;

    /* clean socket*/
    if (cleanSocket(conn) == SEDNA_ERROR)
        return SEDNA_ERROR;

    conn->first_next = 0;

    /*send GetNextItem - 310*/
    conn->msg.instruction = se_GetNextItem;
    conn->msg.length = 0;

    if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3006, NULL, NULL);
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
        setDriverErrorMsg(conn, SE3022, NULL);        /* "Invalid argument."*/
        conn->result_end = 1;   /* tell result is finished*/
        conn->socket_keeps_data = 0;    /* tell there is no data in socket*/
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
                connectionFailure(conn, SE3007, NULL, NULL);
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
            if (conn->msg.instruction == se_ItemPart)   /*ItemPortion*/
            {
                if ((conn->msg.length - 5) > bytes_to_read)
                {
                    memcpy(buf + buf_position, conn->msg.body + 5, bytes_to_read);
                    buf_position += bytes_to_read;
                    memcpy(conn->local_data_buf, conn->msg.body + 5 + bytes_to_read, conn->msg.length - 5 - bytes_to_read);
                    conn->local_data_length = conn->msg.length - 5 - bytes_to_read;
                    return buf_position;
                }
                else
                {
                    memcpy(buf + buf_position, conn->msg.body + 5, conn->msg.length - 5);
                    buf_position += (conn->msg.length - 5);
                    bytes_to_read -= (conn->msg.length - 5);
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
                return buf_position;
            }
            else
            {
                connectionFailure(conn, SE3008, NULL, NULL);            /* "Unknown message from server" */
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
        int query_size = 0;

        /*send 300 - ExecuteQuery*/
        conn->msg.instruction = 300;
        conn->msg.body[0] = 0;  /* result format code*/
        conn->msg.body[1] = 0;  /* string format*/

        query_str = conn->msg.body + 6;
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

        int2net_int(query_size, conn->msg.body + 2);
        conn->msg.length = query_size + 6;      /* body containes: result format (sxml=1 or xml=0) - 1 byte)*/
        /* string format - 1 byte;*/
        /* string length - 4 bytes*/
        /* string*/

        if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3006, NULL, NULL);
            return SEDNA_ERROR;
        }
        if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3007, NULL, NULL);
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
            connectionFailure(conn, SE3006, NULL, NULL);
            return SEDNA_ERROR;
        }
        if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
        {
            connectionFailure(conn, SE3007, NULL, NULL);
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
            connectionFailure(conn, SE3006, NULL, NULL);
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
        setDriverErrorMsg(conn, SE3028, NULL);        /* "Connection with server is closed or have not been established yet." */
        return SEDNA_ERROR;
    }
    if (conn->isConnectionOk != SEDNA_CONNECTION_OK)
        return SEDNA_ERROR;

    clearLastError(conn);

    /* clean socket*/
    if (cleanSocket(conn) == SEDNA_ERROR)
        return SEDNA_ERROR;

    conn->msg.instruction = se_BulkLoadEnd;     /*BulkLoadEnd*/
    conn->msg.length = 0;

    if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3006, NULL, NULL);
        return SEDNA_ERROR;
    }
    
    setBulkLoadFinished(conn);

    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3007, NULL, NULL);
        return SEDNA_ERROR;
    }

    if (conn->msg.instruction == se_ErrorResponse)
    {
        setServerErrorMsg(conn, conn->msg);
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        return SEDNA_ERROR;
    }
    else if ((conn->msg.instruction == se_BulkLoadSucceeded) || (conn->msg.instruction == se_UpdateSucceeded))  /*BulkLoadSucceeded*/
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
    else if ((conn->msg.instruction == se_BulkLoadFailed) || (conn->msg.instruction == se_UpdateFailed))        /*BulkLoadFailed*/
    {
        conn->in_query = 0;
        setServerErrorMsg(conn, conn->msg);
        conn->isInTransaction = SEDNA_NO_TRANSACTION;
        return SEDNA_BULK_LOAD_FAILED;
    }
    else
    {
        connectionFailure(conn, SE3008, NULL, NULL);            /* "Unknown message from server" */
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

    conn->msg.instruction = se_ShowTime;        /*ShowTime*/
    conn->msg.length = 0;

    clearLastError(conn);

    if (conn->in_query)
    {
        strcpy(conn->query_time, "not available");
        return conn->query_time;
    }
    if (sp_send_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3006, NULL, NULL);
        strcpy(conn->query_time, "not available");
        return conn->query_time;
    }

    if (sp_recv_msg(conn->socket, &(conn->msg)) != 0)
    {
        connectionFailure(conn, SE3006, NULL, NULL);
        strcpy(conn->query_time, "not available");
        return conn->query_time;
    }

    if (conn->msg.instruction == se_LastQueryTime)      /*LastQueryTime*/
    {
        strcpy(conn->query_time, conn->msg.body + 5);
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
         default: 
             setDriverErrorMsg(conn, SE3022, NULL);        /* "Invalid argument."*/
             return SEDNA_ERROR;
    }
    
    return SEDNA_ERROR;
}

int SEgetConnectionAttr(struct SednaConnection *conn, enum SEattr attr, void* attrValue, int* attrValueLength)
{
    int *value;
    
    clearLastError(conn);
    
    switch (attr){
        case SEDNA_ATTR_AUTOCOMMIT:
            *value = (conn->autocommit) ? SEDNA_AUTOCOMMIT_ON: SEDNA_AUTOCOMMIT_OFF;
            memcpy(attrValue, value, 4);
            *attrValueLength = 4;
            return SEDNA_GET_ATTRIBUTE_SUCCEEDED;
         default: 
             setDriverErrorMsg(conn, SE3022, NULL);        /* "Invalid argument."*/
             return SEDNA_ERROR;
    }
    
    return SEDNA_ERROR;
}
