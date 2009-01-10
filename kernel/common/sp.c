/*
 * File:  sp.c
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sp.h"
#include "common/errdbg/d_printf.h"
#include "common/u/uutils.h"

/* returns zero - if succeeded;                        
   returns 1 - if Message length exceeds available size
   returns U_SOCKET_ERROR if error */
int sp_recv_msg(USOCKET s, struct msg_struct *msg)
{
    int rc = 0, got = 0;
    __int64 buf = 0;
    char* ptr = (char*)&buf;

    while (got < 8)
    {
        rc = urecv(s, ptr + got, 8 - got, __sys_call_error);
        if ((rc == U_SOCKET_ERROR) || (rc == 0))
            return U_SOCKET_ERROR;
        got += rc;
    }

    msg->instruction = ntohl(*(sp_int32 *) ptr);
    msg->length = ntohl(*(sp_int32 *) (ptr + 4));
    if (msg->length > SE_SOCKET_MSG_BUF_SIZE)
    {
        return 1;               /* Message length exceeds available size */
    }

    got = 0;
    while (got < msg->length)
    {
        rc = urecv(s, msg->body + got, msg->length - got, __sys_call_error);
        if ((rc == U_SOCKET_ERROR) || (rc == 0))
            return U_SOCKET_ERROR;
        got += rc;
    }
    return 0;
}


/* returns zero if succeeded
   returns U_SOCKET_ERROR if error */
int sp_send_msg(USOCKET s, const struct msg_struct *msg)
{
    __int64 buf = 0;
    char* ptr = (char*)&buf;
    int rc = 0, sent = 0;

    *(sp_int32*)ptr = htonl(msg->instruction);
    *((sp_int32*)(ptr + 4)) = htonl(msg->length);
    while (sent < 8)
    {
        rc = usend(s, ptr + sent, 8 - sent, __sys_call_error);
        if (rc == U_SOCKET_ERROR)
            return U_SOCKET_ERROR;
        sent += rc;
    }

    sent = rc = 0;
    while (sent < msg->length)
    {
        rc = usend(s, (const char *) (msg->body + sent), msg->length - sent, __sys_call_error);
        if (rc == U_SOCKET_ERROR)
            return U_SOCKET_ERROR;
        sent += rc;
    }

    return 0;
}

/*  sends error message to client. 
    Error message contains message instruction, message length, error code, error info.
    returns zero if succeeded, U_SOCKET_ERROR if failed */
int sp_error_message_handler(USOCKET s, int error_ins, int error_code, const char *error_info)
{
    struct msg_struct server_msg;
    int err_length = strlen(error_info);

    server_msg.instruction = error_ins;
    server_msg.length = err_length + 5 + 4;
    *(sp_int32*)server_msg.body = htonl(error_code);  /* this is error code */
    server_msg.body[4] = 0;
    *(sp_int32*)(server_msg.body + 5) = htonl(err_length);  /* this is error_info length */
    memcpy(server_msg.body + 9, error_info, err_length);

    return sp_send_msg(s, &server_msg);
}
