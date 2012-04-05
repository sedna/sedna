/*
 * File:  sp.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _SP_H
#define _SP_H

#include "u/usocket.h"
#include "sp_defs.h"


#ifdef __cplusplus
extern "C"
{
#endif

/* returns zero - if succeeded;                        
   returns 1 - if Message length exceeds available size
   returns U_SOCKET_ERROR if error */
    int sp_recv_msg(USOCKET s, struct msg_struct *msg);

/* returns zero if succeeded
   returns U_SOCKET_ERROR if error */
    int sp_send_msg(USOCKET s, const struct msg_struct *msg);

/*  sends error message to client. 
    Error message contains message instruction, message length, error code, error info.
    returns zero if succeeded, U_SOCKET_ERROR if failed */
    int sp_error_message_handler(USOCKET s, int error_ins, int error_code, const char *error_info);

#ifdef __cplusplus
}
#endif

#endif
