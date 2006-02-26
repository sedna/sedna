/*
 * File:  usocket.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef USOCKET_H
#define USOCKET_H


#include "u.h"

#ifdef __cplusplus
extern "C"
{
#endif

#ifdef _WIN32
    typedef SOCKET USOCKET;

#define U_INVALID_SOCKET			INVALID_SOCKET
#define U_SOCKET_ERROR              SOCKET_ERROR

#else
#include <sys/socket.h>
    
    typedef int USOCKET;

#define U_INVALID_SOCKET			(-1)
#define U_SOCKET_ERROR              (-1)

#endif


/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
    int uSocketInit();

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
    int uSocketCleanup();

/* returns U_INVALID_SOCKET if failed */
    USOCKET usocket(int af, int type, int protocol);

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
    int ubind_tcp(USOCKET s, int port);

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
    int uconnect_tcp(USOCKET s, int port, const char *hostname);

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
    int usetsockopt(USOCKET s, int level, int optname, const void* optval, int optlen);

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
    int ugetsockopt(USOCKET s, int level, int optname, void* optval, int optlen);

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
    int ulisten(USOCKET s, int backlog);

/* returns U_INVALID_SOCKET if failed */
    USOCKET uaccept(USOCKET s);

/* return value indicates number of bytes received
   returns zero if connection was gracefully closed
   returns U_SOCKET_ERROR in the case of error */
    int urecv(USOCKET s, char *buf, int len);

/* return value indicates number of bytes send  
   returns U_SOCKET_ERROR in the case of error  */
    int usend(USOCKET s, const char *buf, int len);

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
    int uclose_socket(USOCKET s);

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
    int ushutdown_close_socket(USOCKET s);

/* returns 1 (number of sockets ready to recv) if there is data pending in network connection
   returns 0 if timeout
   returns U_SOCKET_ERROR if failed */
    int uselect_read(USOCKET s, struct timeval *timeout);

/* retrieves the last socket error description */
    const char *usocket_error_translator();

/*  when connect has failed, utry_connect_again checks if it is reasonable to reconnect 
    returns 1 if it is reasonable, zero if it is not */
    int utry_connect_again();

#ifdef __cplusplus
}
#endif


#endif
