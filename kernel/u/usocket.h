/*
 * File:  usocket.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _USOCKET_H
#define _USOCKET_H

#include "u/u.h"

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

// defines for fd_set
#define U_SSET 					fd_set
#define U_SSET_SIZE				FD_SETSIZE

#define U_SSET_SET 				FD_SET
#define U_SSET_ISSET 			FD_ISSET
#define U_SSET_CLR 				FD_CLR
#define U_SSET_ZERO 			FD_ZERO

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
    int uSocketInit(sys_call_error_fun fun);

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
    int uSocketCleanup(sys_call_error_fun fun);

/* returns U_INVALID_SOCKET if failed */
    USOCKET usocket(int af, int type, int protocol, sys_call_error_fun fun);

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
    int ubind_tcp(USOCKET s, short unsigned int port, const char* addr, sys_call_error_fun fun );

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
    int uconnect_tcp(USOCKET s, unsigned short port, const char *hostname, sys_call_error_fun fun);

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
    int usetsockopt(USOCKET s, int level, int optname, const void* optval, unsigned int optlen, sys_call_error_fun fun);

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
    int ugetsockopt(USOCKET s, int level, int optname, void* optval, unsigned int optlen, sys_call_error_fun fun);

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
    int ulisten(USOCKET s, int backlog, sys_call_error_fun fun);

/* returns U_INVALID_SOCKET if failed */
    USOCKET uaccept(USOCKET s, sys_call_error_fun fun);

/* return value indicates number of bytes received
   returns zero if connection was gracefully closed
   returns U_SOCKET_ERROR in the case of error */
    int urecv(USOCKET s, char *buf, size_t len, sys_call_error_fun fun);

/* return value indicates number of bytes send  
   returns U_SOCKET_ERROR in the case of error  */
    int usend(USOCKET s, const char *buf, size_t len, sys_call_error_fun fun);

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
    int uclose_socket(USOCKET s, sys_call_error_fun fun);

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
    int ushutdown_close_socket(USOCKET s, sys_call_error_fun fun);

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
	int ushutdown_socket(USOCKET s, sys_call_error_fun fun);

/* returns 1 (number of sockets ready to recv) if there is data pending in network connection
   returns 0 if timeout
   returns U_SOCKET_ERROR if failed */
    int uselect_read(USOCKET s, struct timeval *timeout, sys_call_error_fun fun);

/* returns number of sockets ready to recv if there is data pending in network connection 
		(s is changed and contains result of FD_ISSET)
   returns 0 if timeout
   returns U_SOCKET_ERROR if failed */
	int uselect_read_arr(U_SSET *s, USOCKET maxfd, struct timeval *timeout, sys_call_error_fun fun);

/* retrieves the last socket error description */
    const char *usocket_error_translator();

/*  when connect has failed, utry_connect_again checks if it is reasonable to reconnect 
    returns 1 if it is reasonable, zero if it is not */
    int utry_connect_again();

#ifdef __cplusplus
}
#endif

#endif
