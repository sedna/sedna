/*
 * File:  usocket.c
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _WIN32
#include <netdb.h>
#include <string.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif

#include "usocket.h"
#include "d_printf.h"


/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
int uSocketInit(sys_call_error_fun fun)
{
#ifdef _WIN32
    WORD wVersionRequested;
    WSADATA wsaData;
    int err;

    wVersionRequested = MAKEWORD(2, 2);

    err = WSAStartup(wVersionRequested, &wsaData);
    if (err != 0)
    {
        return U_SOCKET_ERROR;
    }

    if (LOBYTE(wsaData.wVersion) != 2 || HIBYTE(wsaData.wVersion) != 2)
    {
        WSACleanup();
        return U_SOCKET_ERROR;
    }

    return 0;
#else
    return 0;
#endif
}

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
int uSocketCleanup(sys_call_error_fun fun)
{
#ifdef _WIN32
    return WSACleanup();
#else
    return 0;
#endif
}


/* returns U_INVALID_SOCKET if failed */
USOCKET usocket(int af, int type, int protocol, sys_call_error_fun fun)
{
#ifdef _WIN32
    return socket(af, type, protocol);
#else
    int sockfd = socket(af, type, protocol);
    if (sockfd == -1)
        return U_INVALID_SOCKET;
    return sockfd;
#endif
}

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
int ubind_tcp(USOCKET s, int port, sys_call_error_fun fun)
{
#ifdef _WIN32
    struct hostent *hp;
    struct sockaddr_in ownaddr;

    if ((hp = gethostbyname("0.0.0.0")) == NULL)
        return U_SOCKET_ERROR;

    memset(&ownaddr, 0, sizeof ownaddr);
    ownaddr.sin_family = AF_INET;
    ownaddr.sin_port = htons(port);
    memcpy(&ownaddr.sin_addr, hp->h_addr, hp->h_length);

    if (bind(s, (struct sockaddr *) &ownaddr, sizeof ownaddr) != 0)
        return U_SOCKET_ERROR;
    return 0;
#else
    struct hostent *hp;
    struct sockaddr_in ownaddr;

    int t_reuse = 1;
    setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *) &t_reuse, sizeof(int));

    if ((hp = gethostbyname("0.0.0.0")) == NULL)
        return U_SOCKET_ERROR;

    memset(&ownaddr, 0, sizeof ownaddr);
    ownaddr.sin_family = AF_INET;
    ownaddr.sin_port = htons(port);
    memcpy(&ownaddr.sin_addr, hp->h_addr, hp->h_length);

    if (bind(s, (struct sockaddr *) &ownaddr, sizeof ownaddr) != 0)
        return U_SOCKET_ERROR;
    return 0;
#endif
}

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
int uconnect_tcp(USOCKET s, int port, const char *hostname, sys_call_error_fun fun)
{
#ifdef _WIN32
    struct hostent *hp;
    struct sockaddr_in ownaddr;

    if ((hp = gethostbyname(hostname)) == NULL)
        return U_SOCKET_ERROR;

    memset(&ownaddr, 0, sizeof ownaddr);
    ownaddr.sin_family = AF_INET;
    ownaddr.sin_port = htons(port);
    memcpy(&ownaddr.sin_addr, hp->h_addr, hp->h_length);

    if (connect(s, (struct sockaddr *) &ownaddr, sizeof ownaddr) != 0)
        return U_SOCKET_ERROR;
    return 0;
#else
    struct hostent *hp;
    struct sockaddr_in ownaddr;

    if ((hp = gethostbyname(hostname)) == NULL)
        return U_SOCKET_ERROR;

    memset(&ownaddr, 0, sizeof ownaddr);
    ownaddr.sin_family = AF_INET;
    ownaddr.sin_port = htons(port);
    memcpy(&ownaddr.sin_addr, hp->h_addr, hp->h_length);

    if (connect(s, (struct sockaddr *) &ownaddr, sizeof ownaddr) != 0)
        return U_SOCKET_ERROR;
    return 0;
#endif
}

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
int usetsockopt(USOCKET s, int level, int optname, const void* optval, int optlen, sys_call_error_fun fun)
{
#ifdef _WIN32
    return setsockopt(s, level, optname, (const char*)optval, optlen);
#else
    return setsockopt(s, level, optname, optval, optlen);
#endif
}

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
int ugetsockopt(USOCKET s, int level, int optname, void* optval, int optlen, sys_call_error_fun fun)
{
#ifdef _WIN32
    return getsockopt(s, level, optname, (char*)optval, &optlen);
#else
    return getsockopt(s, level, optname, optval, &optlen);
#endif
}

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
int ulisten(USOCKET s, int backlog, sys_call_error_fun fun)
{
#ifdef _WIN32
    return listen(s, backlog);
#else
    return listen(s, backlog);
#endif
}

/* returns U_INVALID_SOCKET if failed */
USOCKET uaccept(USOCKET s, sys_call_error_fun fun)
{
#ifdef _WIN32
    struct sockaddr_in commonaddr;
    int commonlen;
    USOCKET socknew;
    memset(&commonaddr, 0, sizeof commonaddr);
    commonlen = sizeof commonaddr;
    socknew = accept(s, (struct sockaddr *) &commonaddr, &commonlen);
    if (socknew == INVALID_SOCKET)
        return U_INVALID_SOCKET;
    return socknew;
#else
    struct sockaddr_in commonaddr;
    unsigned int commonlen;
    USOCKET socknew;
    memset(&commonaddr, 0, sizeof commonaddr);
    commonlen = sizeof commonaddr;
    socknew = accept(s, (struct sockaddr *) &commonaddr, &commonlen);
    if (socknew == -1)
        return U_INVALID_SOCKET;
    return socknew;
#endif
}

/* return value indicates number of bytes received
   returns zero if connection was gracefully closed
   returns U_SOCKET_ERROR in the case of error */
int urecv(USOCKET s, char *buf, int len, sys_call_error_fun fun)
{
#ifdef _WIN32
    int res_len = recv(s, buf, len, 0);
    return res_len;
#else
    int res_len;

    while (1)
    {
        res_len = recv(s, buf, len, 0);

        if (res_len == U_SOCKET_ERROR)
            if (errno == EINTR)
                continue;
            else
                return U_SOCKET_ERROR;
        else
            return res_len;
    }
    return res_len;
#endif
}

/* return value indicates number of bytes send  
   returns U_SOCKET_ERROR in the case of error  */
int usend(USOCKET s, const char *buf, int len, sys_call_error_fun fun)
{
#ifdef _WIN32
    int res_len = send(s, buf, len, 0);
    if (res_len == U_SOCKET_ERROR)
        return U_SOCKET_ERROR;

    return res_len;
#else
    int res_len;

    while (1)
    {
        res_len = send(s, buf, len, 0);
        if (res_len == U_SOCKET_ERROR)
            if (errno == EINTR)
                continue;
            else
                return U_SOCKET_ERROR;
        else
            return res_len;
    }
    return res_len;
#endif
}

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
int uclose_socket(USOCKET s, sys_call_error_fun fun)
{
#ifdef _WIN32
    int res = closesocket(s);
    return res;
#else
    int res = close(s);
    return res;
#endif
}

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
int ushutdown_close_socket(USOCKET s, sys_call_error_fun fun)
{
#ifdef _WIN32
    int res = shutdown(s, 0x02);
    if (res != 0) return U_SOCKET_ERROR;
    res = closesocket(s);
    return res;
#else
    int res = shutdown(s, 2);
    if (res != 0) return U_SOCKET_ERROR;
    res = close(s);
    return res;
#endif
}

/* returns 1 (number of sockets ready to recv) if there is data pending in network connection
   returns 0 if timeout
   returns U_SOCKET_ERROR if failed */
int uselect_read(USOCKET s, struct timeval *timeout, sys_call_error_fun fun)
{
#ifdef _WIN32
    fd_set socks;
    int res = 0;

    FD_ZERO(&socks);
    FD_SET(s, &socks);
    res = select(1, &socks, (fd_set *) NULL, (fd_set *) NULL, timeout);
    return res;
#else
    fd_set socks;
    int res = 0;

    FD_ZERO(&socks);
    FD_SET(s, &socks);

    while (1)
    {
        res = select(s + 1, &socks, (fd_set *) NULL, (fd_set *) NULL, timeout);

        if (res == -1)
            if (errno == EINTR)
                continue;
            else
                return U_SOCKET_ERROR;
        else
            return res;
    }
    return res;
#endif
}

/* retrieves the last socket error description */
const char *usocket_error_translator()
{
#ifdef _WIN32
    static char se_socket_error_buf[1024];
    
    if (!FormatMessage( 
        FORMAT_MESSAGE_FROM_SYSTEM | 
        FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL,
        WSAGetLastError(),
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        (LPTSTR)se_socket_error_buf,
        1024,
        NULL ))
    {
        strcpy(se_socket_error_buf, "unknown error");
    }
    return se_socket_error_buf;
#else
    return strerror(errno);
#endif

}

/*  when connect has failed, utry_connect_again checks if it is reasonable to reconnect 
    returns 1 if it is reasonable, zero if it is not */
int utry_connect_again()
{
#ifdef _WIN32
    int sock_error;
    sock_error = WSAGetLastError();
    if ((sock_error == WSAENETUNREACH) || (sock_error == WSAETIMEDOUT))
        return 1;
    else
        return 0;
#else
    int sock_error;
    sock_error = errno;
    if ((sock_error == ENETUNREACH) || (sock_error == ETIMEDOUT))
        return 1;
    else
        return 0;
#endif
}
