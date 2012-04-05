/*
 * File:  usocket.c
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _WIN32
#include <netdb.h>
#include <string.h>
#include <sys/types.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#else
#include <Winsock2.h>
#include <ws2tcpip.h>
#endif


#include "u/usocket.h"
#include "common/errdbg/d_printf.h"
#include "u/uutils.h"

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
        sys_call_error2("WSAStartup", &err);
        return U_SOCKET_ERROR;
    }

    if (LOBYTE(wsaData.wVersion) != 2 || HIBYTE(wsaData.wVersion) != 2)
    {
        if (WSACleanup() != 0) sys_call_error("WSACleanup");

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
    int res = WSACleanup();
    if (res != 0) sys_call_error("WSACleanup");
    return res;
#else
    return 0;
#endif
}


/* returns U_INVALID_SOCKET if failed */
USOCKET usocket(int af, int type, int protocol, sys_call_error_fun fun)
{
#ifdef _WIN32
    USOCKET res = socket(af, type, protocol);
    if (res == INVALID_SOCKET) sys_call_error("socket");
    return res;
#else
    int param = 1, res = 0;
    int sockfd = socket(af, type, protocol);
    if (sockfd == U_SOCKET_ERROR)
    {
        sys_call_error("socket");
        return U_INVALID_SOCKET;
    }

#if defined(DARWIN)  /// Under DARWIN this is the only way not to get damn SIGPIPE!
    res = setsockopt(sockfd, SOL_SOCKET, SO_NOSIGPIPE, &param, sizeof(int));
    if(-1 == res)
    {
        sys_call_error("setsockopt");
        close(sockfd);
        return U_INVALID_SOCKET;
    } 
#endif /* DARWIN */

    return sockfd;
#endif
}

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
int ubind_tcp(USOCKET s, unsigned short port, const char * addr, sys_call_error_fun fun)
{
    struct addrinfo hints, *hp;
    char buf[8];
#ifndef _WIN32
    int t_reuse = 1;
    setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *) &t_reuse, sizeof(int));
#endif

/* pay attention to getaddrinfo function. It creates addrinfo structure and it needs
 * to get port number as C-string (not as a number -- it leads to segfault) and it
 * also needs some additional options which are written in struct addrinfo hints.
 * addrinfo.ai_family and addrinfo.ai_socktype are obligatory in fact (though it isn't
 * mentioned anywhere) for correct work. Also pay attention to the fact that return codes
 * (of errors) are different in linux and freebsd and some options also may be different.
 * If something goes wrong it's convinient to get info on errors using special function
 * instead of trying to find error codes in sources: * const char *gai_strerror(int errcode);
 * You can find more info with "man getaddrinfo". */

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;

    if (getaddrinfo(addr, int2c_str((int)port, buf), &hints, &hp) != 0)
    {
        sys_call_error("getaddrinfo");
        return U_SOCKET_ERROR;
    }

    if (bind(s, (struct sockaddr *) hp->ai_addr, hp->ai_addrlen) != 0)
    {
        freeaddrinfo(hp);
        sys_call_error("bind");
        return U_SOCKET_ERROR;
    }

    freeaddrinfo(hp);
    return 0;
}

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
int uconnect_tcp(USOCKET s, unsigned short port, const char *hostname, sys_call_error_fun fun)
{
    struct addrinfo hints, *hp;
    /* 8 is enough for c-str for any port (and yes, 5 is not enough :)) */
    char buf[8];

    /*!TODO !FIXME  In new Sedna release there would be new mechanism of starting processes -- this
    check should be there   */
    const char * dest;
    const char * localhost = "localhost";
    if (strcmp (hostname, "0.0.0.0") == 0) {
        dest = localhost;
    } else {
        dest = hostname;
    }

    /* pay attention to getaddrinfo function. It creates addrinfo structure and it needs
     * to get port number as C-string (not as a number -- it leads to segfault) and it
     * also needs some additional options which are written in struct addrinfo hints.
     * addrinfo.ai_family and addrinfo.ai_socktype are obligatory in fact (though it isn't
     * mentioned anywhere) for correct work. Also pay attention to the fact that return codes
     * (of errors) are different in linux and freebsd and some options also may be different.
     * If something goes wrong it's convinient to get info on errors using special function
     * instead of trying to find error codes in sources: * const char *gai_strerror(int errcode);
     * You can find more info with "man getaddrinfo". */


    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    
    if (getaddrinfo(dest, int2c_str((int) port, buf), &hints, &hp) != 0)
    {
        sys_call_error("getaddrinfo");
        return U_SOCKET_ERROR;
    }

    if (connect(s, (struct sockaddr *) hp->ai_addr, hp->ai_addrlen) != 0)
    {
        sys_call_error("connect");
        freeaddrinfo(hp);
        return U_SOCKET_ERROR;
    }
    freeaddrinfo(hp);
    return 0;
}

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
int usetsockopt(USOCKET s, int level, int optname, const void* optval, unsigned int optlen, sys_call_error_fun fun)
{
#ifdef _WIN32
    int res = setsockopt(s, level, optname, (const char*)optval, optlen);
#else
    int res = setsockopt(s, level, optname, optval, optlen);
#endif
    if (res == U_SOCKET_ERROR) sys_call_error("setsockopt");
    return res;
}

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
int ugetsockopt(USOCKET s, int level, int optname, void* optval, unsigned int optlen, sys_call_error_fun fun)
{
#ifdef _WIN32
    int res = getsockopt(s, level, optname, (char*)optval, &optlen);
#else
    int res = getsockopt(s, level, optname, optval, &optlen);
#endif
    if (res == U_SOCKET_ERROR) sys_call_error("getsockopt");
    return res;
}

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
int ulisten(USOCKET s, int backlog, sys_call_error_fun fun)
{
    int res = listen(s, backlog);
    if (res == U_SOCKET_ERROR) sys_call_error("listen");
    return res;
}

/* returns U_INVALID_SOCKET if failed */
USOCKET uaccept(USOCKET s, sys_call_error_fun fun)
{
#ifdef _WIN32
    int commonlen;
#else
    unsigned int commonlen;
#endif
    int res = 0, param = 1; 
    struct sockaddr_in commonaddr;
    USOCKET socknew;

    memset(&commonaddr, 0, sizeof commonaddr);
    commonlen = sizeof commonaddr;
    socknew = accept(s, (struct sockaddr *) &commonaddr, &commonlen);
    if (socknew == U_INVALID_SOCKET)
    {
        sys_call_error("accept");
        return U_INVALID_SOCKET;
    }
#if defined(DARWIN)  /// Under DARWIN this is the only way not to get damn SIGPIPE!
    res = setsockopt(socknew, SOL_SOCKET, SO_NOSIGPIPE, &param, sizeof(int));
    if(-1 == res)
    {
        sys_call_error("setsockopt");
        close(socknew);
        return U_INVALID_SOCKET;
    } 
#endif /* DARWIN */
    return socknew;
}

/* return value indicates number of bytes received
   returns zero if connection was gracefully closed
   returns U_SOCKET_ERROR in the case of error */
int urecv(USOCKET s, char *buf, size_t len, sys_call_error_fun fun)
{
#ifdef _WIN32
    int res_len = recv(s, buf, (int)len, 0);
    if (res_len == U_SOCKET_ERROR) sys_call_error("recv");
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
            {
                sys_call_error("recv");
                return U_SOCKET_ERROR;
            }
        else
            return res_len;
    }
#endif
}

/* return value indicates number of bytes send  
   returns U_SOCKET_ERROR in the case of error  */
int usend(USOCKET s, const char *buf, size_t len, sys_call_error_fun fun)
{
#ifdef _WIN32
    int res_len;
    res_len = send(s, buf, (int)len, 0);
    if (res_len == U_SOCKET_ERROR)
    {
        sys_call_error("send");
        return U_SOCKET_ERROR;
    }

    return res_len;
#else
    int res_len;

    while (1)
    {
        res_len = send(s, buf, len, U_MSG_NOSIGNAL);
        if (res_len == U_SOCKET_ERROR)
            if (errno == EINTR)
                continue;
            else
            {
                sys_call_error("send");
                return U_SOCKET_ERROR;
            }
        else
            return res_len;
    }
#endif
}

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
int uclose_socket(USOCKET s, sys_call_error_fun fun)
{
#ifdef _WIN32
    int res = closesocket(s);
#else
    int res = close(s);
#endif
    if (res == U_SOCKET_ERROR) sys_call_error("closesocket");
    return res;
}

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
int ushutdown_close_socket(USOCKET s, sys_call_error_fun fun)
{
#ifdef _WIN32
    int res = shutdown(s, SD_BOTH);
    if (res != 0)
    {
       sys_call_error("shutdown");
       return U_SOCKET_ERROR;
    }
    res = closesocket(s);
#else
    int res = shutdown(s, SHUT_RDWR);
    if (res != 0)
    {
       if(errno != ENOTCONN)
       {
           sys_call_error("shutdown");       
           return U_SOCKET_ERROR;
       }
    }
    res = close(s);
#endif
    if (res == U_SOCKET_ERROR) sys_call_error("closesocket");
    return res;
}

/* returns zero if succeeded
   returns U_SOCKET_ERROR if failed */
int ushutdown_socket(USOCKET s, sys_call_error_fun fun)
{
#ifdef _WIN32
    int res = shutdown(s, SD_BOTH);
    if (res != 0)
    {
       sys_call_error("shutdown");
       return U_SOCKET_ERROR;
    }
    return res;
#else
    int res = shutdown(s, SHUT_RDWR);
    if (res != 0)
    {
       if(errno != ENOTCONN)
       {
           sys_call_error("shutdown");       
           return U_SOCKET_ERROR;
       }
    }
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
    if (res == U_SOCKET_ERROR) sys_call_error("select");
    return res;
#else
    fd_set socks;
    int res = 0;

    FD_ZERO(&socks);
    FD_SET(s, &socks);

    while (1)
    {
        res = select(s + 1, &socks, (fd_set *) NULL, (fd_set *) NULL, timeout);

        if (res == U_SOCKET_ERROR)
            if (errno == EINTR)
                continue;
            else
            {
                sys_call_error("select");
                return U_SOCKET_ERROR;
            }
        else
            return res;
    }
#endif
}

/* returns number of sockets ready to recv if there is data pending in network connection 
		(s is changed and contains result)
   returns 0 if timeout
   returns U_SOCKET_ERROR if failed */
int uselect_read_arr(U_SSET *s, USOCKET maxfd, struct timeval *timeout, sys_call_error_fun fun)
{
#ifdef _WIN32
    int res = 0;

    res = select(1, s, (fd_set *) NULL, (fd_set *) NULL, timeout);
    if (res == U_SOCKET_ERROR) sys_call_error("select");

    return res;
#else
	int res = 0;

    while (1)
    {
        res = select(maxfd + 1, s, (fd_set *) NULL, (fd_set *) NULL, timeout);

        if (res == U_SOCKET_ERROR)
            if (errno == EINTR)
			{
                continue;
            }
            else
            {
                sys_call_error("select");
                return U_SOCKET_ERROR;
            }
        else
        	return res;
    }

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
