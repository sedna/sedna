/*
 * File:  pping.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPING_H
#define _PPING_H

#include "common/sedna.h"
#include "common/base.h"

#include "common/u/usocket.h"
#include "common/u/uthread.h"
#include "common/u/usem.h"

#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
#ifdef _WIN32
#include <windows.h>
#endif
#endif


#define PPING_MAX_HOSTLEN 128

class pping_client
{
private:
    int port;
    char host[PPING_MAX_HOSTLEN];
    int component;
    USOCKET sock;
    bool stop_keep_alive;
    bool initialized;
    UTHANDLE client_thread_handle;
    UUnnamedSemaphore sem;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////
    /// Timeout counter and flags. 
    /// Timer is implemented above pping since there is no portable way to implement good timer on POSIX systems.
    int counter;
    volatile int timeout;
    volatile bool  reset_flag;
    volatile bool* volatile signaled_flag;
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////

    void throw_exception(SednaUserException& e, bool is_soft);
    void startup(SednaUserException& e, bool is_soft);

#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////
    /// Exception logging. 
#ifdef _WIN32
private:
	volatile LPEXCEPTION_POINTERS exceptPtrs;
	volatile DWORD except_thread_id;
	volatile UFile stacktrace_fh;
public:
	void WriteStackTraceFile(LPEXCEPTION_POINTERS exceptPtrs);
#endif
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////
#endif


public:
    pping_client(int _port_, int _component_, const char* _host_ = NULL);
    pping_client(int _port_, int _component_, volatile bool* volatile _signaled_flag_, const char* _host_ = NULL);
    ~pping_client();

    void startup(SednaUserException& e);
    void startup(SednaUserSoftException& e);
    void shutdown();

    void start_timer(int _timeout_);
    void stop_timer();

    friend U_THREAD_PROC(pping_client_thread_proc, arg);
};


#define PPING_SERVER_THREAD_TABLE_SIZE		(2 * (MAX_SESSIONS_NUMBER + MAX_DBS_NUMBER))


class pping_server
{
public:
    struct thread_table_t
    {
        UTHANDLE handle;
        bool is_running;
        bool is_empty;
    };

private:
    int port;
    USOCKET sock;
    int component;
    bool initialized;
    UTHANDLE server_lstn_thread_handle;
    volatile bool close_lstn_thread;
    thread_table_t thread_table[PPING_SERVER_THREAD_TABLE_SIZE];

public:
    pping_server(int _port_, int _component_);
    ~pping_server();

    void startup();
    void shutdown();

    friend U_THREAD_PROC(pping_server_cli_thread_proc, arg);
    friend U_THREAD_PROC(pping_server_lstn_thread_proc_mt, arg);
    friend U_THREAD_PROC(pping_server_lstn_thread_proc_st, arg);
    friend int client_exception_handler(USOCKET sock, const pping_server *pps);
};


#endif
