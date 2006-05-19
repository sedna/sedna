/*
 * File:  pping.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPING_H
#define _PPING_H

#include "sedna.h"

#include "usocket.h"
#include "uthread.h"
#include "usem.h"


#define PPING_ON

#define PPING_MAX_HOSTLEN 128

class pping_client
{
private:
    int port;
    char host[PPING_MAX_HOSTLEN];
    USOCKET sock;
    bool stop_keep_alive;
    bool initialized;
    UTHANDLE client_thread_handle;
    UUnnamedSemaphore sem;

    void throw_exception(SednaUserException& e, bool is_soft);
    void startup(SednaUserException& e, bool is_soft);

public:
    pping_client(int _port_, const char* _host_ = NULL);
    ~pping_client();

    void startup(SednaUserException& e);
    void startup(SednaUserSoftException& e);
    void shutdown();

    friend U_THREAD_PROC(pping_client_thread_proc, arg);
};


#define PPING_SERVER_THREAD_TABLE_SIZE		100

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
    bool initialized;
    UTHANDLE server_lstn_thread_handle;
    bool close_lstn_thread;
    thread_table_t thread_table[PPING_SERVER_THREAD_TABLE_SIZE];

public:
    pping_server(int _port_);
    ~pping_server();

    void startup();
    void shutdown();

    friend U_THREAD_PROC(pping_server_cli_thread_proc, arg);
    friend U_THREAD_PROC(pping_server_lstn_thread_proc, arg);
};


#endif
