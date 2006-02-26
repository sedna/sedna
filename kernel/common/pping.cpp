/*
 * File:  pping.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "pping.h"
#include "uprocess.h"
#include "d_printf.h"


#ifdef _WIN32
#define PPING_STACK_SIZE		10240
#else
#define PPING_STACK_SIZE		102400
#endif

#define PPING_KEEP_ALIVE_MSG	'a'
#define PPING_DISCONNECT_MSG	'b'

#define PPING_LSTNR_QUEUE_LEN	100



////////////////////////////////////////////////////////////////////////////////
/// pping_client
////////////////////////////////////////////////////////////////////////////////

U_THREAD_PROC(pping_client_thread_proc, arg)
{
    if (uThreadBlockAllSignals() != 0)
        d_printf1("Failed to block signals for pping_client_thread_proc");

    pping_client *ppc = (pping_client*)arg;

    char c = PPING_KEEP_ALIVE_MSG;
    while (true)
    {
        if (ppc->stop_keep_alive) return 0;
        if (usend(ppc->sock, &c, sizeof(c)) != sizeof(c))
        {
            if (!ppc->stop_keep_alive)
            {
                if (ppc->failure_str) printf("%s\n", ppc->failure_str);
                else printf("System failure\n");
                printf("pping_client_thread_proc\n");
                d_flush();
                uExitProcess(1);
            }
        }
//        uSleep(1);
        UUnnamedSemaphoreDown(&(ppc->sem), 1000);
    }
    return 0;
}

pping_client::pping_client(const char *_failure_str_, int _port_, const char* _host_)
{
#ifdef PPING_ON
    if (_failure_str_)
    {
        failure_str = new char[strlen(_failure_str_) + 1];
        strcpy(failure_str, _failure_str_);
    }
    else failure_str = NULL;
    port = _port_;
    if (_host_)
    {
        host = new char[strlen(_host_) + 1];
        strcpy(host, _host_);
    }
    else host = NULL;
    stop_keep_alive = false;
    initialized = false;
#endif
}

pping_client::~pping_client()
{
#ifdef PPING_ON
    if (host) delete [] host;
    if (failure_str) delete [] failure_str;
#endif
}

void pping_client::startup(SednaUserException& e)
{
    startup(e, false);
}

void pping_client::startup(SednaUserSoftException& e)
{
    startup(e, true);
}

void pping_client::throw_exception(SednaUserException& e, bool is_soft)
{
    if (is_soft) throw dynamic_cast<SednaUserSoftException&>(e);
    else throw e;
}

void pping_client::startup(SednaUserException& e, bool is_soft)
{
#ifdef PPING_ON
    sock = usocket(AF_INET, SOCK_STREAM, 0);

    if (sock == U_INVALID_SOCKET) throw USER_ENV_EXCEPTION("Failed to create socket", false);
    if (host)
    {
        if (uconnect_tcp(sock, port, host) == U_SOCKET_ERROR) throw_exception(e, is_soft);
            //throw USER_ENV_EXCEPTION("Failed to create TCP connection", false);
    }
    else
    {
//        std::string local_host;
//        if (ulocalhost(&local_host) == U_SOCKET_ERROR)
//            throw USER_ENV_EXCEPTION("Failed to obtain local host name", false);
        if (uconnect_tcp(sock, port, "127.0.0.1") == U_SOCKET_ERROR) throw_exception(e, is_soft);
            //throw USER_ENV_EXCEPTION("Failed to create TCP connection", false);
    }

    if (UUnnamedSemaphoreCreate(&sem, 0) != 0)
        throw USER_ENV_EXCEPTION("Failed to create semaphore", false);


    uResVal res = uCreateThread(pping_client_thread_proc, this, &client_thread_handle, PPING_STACK_SIZE);
    if (res != 0) throw USER_ENV_EXCEPTION("Failed to create pping client thread", false);

    initialized = true;
#endif
}

void pping_client::shutdown()
{
#ifdef PPING_ON
    if (!initialized) return;

    stop_keep_alive = true;
    if (UUnnamedSemaphoreUp(&sem) != 0)
        throw USER_ENV_EXCEPTION("Failed to up semaphore", false);

    if (uThreadJoin(client_thread_handle) != 0)
        throw USER_ENV_EXCEPTION("Error waiting for pping client thread to shutdown", false);

    if (uCloseThreadHandle(client_thread_handle) != 0)
        throw USER_EXCEPTION2(SE4063, "pping client_thread");

    char c = PPING_DISCONNECT_MSG;
    if (usend(sock, &c, sizeof(char)) != sizeof(char))
        throw SYSTEM_EXCEPTION("pping server is down");
    if (uclose_socket(sock) == U_SOCKET_ERROR)
        throw USER_ENV_EXCEPTION("Failed to close socket", false);

    if (UUnnamedSemaphoreRelease(&sem) != 0)
        throw USER_ENV_EXCEPTION("Failed to release semaphore", false);
    
#endif
}



////////////////////////////////////////////////////////////////////////////////
/// pping_server
////////////////////////////////////////////////////////////////////////////////

struct pping_serv_arg
{
    pping_server *pps;
    USOCKET sock;
    int id; // thread_table id
};


U_THREAD_PROC(pping_server_cli_thread_proc, arg)
{
    if (uThreadBlockAllSignals() != 0)
        d_printf1("Failed to block signals for SSMMsg_server_proc");

    pping_server *pps = ((pping_serv_arg*)arg)->pps;
    USOCKET sock = ((pping_serv_arg*)arg)->sock;
    int id = ((pping_serv_arg*)arg)->id;
    delete ((pping_serv_arg*)arg);

    char c = PPING_DISCONNECT_MSG;
    while (true)
    {
        if (urecv(sock, &c, sizeof(c)) != sizeof(c)) goto sys_failure;
        if (c == PPING_DISCONNECT_MSG) break;
        if (c != PPING_KEEP_ALIVE_MSG) goto sys_failure;
    }

    //d_printf1("pping_server's client is closed\n");
    if (uclose_socket(sock) == U_SOCKET_ERROR) goto sys_failure;

    pps->thread_table[id].is_running = false;
    return 0;

sys_failure:
    if (pps->failure_str) printf("%s\n", pps->failure_str);
    else printf("System failure\n");
    printf("pping_server_cli+thread_proc\n");
    d_flush();
    uExitProcess(1);

    return 0;
}

U_THREAD_PROC(pping_server_lstn_thread_proc, arg)
{
    if (uThreadBlockAllSignals() != 0)
        d_printf1("Failed to block signals for SSMMsg_server_proc");

    pping_server *pps = (pping_server*)arg;
    int i = 0;

    while (true)
    {
        pping_serv_arg *pps_arg = new pping_serv_arg;
           
        //accept a call from a client
        pps_arg->pps = pps;
        pps_arg->id = -1;
        pps_arg->sock = uaccept(pps->sock);

        if (pps_arg->sock == U_INVALID_SOCKET || pps->close_lstn_thread) 
        {
            if (pps->close_lstn_thread) 
            {
                for (i = 0; i < PPING_SERVER_THREAD_TABLE_SIZE; ++i)
                {
                    if (!(pps->thread_table[i].is_empty))
                    {
                        if (uThreadJoin(pps->thread_table[i].handle) != 0)
                            goto sys_failure;
                        if (uCloseThreadHandle(pps->thread_table[i].handle) != 0)
                            goto sys_failure;

                        pps->thread_table[i].handle = (UTHANDLE)0;
                        pps->thread_table[i].is_running = true;
                        pps->thread_table[i].is_empty = true;
                    }
                }

                return 0;
            }
            else goto sys_failure;
        }

        for (i = 0; i < PPING_SERVER_THREAD_TABLE_SIZE; ++i)
        {
            if (!(pps->thread_table[i].is_empty) &&
                !(pps->thread_table[i].is_running))
            {
                if (uThreadJoin(pps->thread_table[i].handle) != 0)
                    goto sys_failure;
                if (uCloseThreadHandle(pps->thread_table[i].handle) != 0)
                    goto sys_failure;

                pps->thread_table[i].handle = (UTHANDLE)0;
                pps->thread_table[i].is_running = true;
                pps->thread_table[i].is_empty = true;
            }

            if (pps_arg->id == -1 && pps->thread_table[i].is_empty)
                pps_arg->id = i;
        }

        pps->thread_table[pps_arg->id].is_running = true;
        pps->thread_table[pps_arg->id].is_empty = false;
        uResVal res = uCreateThread(pping_server_cli_thread_proc, 
                                    pps_arg, 
                                    &(pps->thread_table[pps_arg->id].handle), 
                                    PPING_STACK_SIZE);
        if (res != 0) goto sys_failure;
    }
    return 0;

sys_failure:
    if (pps->failure_str) printf("%s\n", pps->failure_str);
    else printf("System failure\n");
    printf("pping_server_lstn_thread_proc\n");
    uExitProcess(1);

    return 0;
}

pping_server::pping_server(const char *_failure_str_, int _port_)
{
#ifdef PPING_ON
    if (_failure_str_)
    {
        failure_str = new char[strlen(_failure_str_) + 1];
        strcpy(failure_str, _failure_str_);
    }
    else failure_str = NULL;
    port = _port_;
    close_lstn_thread = false;
    initialized = false;

    for (int i = 0; i < PPING_SERVER_THREAD_TABLE_SIZE; ++i)
    {
        thread_table[i].handle = (UTHANDLE)0;
        thread_table[i].is_running = true;
        thread_table[i].is_empty = true;
    }
#endif
}

pping_server::~pping_server()
{
#ifdef PPING_ON
    if (failure_str) delete [] failure_str;
#endif
}

void pping_server::startup()
{
#ifdef PPING_ON
    sock = usocket(AF_INET, SOCK_STREAM, 0);
    if (sock == U_INVALID_SOCKET) throw USER_ENV_EXCEPTION("Failed to create socket", false);

    if (ubind_tcp(sock, port) == U_SOCKET_ERROR) {printf(usocket_error_translator());throw USER_ENV_EXCEPTION("Failed to bind socket", false);}

    if (ulisten(sock, PPING_LSTNR_QUEUE_LEN) == U_SOCKET_ERROR) throw USER_ENV_EXCEPTION("Failed to listen socket", false);

    uResVal res = uCreateThread(pping_server_lstn_thread_proc, this, &server_lstn_thread_handle, PPING_STACK_SIZE);
    if (res != 0) throw USER_ENV_EXCEPTION("Failed to create pping server thread", false);
    initialized = true;
#endif
}

void pping_server::shutdown()
{
#ifdef PPING_ON
    if (!initialized) return;

    close_lstn_thread = true;

    // send closing message: begin
    USOCKET s = usocket(AF_INET, SOCK_STREAM, 0);
    if (s == U_INVALID_SOCKET) 
        throw USER_ENV_EXCEPTION("Failed to create socket", false);

    if (uconnect_tcp(s, port, "127.0.0.1") == U_SOCKET_ERROR) 
        throw USER_ENV_EXCEPTION("Failed to create TCP connection", false);

    char c = PPING_KEEP_ALIVE_MSG;
    usend(s, &c, sizeof(c));

    if (uclose_socket(s) != 0)
        throw USER_ENV_EXCEPTION("Failed to close socket", false);
    // send closin message: end

    if (uclose_socket(sock) != 0)
        throw USER_ENV_EXCEPTION("Failed to close socket", false);

    if (uThreadJoin(server_lstn_thread_handle) != 0)
        throw USER_ENV_EXCEPTION("Error waiting for pping server_lstn thread to shutdown", false);

    if (uCloseThreadHandle(server_lstn_thread_handle) != 0)
        throw USER_EXCEPTION2(SE4063, "pping server_lstn_thread");


#endif
}

