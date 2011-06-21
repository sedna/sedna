/*
 * File:  pping.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "common/pping.h"
#include "common/errdbg/d_printf.h"
#include "common/ipc_ops.h"

#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
#include "common/st/stacktrace.h"
#include "common/u/uhdd.h"
#ifdef _WIN32
#include <dbghelp.h>
#include <shellapi.h>
#include <shlobj.h>
#else
#include <string.h>
#endif /* _WIN32 */
#endif

#define PPING_STACK_SIZE             102400

#define PPING_KEEP_ALIVE_MSG	     'a'
#define PPING_DISCONNECT_MSG	     'b'
#define PPING_PROC_EXCEPTION_MSG     'e'

#define PPING_LSTNR_QUEUE_LEN        100


#define SYS_FAILURE_SERVER( message )     { sedna_soft_fault(message, pps->component); \
                                            return 0; }

#define SYS_FAILURE_CLIENT( message )     { sedna_soft_fault(message, ppc->component); \
                                            return 0; }


////////////////////////////////////////////////////////////////////////////////
/// pping_client
////////////////////////////////////////////////////////////////////////////////

U_THREAD_PROC(pping_client_thread_proc, arg)
{
    if (uThreadBlockAllSignals(NULL) != 0)
        d_printf1("Failed to block signals for pping_client_thread_proc");

    pping_client *ppc = (pping_client*)arg;
    char c = PPING_KEEP_ALIVE_MSG;
    
     while (true)
     {
        if (ppc->stop_keep_alive) return 0;
        
        int res = usend(ppc->sock, &c, sizeof(c), NULL);

        if (res != sizeof(c) && !ppc->stop_keep_alive) {
            sedna_soft_fault("SEDNA GOVERNOR is down", ppc->component);
        }
        
        /* se_stop -hard has been called? */
        if(ppc->signaled_flag    != NULL  && 
           GOV_HEADER_GLOBAL_PTR != NULL  && 
           GOV_HEADER_GLOBAL_PTR -> is_server_stop == SE_STOP_HARD)
        {
            *(ppc->signaled_flag) = true;
        }
        
        UUnnamedSemaphoreDownTimeout(&(ppc->sem), 1000, NULL);

        if(ppc->timeout)
        {

            if (!ppc->counter && !ppc->reset_flag) {*(ppc->signaled_flag) = true; ppc->counter = ppc->timeout;}
  
            if(ppc->reset_flag)
            {
                ppc->counter = ppc->timeout;
                if(ppc->signaled_flag) *(ppc->signaled_flag) = false;
                ppc->reset_flag = false;
            }

            (ppc->counter)--;
        }
#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
#define SENDVAR(v) if (usend(ppc->sock, (char*)&v, sizeof(v), NULL) != sizeof(v)) continue;
#ifdef _WIN32
		if (ppc->exceptPtrs)
		{
			//TODO: do something if usend/urecv fails or get wrong msg
			char cc = PPING_PROC_EXCEPTION_MSG;
			DWORD proc_id = GetCurrentProcessId();

			if (usend(ppc->sock, &cc, sizeof(cc), NULL) != sizeof(cc))
				{ ppc->exceptPtrs = NULL; continue; }

			SENDVAR(proc_id)
			SENDVAR(ppc->component);
			SENDVAR(ppc->except_thread_id);
			SENDVAR(ppc->exceptPtrs);

			if (urecv(ppc->sock, &cc, sizeof(cc), NULL) != sizeof(cc))
				{ ppc->exceptPtrs = NULL; continue; }
			if (cc != PPING_PROC_EXCEPTION_MSG)
				{ ppc->exceptPtrs = NULL; continue; }

			if (ppc->stacktrace_fh != U_INVALID_FD)
			{
				if (StackTraceInit() != 0)
				{
					StackTraceWriteFd(ppc->exceptPtrs->ContextRecord, (intptr_t)ppc->stacktrace_fh, 9999, 0);
					
					StackTraceDeinit();
				}
				uCloseFile(ppc->stacktrace_fh, NULL);
			}

			
			ppc->exceptPtrs = NULL;
		}
#endif
#undef SENDVAR
#endif
    }
    return 0;
}

pping_client::pping_client(int _port_, int _component_, const char* _host_)
{
    port = _port_;
    if (_host_ && strlen(_host_) < U_MAX_HOSTNAME)
        strcpy(host, _host_);
    else
        strcpy(host, "localhost");
	component = _component_;
	
    stop_keep_alive = false;
    initialized = false;

    counter = 0;
    timeout = 0;
    reset_flag = false;
    signaled_flag = NULL;

#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
#ifdef _WIN32
	exceptPtrs = NULL;
	stacktrace_fh = U_INVALID_FD;
#endif
#endif
}

pping_client::pping_client(int _port_, int _component_, volatile bool* volatile _signaled_flag_, const char* _host_)
{
    port = _port_;
    if (_host_ && strlen(_host_) < U_MAX_HOSTNAME)
        strcpy(host, _host_);
    else
        strcpy(host, "localhost");
	component = _component_;
	
    stop_keep_alive = false;
    initialized = false;

    counter = 0;
    timeout = 0;
    reset_flag = false;
    signaled_flag = _signaled_flag_;

#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
#ifdef _WIN32
	exceptPtrs = NULL;
	stacktrace_fh = U_INVALID_FD;
#endif
#endif
}

pping_client::~pping_client()
{
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
    sock = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);

    if (sock == U_INVALID_SOCKET) throw USER_ENV_EXCEPTION("Failed to create socket", false);
    if (uconnect_tcp(sock, port, host, __sys_call_error) == U_SOCKET_ERROR) 
    {
        if (uclose_socket(sock, NULL) == U_SOCKET_ERROR)
            throw USER_ENV_EXCEPTION("Failed to close socket", false);
        throw_exception(e, is_soft);
    }

    if (UUnnamedSemaphoreCreate(&sem, 0, NULL, __sys_call_error) != 0)
        throw USER_ENV_EXCEPTION("Failed to create semaphore", false);


    uResVal res = uCreateThread(pping_client_thread_proc, this, &client_thread_handle, PPING_STACK_SIZE, NULL, __sys_call_error);
    if (res != 0) throw USER_ENV_EXCEPTION("Failed to create pping client thread", false);

    initialized = true;
}

void pping_client::shutdown()
{
    if (!initialized) return;

    stop_keep_alive = true;
    if (UUnnamedSemaphoreUp(&sem, NULL) != 0)
        throw USER_ENV_EXCEPTION("Failed to up semaphore", false);

    if (uThreadJoin(client_thread_handle, NULL) != 0)
        throw USER_ENV_EXCEPTION("Error waiting for pping client thread to shutdown", false);

    if (uCloseThreadHandle(client_thread_handle, NULL) != 0)
        throw USER_EXCEPTION2(SE4063, "pping client_thread");

    char c = PPING_DISCONNECT_MSG;
    if (usend(sock, &c, sizeof(char), NULL) != sizeof(char))
        throw SYSTEM_EXCEPTION("pping server is down");
    if (uclose_socket(sock, NULL) == U_SOCKET_ERROR)
        throw USER_ENV_EXCEPTION("Failed to close socket", false);

    if (UUnnamedSemaphoreRelease(&sem, NULL) != 0)
        throw USER_ENV_EXCEPTION("Failed to release semaphore", false);

    initialized = false;
}

void pping_client::start_timer(int _timeout_)
{
    
	timeout = _timeout_;
	reset_flag = true;
    if(signaled_flag) *signaled_flag = false;

}

void pping_client::stop_timer()
{
   start_timer(0);
}


#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
///////////////////////////////////////////////////////////////////////////////
/// Exception logging. 
#ifdef _WIN32
void pping_client::WriteStackTraceFile(LPEXCEPTION_POINTERS exceptPtrs)
{
	if (exceptPtrs != NULL)
	{
		int wait = 60;
		this->except_thread_id = GetCurrentThreadId();
		this->stacktrace_fh = sedna_soft_fault_log_fh(this->component, "-st");
		this->exceptPtrs = exceptPtrs;
		while (this->exceptPtrs != NULL && wait > 0)
		{
			if (UUnnamedSemaphoreUp(&sem, NULL) != 0)
				return;
			uSleep(1, NULL);
			wait--;
		}
	}
}
#endif
///////////////////////////////////////////////////////////////////////////////
#endif


///////////////////////////////////////////////////////////////////////////////
/// pping_server
///////////////////////////////////////////////////////////////////////////////

struct pping_serv_arg
{
    pping_server *pps;
    USOCKET sock;
    int id; // thread_table id
};

inline int
client_exception_handler(USOCKET sock, 
                         const pping_server *pps)
{
#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
#ifdef _WIN32
    char cc = PPING_PROC_EXCEPTION_MSG;
    HANDLE proc_h;
    DWORD proc_id;
    DWORD except_thread_id;
    PEXCEPTION_POINTERS client_exceptPtrs;
    MINIDUMP_EXCEPTION_INFORMATION ExpParam;
    int component;
    UFile fh;

#define GETVAR(v) if (urecv(sock, (char*)&v, sizeof(v), NULL) != sizeof(v)) \
                      SYS_FAILURE_SERVER("Failure in pping server (cannot receive exception parameter from the client).");
    GETVAR(proc_id)
    GETVAR(component);
    GETVAR(except_thread_id);
    GETVAR(client_exceptPtrs);
#undef GETVAR			
    
    proc_h = OpenProcess(PROCESS_ALL_ACCESS, FALSE, proc_id);
    if (proc_h == NULL)
        d_printf2("Failed to open process handle, GetLastError=%u\n", GetLastError());

    ExpParam.ThreadId = except_thread_id;
    ExpParam.ExceptionPointers = client_exceptPtrs;
    ExpParam.ClientPointers = TRUE;

    fh = sedna_soft_fault_log_fh(component, "-dump");
    
    if (!MiniDumpWriteDump(proc_h, proc_id, fh, MiniDumpWithDataSegs, &ExpParam, NULL, NULL))
        d_printf2("Failed to save minidump, GetLastError=%u\n", GetLastError());

    uCloseFile(fh, NULL);
    
    if (usend(sock, &cc, sizeof(cc), NULL) != sizeof(cc))
	    SYS_FAILURE_SERVER("Failure in pping server (cannot send exception ack to the client)."); 

#endif /* _WIN32 */
#endif /* EL_DEBUG */

	return 1;
}


U_THREAD_PROC(pping_server_cli_thread_proc, arg)
{
    if (uThreadBlockAllSignals(NULL) != 0)
        d_printf1("Failed to block signals for SSMMsg_server_proc");

    pping_server *pps = ((pping_serv_arg*)arg)->pps;
    USOCKET sock = ((pping_serv_arg*)arg)->sock;
    int id = ((pping_serv_arg*)arg)->id;
    int component = ((pping_serv_arg*)arg)->pps->component;
    delete ((pping_serv_arg*)arg);

    char c = PPING_DISCONNECT_MSG;
    while (true)
    {
        if (urecv(sock, &c, sizeof(c), NULL) != sizeof(c)) goto sys_failure;
        if (c == PPING_DISCONNECT_MSG) break;
#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
		if (c == PPING_PROC_EXCEPTION_MSG)
		{
			if (client_exception_handler(sock, pps) != 1) return 0;
			continue;
		}
#endif
        if (c != PPING_KEEP_ALIVE_MSG) goto sys_failure;
    }

    //d_printf1("pping_server's client is closed\n");
    if (uclose_socket(sock, NULL) == U_SOCKET_ERROR) goto sys_failure;

    pps->thread_table[id].is_running = false;
    return 0;

sys_failure:
    sedna_soft_fault("One of SEDNA processes is down", component);

    return 0;
}

/* Single threaded version of pping server.
 * Uses select() to create connections and serve
 * previously created as well.
 *
 * TODO: read all buffer at once
 * TODO: better failures diagnostric
 * TODO: place accept+uNotInherit into a critical section to prevent 
 *       descriptors inheritance on fork
 *
 */
U_THREAD_PROC(pping_server_lstn_thread_proc_st, arg)
{
    if (uThreadBlockAllSignals(NULL) != 0)
        d_printf1("Failed to block signals for pping server thread.");

    U_SSET allset, rset;
    pping_server *pps = (pping_server*)arg;
    int res;
    char c = PPING_DISCONNECT_MSG;
    bool maxfd_valid = true;
    USOCKET client_sock, maxfd, i;

    U_SSET_ZERO(&allset);
    U_SSET_SET(pps->sock, &allset);
    maxfd = pps->sock + 1;

    while (true)
    {
        rset = allset;
        res  = uselect_read_arr(&rset, maxfd, NULL, NULL);
        if (res == U_SOCKET_ERROR) {
            SYS_FAILURE_SERVER("Failure in pping server (select() call failed).");
        }

        /* Iterate through ready to read client descriptors */
        for (i = 0; i < maxfd; i++)
        {
            if(U_SSET_ISSET(i, &rset) && i != pps->sock)
            {
                res = urecv(i, &c, sizeof(c), NULL);
                if( res != sizeof(c) ) {
                    SYS_FAILURE_SERVER("Failure in pping server (recv() call failed, one of the clients may be down).");
                }
                if (c == PPING_DISCONNECT_MSG) 
                {
                    U_SSET_CLR(i, &allset);
                    if(uclose_socket(i, NULL) == U_SOCKET_ERROR)
                        SYS_FAILURE_SERVER("Failure in pping server (cannot close a socket descriptor on disconnect).");
                    if ( maxfd-1 == i )
                        maxfd_valid = false;
                }
#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
                else if (c == PPING_PROC_EXCEPTION_MSG) {
                    if (client_exception_handler(i, pps) != 1) return 0;
                }
#endif
                else if (c != PPING_KEEP_ALIVE_MSG)
                    SYS_FAILURE_SERVER("Failure in pping server (unexpected message from client).");
            }
        }

        /* Renew maximum descriptor number */
        if( !maxfd_valid )
        {
            for (i = maxfd-1; i >= 0; i--)
            {
                if(U_SSET_ISSET(i, &allset))
                {
                    maxfd = i + 1;
                    maxfd_valid = true;
                    break;
                }
            }
        }
        
        /* Create a new connection */
        if (U_SSET_ISSET(pps->sock, &rset))
        {
            client_sock = uaccept(pps->sock, NULL);
            
            if(client_sock == U_INVALID_SOCKET) 
                SYS_FAILURE_SERVER("Failure in pping server (accept() call failed).");
    
            U_SSET_SET(client_sock, &allset);
            if (client_sock > maxfd-1) {            
                maxfd = client_sock + 1;
            }

            /* Check if governor wants to shutdown us */
            if (pps->close_lstn_thread) 
            {
                for (i = 0; i < maxfd; i++)
                {
                    if(U_SSET_ISSET(i, &allset) && 
                       i != pps->sock && 
                       uclose_socket(i, NULL) == U_SOCKET_ERROR)
                           SYS_FAILURE_SERVER("Failure in pping server (cannot close a socket descriptor).");
                }
                break;
            }

            /* Make the client descriptor unheritable */
            if (uNotInheritDescriptor(UHANDLE(client_sock), NULL) != 0) 
                SYS_FAILURE_SERVER("Failure in pping server (cannot make a socket descriptor unheritable).");
        }
    }
    return 0;
}


pping_server::pping_server(int _port_, int _component_)
{
    port = _port_;
    component = _component_;
    close_lstn_thread = false;
    initialized = false;

    for (int i = 0; i < PPING_SERVER_THREAD_TABLE_SIZE; ++i)
    {
        thread_table[i].handle = (UTHANDLE)0;
        thread_table[i].is_running = true;
        thread_table[i].is_empty = true;
    }
}

pping_server::~pping_server()
{
}

void pping_server::startup()
{
    sock = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);
    if (sock == U_INVALID_SOCKET) throw USER_ENV_EXCEPTION("Failed to create socket", false);
    
    if (uNotInheritDescriptor(UHANDLE(sock), __sys_call_error) != 0) throw USER_EXCEPTION(SE4080);

    if (ubind_tcp(sock, port, "localhost", __sys_call_error) == U_SOCKET_ERROR) throw USER_ENV_EXCEPTION2("Failed to bind socket", usocket_error_translator(), false);

    if (ulisten(sock, PPING_LSTNR_QUEUE_LEN, __sys_call_error) == U_SOCKET_ERROR) throw USER_ENV_EXCEPTION("Failed to listen socket", false);

    uResVal res = uCreateThread(pping_server_lstn_thread_proc_st, this, &server_lstn_thread_handle, PPING_STACK_SIZE, NULL, __sys_call_error);
    if (res != 0) throw USER_ENV_EXCEPTION("Failed to create pping server thread", false);
    initialized = true;
}

void pping_server::shutdown()
{
    if (!initialized) return;

    close_lstn_thread = true;

    USOCKET s = usocket(AF_INET, SOCK_STREAM, 0, NULL);
    if (s == U_INVALID_SOCKET) 
        throw USER_ENV_EXCEPTION("Failed to create socket", false);

    if (uconnect_tcp(s, port, "127.0.0.1", NULL) == U_SOCKET_ERROR) 
        throw USER_ENV_EXCEPTION("Failed to create TCP connection", false);

    char c = PPING_KEEP_ALIVE_MSG;
    usend(s, &c, sizeof(c), NULL);

    if (uThreadJoin(server_lstn_thread_handle, NULL) != 0)
        throw USER_ENV_EXCEPTION("Error waiting for pping server_lstn thread to shutdown", false);

    if (uclose_socket(s, NULL) != 0)
        throw USER_ENV_EXCEPTION("Failed to close socket", false);

    if (uclose_socket(sock, NULL) != 0)
        throw USER_ENV_EXCEPTION("Failed to close socket", false);

    if (uCloseThreadHandle(server_lstn_thread_handle, NULL) != 0)
        throw USER_EXCEPTION2(SE4063, "pping server_lstn_thread");
}

