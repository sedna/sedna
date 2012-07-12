/*
 * File:  SSMMsg.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __SSMMsg_H
#define __SSMMsg_H

// SSMMsg stands for Simple Shared Memory Messages Client/Server Architecture

#include <string>

#include "common/sedna.h"

#include "u/usem.h"
#include "u/uthread.h"
#include "u/ushm.h"

#define SSMMSG_DEFAULT_TIMEOUT		10000



// function that serves client msg on the server side
// it has only arg - addres of block with input data; output data must be 
// written at the same block
typedef int (*process_msg_func)(void *);

class SSMMsg;

struct SSMMsg_server_thread_param
{
    process_msg_func func;
    SSMMsg *ssmmsg;
    int i;
};


class SSMMsg
{
    friend U_THREAD_PROC(SSMMsg_server_proc, arg);

public: 
    // type mode for SSMMsg object
    enum mode {Server, Client};

private:
    mode m;
    int msg_size; 
    int servers_amount;

    int real_block_size;
    int shared_memory_size;
    int *waiting;
    int *slot;
    int *process_messages; // is server accept messages (1 - accept, 0 - don't accept)
    int *busy_servers_amount; // amount of the busy servers
    void *shar_mem;
    void *buf_addr;
    void *sysinf_addr;
    int sems_num;

    char g_name_shmem[128];
    char g_name_sems[128];

    SSMMsg_server_thread_param *server_param;

    unsigned int millisec;

    UTHANDLE *thread_handles;
    UShMem sh_mem;
    USemaphoreArr sems;
/*
    Semaphore array sems consists of the following semaphores:
    semaphore            initial value
    ----------------------------------
    sem_clients				0
    sem_servers				0
    sem_mutex				1

    sem_pact				1
    sem_pact_written		0
    sem_pact_read			0

    sem_data_written[]		0
    sem_data_processed[]	0
    sem_data_read[]			0
*/

    bool shutdown_server_proc;

public:

    // initializing SSMMsg object
    // constructor has four parameters:
    // m - mode, in which object should work
    // msg_size - size for memory region that will store intermediate data (size
    // must be specified in bytes, but notice that it will be round up
    // to page size (4Kb on x86)) 
    // shared_memory_name - name of shared memory region (unique name in the 
    // system) 
    // servers_amount - amount of server threads
    // msg_size, shared_memory_name, servers_amount must be the same at the 
    // server side and at all clients (system doesn't control it, so be 
    // careful)
    SSMMsg(mode _m_, 
           int _msg_size_, 
           global_name _g_name_, 
           int _servers_amount_, 
           unsigned int _millisec_ = SSMMSG_DEFAULT_TIMEOUT);


    // for all functions below assumed that they return 0 as success and 1 otherwise

    // init object (call it before using object)
    int init();

    // shutdown object (call it to free resources)
    int shutdown();

    // send msg to server and wait for answer (this is blocking operation)
    // buf contains data sending to server and it will contains result if function
    // completes successfully (assumes that size of buf is msg_size)
    int send_msg(void * buf);

    // starts serving clients 
    // it has only one arg - a function that processing msg
    int serve_clients(process_msg_func func);

    // stops accept new messages and wait until all processing messages finish
    int stop_serve_clients();

    static void ipc_cleanup(global_name name);
};


#endif

