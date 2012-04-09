/*
 * File:  SSMMsg.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <assert.h>

#include "SSMMsg.h"

#include "common/errdbg/d_printf.h"
#include "u/ugnames.h"

using namespace std;

// SSMMsg stands for Simple Shared Memory Messages Client/Server Architecture



#define SSMMSG_PAGE_SIZE						4096
#define MESSAGES_IN_QUEUE						100

#define PROCESS_METHOD_THREAD_STACK_SIZE        (1024 * 100)

#define SSMMSG_SHMEM_SUFFIX						"shmem"
#define SSMMSG_SEMS_SUFFIX						"sems"


#define sem_clients								0
#define sem_servers								1
#define sem_mutex								2
#define sem_pact								3
#define sem_pact_written						4
#define sem_pact_read							5

#define NAMED_SEMS_NUM							6

#define sem_data_written						NAMED_SEMS_NUM
#define sem_data_processed						(NAMED_SEMS_NUM + servers_amount)
#define sem_data_read							(NAMED_SEMS_NUM + 2 * servers_amount)

#define sem_data_processed2(s)					(NAMED_SEMS_NUM + (s)->servers_amount)
#define sem_data_read2(s)						(NAMED_SEMS_NUM + 2 * (s)->servers_amount)

struct SSMMsgNames
{
	global_name memory;
	global_name sems;
};

static void InitSSMMsgNames(SSMMsgNames *names, global_name name, char *buf, size_t bufSz)
{
	assert(names);
	if ((names->memory = UGlobalNameFromCompoundName(name, 0, buf, bufSz)))
	{
		size_t consumed = strlen(names->memory)+1;
		buf+=consumed; bufSz-=consumed;
	}
	names->sems = UGlobalNameFromCompoundName(name, 0, buf, bufSz);
}

SSMMsg::SSMMsg(mode _m_, 
               int _msg_size_, 
               global_name _g_name_, 
               int _servers_amount_, 
               unsigned int _millisec_)
{
    m = _m_;
    msg_size = _msg_size_;
    servers_amount = _servers_amount_;

    real_block_size = 0;
    shared_memory_size = 0;
    waiting = NULL;
    slot = NULL;
    process_messages = NULL;
    busy_servers_amount = NULL;
    shar_mem = NULL;
    buf_addr = NULL;
    sysinf_addr = NULL;
    sems_num = NAMED_SEMS_NUM + 3 * servers_amount;

	SSMMsgNames names = {};
	InitSSMMsgNames(&names, _g_name_, g_names__buf__, 256);
    g_name_shmem = names.memory;
    g_name_sems = names.sems;

    server_param = NULL;

    millisec = _millisec_;

    thread_handles = NULL;
    sems = 0;


    real_block_size = msg_size % SSMMSG_PAGE_SIZE == 0 ? msg_size : (msg_size / SSMMSG_PAGE_SIZE + 1) * SSMMSG_PAGE_SIZE;
    // the idea is that first page is for system purposes (it contains system 
    // data)
    shared_memory_size = SSMMSG_PAGE_SIZE + real_block_size * servers_amount;

    shutdown_server_proc = false;
}


int SSMMsg::init()
{
    //d_printf1("!!!!!!!! SSMMsg::init() !!!!!!!\n");

    if (m == Server)
    {
        if (0 != uCreateShMem(&sh_mem, g_name_shmem, shared_memory_size, NULL, __sys_call_error))
        {
            d_printf1("uCreateShMem failed\n");
            return 1;
        }

        shar_mem = uAttachShMem(&sh_mem, NULL, 0, __sys_call_error);
        if (shar_mem == NULL)
        {
            d_printf1("uAttachShMem failed\n");
            return 1;
        }

        //d_printf2("shar_mem 0x%x\n", shar_mem);
        //d_printf2("shared_memory_size %d\n", shared_memory_size);

        int *init_values = new int[sems_num];

        init_values[sem_clients]      = 0;
        init_values[sem_servers]      = 0;
        init_values[sem_mutex]        = 1;
        init_values[sem_pact]         = 1;
        init_values[sem_pact_written] = 0;
        init_values[sem_pact_read]    = 0;
        int i = 0;
        for (i = NAMED_SEMS_NUM; i < sems_num; i++)
            init_values[i] = 0;

        if (0 != USemaphoreArrCreate(&sems, sems_num, init_values, g_name_sems, NULL, __sys_call_error))
        {
            d_printf1("USemaphoreArrCreate failed\n");
            return 1;
        }

        delete [] init_values;


        sysinf_addr = shar_mem;
        buf_addr = (void*)((char*)shar_mem + SSMMSG_PAGE_SIZE);

        waiting = (int*)sysinf_addr;
        slot = (int*)sysinf_addr + 1;
        process_messages = (int*)sysinf_addr + 2;
        busy_servers_amount = (int*)sysinf_addr + 3;

        *waiting = 0;
        *slot = 0;
        *process_messages = 1;
        *busy_servers_amount = 0;
    }
    else if (m == Client)
    {
        //d_printf1("!!!!!!!!!!! Client initialization !!!!!!!!!!!!!!!!!!!!!!!!!\n");

        if (0 != uOpenShMem(&sh_mem, g_name_shmem, __sys_call_error))
        {
            d_printf1("uOpenShMem failed\n");
            return 1;
        }

        shar_mem = uAttachShMem(&sh_mem, NULL, 0, __sys_call_error);
        if (shar_mem == NULL)
        {
            d_printf1("uAttachShMem failed\n");
            return 1;
        }

        //d_printf2("shar_mem 0x%x\n", shar_mem);
        //d_printf2("shared_memory_size %d\n", shared_memory_size);

        if (0 != USemaphoreArrOpen(&sems, sems_num, g_name_sems, __sys_call_error))
        {
            d_printf1("USemaphoreArrOpen failed\n");
            return 1;
        }

        sysinf_addr = shar_mem;
        buf_addr = (void*)((char*)shar_mem + SSMMSG_PAGE_SIZE);

        waiting = (int*)sysinf_addr;
        slot = (int*)sysinf_addr + 1;
        process_messages = (int*)sysinf_addr + 2;
        busy_servers_amount = (int*)sysinf_addr + 3;
    }
    else return 1;

    return 0;
}


int SSMMsg::shutdown()
{
    if (0 != uDettachShMem(&sh_mem, shar_mem, __sys_call_error))
    {
        d_printf1("uDettachShMem failed\n");
        return 1;
    }

    if (m == Server)
    {
        if (0 != uReleaseShMem(&sh_mem, g_name_shmem, __sys_call_error))
        {
            d_printf1("uReleaseShMem failed\n");
            return 1;
        }

        if (0 != USemaphoreArrRelease(sems, sems_num, __sys_call_error))
        {
            d_printf1("USemaphoreArrRelease failed\n");
            return 1;
        }
    }
    else if (m == Client)
    {
        if (0 != uCloseShMem(&sh_mem, __sys_call_error))
        {
            d_printf1("uCloseShMem failed\n");
            return 1;
        }

        if (0 != USemaphoreArrClose(sems, sems_num, __sys_call_error))
        {
            d_printf1("USemaphoreArrClose failed\n");
            return 1;
        }
    }
    else return 1;

    delete server_param;
    delete [] thread_handles;

    return 0;
}


#define down1(sems, idx, ret)        if (0 != USemaphoreArrDown(sems, idx, __sys_call_error))					\
                                     {														\
                                         d_printf1("USemaphoreArrDown1 failed\n");			\
                                         return ret;										\
                                     }

#define down2(sems, idx, ms, ret)    if (0 != USemaphoreArrDown(sems, idx/*, ms*/, __sys_call_error))			\
                                     {														\
                                         d_printf1("USemaphoreArrDown2 failed\n");			\
                                         return ret;										\
                                     }

#define up(sems, idx, ret)           if (0 != USemaphoreArrUp(sems, idx, __sys_call_error))					\
                                     {														\
                                         d_printf1("USemaphoreArrUp failed\n");				\
                                         return ret;										\
                                     }


int SSMMsg::send_msg(void * buf)
{
//    static int i = 0;

//    d_printf2("send_msg called %d times\n", ++i); fflush(stdout);

    int server_num = 0;

//    d_printf2("down sem_mutex %d\n", (int)sem_mutex); fflush(stdout);
    down2(sems, sem_mutex, millisec, 1); 
//    d_printf2("down sem_mutex %d\n", (int)sem_mutex); fflush(stdout);
    if (*waiting < MESSAGES_IN_QUEUE && *process_messages == 1)
    {
        *waiting += 1;

//        d_printf2("*waiting = %d\n", *waiting); fflush(stdout);
//        d_printf2("*process_messages = %d\n", *process_messages); fflush(stdout);

        up(sems, sem_clients, 1);
        up(sems, sem_mutex, 1);
//        d_printf1("down sem_servers\n"); fflush(stdout);
        down2(sems, sem_servers, millisec, 1);

//        d_printf1("down sem_pact_written\n"); fflush(stdout);
        down2(sems, sem_pact_written, millisec, 1);
        server_num = *slot;
        up(sems, sem_pact_read, 1);

//        d_printf2("server_num %d\n", server_num); fflush(stdout);
//        d_printf2("cell 0x%x\n", (void*)((char*)buf_addr + server_num * real_block_size)); fflush(stdout);
        memcpy((void*)((char*)buf_addr + server_num * real_block_size), buf, msg_size);
        up(sems, sem_data_written + server_num, 1);
//        d_printf2("down sem_data_processed[%d]\n", server_num); fflush(stdout);
        down2(sems, sem_data_processed + server_num, millisec, 1);
//        d_printf1("after down sem_data_processed\n");
        memcpy(buf, (void*)((char*)buf_addr + server_num * real_block_size), msg_size);
//        d_printf2("up sem_data_read[%d]\n", server_num); fflush(stdout);
        up(sems, sem_data_read + server_num, 1);

//        d_printf1("client: msg sent\n"); fflush(stdout);
    }
    else 
    {
//        if (!(*waiting < MESSAGES_IN_QUEUE)) d_printf2("Deny of service: server is busy (queue is too long) %d\n", *waiting);
//        if (!(*process_messages == 1)) d_printf2("Deny of service: server is being shutdown %d\n", *process_messages);
        up(sems, sem_mutex, 1);
//        d_printf1("Deny of service: server is busy or is being shutdown\n");
        return 1;
    }

    return 0;
}

#ifdef _WIN32
#define THREAD_FUN_RET_TYPE		DWORD
#else
#define THREAD_FUN_RET_TYPE		void*
#endif


U_THREAD_PROC(SSMMsg_server_proc, arg)
{
    if (uThreadBlockAllSignals(__sys_call_error) != 0)
        d_printf1("Failed to block signals for SSMMsg_server_proc");

    int i = ((SSMMsg_server_thread_param*)arg)->i;
    process_msg_func func = ((SSMMsg_server_thread_param*)arg)->func;
    SSMMsg *ssmmsg = ((SSMMsg_server_thread_param*)arg)->ssmmsg;

    while (true)
    {
        //d_printf1("cycle iteration\n"); fflush(stdout);
        up(ssmmsg->sems, sem_servers, (THREAD_FUN_RET_TYPE)-1);
        down1(ssmmsg->sems, sem_clients, (THREAD_FUN_RET_TYPE)-1);
        if (ssmmsg->shutdown_server_proc) break;
        down1(ssmmsg->sems, sem_mutex, (THREAD_FUN_RET_TYPE)-1);
        *(ssmmsg->waiting) -= 1;
        *(ssmmsg->busy_servers_amount) += 1;
        up(ssmmsg->sems, sem_mutex, (THREAD_FUN_RET_TYPE)-1);

        down1(ssmmsg->sems, sem_pact, (THREAD_FUN_RET_TYPE)-1);
        *(ssmmsg->slot) = i;
        up(ssmmsg->sems, sem_pact_written, (THREAD_FUN_RET_TYPE)-1);
        down1(ssmmsg->sems, sem_pact_read, (THREAD_FUN_RET_TYPE)-1);
        up(ssmmsg->sems, sem_pact, (THREAD_FUN_RET_TYPE)-1);

        down1(ssmmsg->sems, sem_data_written + i, (THREAD_FUN_RET_TYPE)-1);

        // user activity
        func((void*)((char*)(ssmmsg->buf_addr) + i * ssmmsg->real_block_size));

//      d_printf2("up sem_data_processed[%d]\n", i); fflush(stdout);
        up(ssmmsg->sems, sem_data_processed2(ssmmsg) + i, (THREAD_FUN_RET_TYPE)-1);
//      d_printf2("down sem_data_read[%d]\n", i); fflush(stdout);
        down1(ssmmsg->sems, sem_data_read2(ssmmsg) + i, (THREAD_FUN_RET_TYPE)-1);

        down1(ssmmsg->sems, sem_mutex, (THREAD_FUN_RET_TYPE)-1);
        *(ssmmsg->busy_servers_amount) -= 1;
        up(ssmmsg->sems, sem_mutex, (THREAD_FUN_RET_TYPE)-1);
    }

    return 0;
}

int SSMMsg::serve_clients(process_msg_func func)
{
    shutdown_server_proc = false;

    server_param = new SSMMsg_server_thread_param;
    server_param->func = func;
    server_param->ssmmsg = this;
    thread_handles = new UTHANDLE[servers_amount];
    for (int i = 0; i < servers_amount; i++)
    {
        //d_printf2("server thread number %d started\n", i);
        UTHANDLE id;
        server_param->i = i;
        uResVal res = uCreateThread(SSMMsg_server_proc, server_param, &id, PROCESS_METHOD_THREAD_STACK_SIZE, NULL, __sys_call_error);
        if (res != 0) 
        {
            d_printf1("Failed to create thread\n");
            return 1;
        }
        thread_handles[i] = id;
    }

    return 0;
}

int SSMMsg::stop_serve_clients()
{
    down1(sems, sem_mutex, 1); 
    *process_messages = 0;
    up(sems, sem_mutex, 1);

    while (true)
    {
        down1(sems, sem_mutex, 1); 
        if (*waiting == 0 && *busy_servers_amount == 0)
        {
            int i = 0;

            up(sems, sem_mutex, 1);

            shutdown_server_proc = true;
            for (i = 0; i < servers_amount; i++)
                up(sems, sem_clients, 1);

            for (i = 0; i < servers_amount; i++)
            {
                if (uThreadJoin(thread_handles[i], __sys_call_error) != 0)
                {
                    d_printf1("uThreadJoin failed\n");
                    return 1;
                }

                if (uCloseThreadHandle(thread_handles[i], __sys_call_error) != 0)
                {
                    d_printf1("uCloseThreadHandle failed\n");
                    return 1;
                }
            }

            break;
        }
        else up(sems, sem_mutex, 1);

        uSleep(1, __sys_call_error);
    }

    return 0;
}

void SSMMsg::ipc_cleanup(global_name name)
{
	USemaphoreArr sems;
	UShMem memory;
	SSMMsgNames names;
	char buf[256];

	InitSSMMsgNames(&names, name, buf, sizeof buf);
	if (uOpenShMem(&memory, names.memory,  __sys_call_error) == 0)
	{
		uReleaseShMem(&memory, names.memory, __sys_call_error);
	}	
	if (USemaphoreArrOpen(&sems, 9, names.sems, __sys_call_error) == 0)
	{
		USemaphoreArrRelease(sems, 9, __sys_call_error);
	}
}

