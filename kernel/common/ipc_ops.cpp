/*
 * File:  ipc_ops.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "common/ipc_ops.h"
#include "common/base.h"
#include "common/u/usocket.h"
#include "common/errdbg/d_printf.h"


int WriteHead(UPIPE p, int *cmd, int *len)
{
   int res = uWritePipeAll(p, cmd, sizeof(int), __sys_call_error);
   if(res < 0) 
   {
     d_printf1("Pipe error\n");
     return -1;
   }

   res = uWritePipeAll(p, len, sizeof(int), __sys_call_error);
   if(res < 0) 
   {
      d_printf1("Pipe error\n");
      return -1;
   }
   return 0;
}

int ReadHead(UPIPE p, int *cmd, int *len)
{
   int res = uReadPipeAll(p, cmd, sizeof(int), __sys_call_error);
   if (res < 0) 
   {
      d_printf1("Pipe error\n");
      return -1;
   }

   res = uReadPipeAll(p, len, sizeof(int), __sys_call_error);
   if (res < 0) 
   {
      d_printf1("Pipe error\n");
      return -1;
   }

   return 0;
}


void* open_gov_shm(UShMem *gov_shm_service_dsc)
{
   void* gov_shared_mem;

   if (0 != uOpenShMem(gov_shm_service_dsc,
                       GOVERNOR_SHARED_MEMORY_NAME,
                       GOV_SHM_SIZE,
                       __sys_call_error
                      ))
      throw USER_EXCEPTION(SE4400);


   gov_shared_mem = uAttachShMem(*gov_shm_service_dsc,
                                 NULL,
                                 GOV_SHM_SIZE,
                                 __sys_call_error
                                );

   if (gov_shared_mem == NULL)
      throw USER_EXCEPTION2(SE4023, "GOVERNOR_SHARED_MEMORY_NAME");

   return gov_shared_mem;

}


int close_gov_shm(UShMem gov_shm_service_dsc, void* gov_shared_mem)
{

  if ( 0 != uDettachShMem(gov_shm_service_dsc, gov_shared_mem, __sys_call_error))
     return -1;

  if ( 0 != uCloseShMem(gov_shm_service_dsc, __sys_call_error))
     return -1;

  return 0;

}

void send_command_to_gov(int port_number, int cmd)
{
  USOCKET s;
  int rc;
  char *ptr;
  __int32 tmp;
    
  s = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);

  if (uconnect_tcp(s, port_number, "127.0.0.1", __sys_call_error) == 0)
  {
  	 tmp = htonl(cmd);
     ptr = (char*) &(tmp);	
     rc = 0;
     while(rc < 4)
     {
     	 rc += usend(s, ptr+rc, 4-rc, __sys_call_error);
     }
     rc = 0;
     ptr = (char*) &(rc);	
     while(rc < 4)
     {
     	 rc += usend(s, ptr+rc, 4-rc, __sys_call_error);
     }
     ushutdown_close_socket(s, __sys_call_error);
  }
  else
  	 d_printf2("SOCKET ERROR: %s\n",usocket_error_translator());
}

