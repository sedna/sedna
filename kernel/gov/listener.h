#ifndef _LST_H
#define _LST_H
#include "ipc_ops.h"

#include "usocket.h"
#ifdef _WIN32
#else
#include <sys/types.h> 
#include <sys/wait.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#endif

int sess_registering(USOCKET s, char* msg_buf);
int sm_registering(USOCKET s, char* msg_buf);
int client_listener(bool is_background_mode);
void CreateNewSessionProcess(USOCKET socknew, bool is_background_mode);
void send_runtime_config(USOCKET s);
void check_sm_run(USOCKET s, char* msg_buf);

#endif
