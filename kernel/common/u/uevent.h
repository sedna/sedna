#if (_MSC_VER>1000)
#pragma once
#endif

#ifndef UEVENT_H_INCLUDED
#define UEVENT_H_INCLUDED

#include "common/u/u.h"
#include "common/u/usecurity.h"

#ifndef EXTERN_C
#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C
#endif
#endif

#define U_AUTORESET_EVENT				99
#define U_MANUALRESET_EVENT				17

typedef struct UEvent_tag_
{
#ifdef __cplusplus
private:
#endif
#ifdef _WIN32
	HANDLE handle;
#else
	int semid;
#endif
}
UEvent;

EXTERN_C
int UEventUnlink(global_name gn,
				 sys_call_error_fun fun);

EXTERN_C
int UEventCreate(UEvent *uEvent, 
				 USECURITY_ATTRIBUTES* sa,
				 int eventType,
				 int isSet,
				 global_name gn, 
				 sys_call_error_fun fun);

EXTERN_C
int UEventOpen(UEvent *uEvent, 
			   global_name gn, 
			   sys_call_error_fun fun);

EXTERN_C
int UEventClose(UEvent *uEvent,
				sys_call_error_fun fun);

EXTERN_C 
int UEventCloseAndUnlink(UEvent *uEvent,
						 sys_call_error_fun fun);

EXTERN_C
int UEventSet(UEvent *uEvent,
			  sys_call_error_fun fun);

EXTERN_C
int UEventReset(UEvent *uEvent,
				sys_call_error_fun fun);

EXTERN_C
int UEventWait(UEvent *uEvent,
			   sys_call_error_fun fun);

#endif
