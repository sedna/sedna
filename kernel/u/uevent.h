/*
 * File:  uevent.h
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#if (_MSC_VER>1000)
#pragma once
#endif

#ifndef UEVENT_H_INCLUDED
#define UEVENT_H_INCLUDED

#include "u/u.h"
#include "u/usecurity.h"

#define U_AUTORESET_EVENT				99
#define U_MANUALRESET_EVENT				17

typedef struct UEvent_tag_
{
#ifdef _WIN32
	HANDLE handle;
#else
	int semid;
#endif
}
UEvent;

#ifdef __cplusplus
extern "C" {
#endif


int UEventUnlink(global_name gn,
				 sys_call_error_fun fun);

int UEventCreate(UEvent *uEvent, 
				 USECURITY_ATTRIBUTES* sa,
				 int eventType,
				 int isSet,
				 global_name gn, 
				 sys_call_error_fun fun);

int UEventOpen(UEvent *uEvent, 
			   global_name gn, 
			   sys_call_error_fun fun);

int UEventClose(UEvent *uEvent,
				sys_call_error_fun fun);

int UEventCloseAndUnlink(UEvent *uEvent,
						 sys_call_error_fun fun);

int UEventSet(UEvent *uEvent,
			  sys_call_error_fun fun);

int UEventReset(UEvent *uEvent,
				sys_call_error_fun fun);

int UEventWait(UEvent *uEvent,
			   sys_call_error_fun fun);

#ifdef __cplusplus
}
#endif


#endif
