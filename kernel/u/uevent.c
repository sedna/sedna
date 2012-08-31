/*
 * File:  uevent.c
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <assert.h>

#include "uevent.h"

#include "u/usecurity.h"
#include "u/ugnames.h"

#include "common/errdbg/d_printf.h"

#ifdef _WIN32

/* Windows implementation is straight-forward: we create a really thin
 * wrapper around native Win events. Since explicit unlinking of event
 * object is unsupported (the object is destroyed when the last handle
 * is closed) the UnlinkXXX functions aren't fully implemented.
 * Particulary UEventUnlink does nothing except checking for the
 * valid object name (NULL is rejected), and UEventCloseAndUnlink
 * just calls UEventClose. */

int UEventUnlink(global_name gn, sys_call_error_fun fun)
{
	int status = -1;
	if (gn == NULL)
	{
		d_printf1("UEventUnlink: NULL is invalid in this context\n");
	}
	else
	{
		status = 0;
	}
	return status;
}

int UEventCreate(UEvent *uEvent, 
				 USECURITY_ATTRIBUTES* sa,
				 int eventType,
				 int isSet,
				 global_name gn, 
				 sys_call_error_fun fun)
{
	int status = -1; 
        GLOBAL_NAME_BUFFER_DECL(objectName);
	HANDLE handle = NULL;

	assert(uEvent);
    UGetNameFromGlobalName(gn, objectName, sizeof objectName);
	if (eventType!=U_AUTORESET_EVENT && eventType!=U_MANUALRESET_EVENT)
	{
		d_printf1("UEventCreate: unrecognised event type requested\n");
	}
	else if (NULL == (handle=CreateEvent(sa, eventType==U_MANUALRESET_EVENT, isSet, objectName)))
	{
		SYS_CALL_ERROR(fun, "CreateEvent");
	}
	else
	{
		status = 0;
	}
	uEvent->handle = handle;
	return status;
}

int UEventOpen(UEvent *uEvent, 
			   global_name gn,
			   sys_call_error_fun fun)
{
	int status = -1;
        GLOBAL_NAME_BUFFER_DECL(objectName);
	HANDLE handle = NULL;

	assert(uEvent);
        UGetNameFromGlobalName(gn, objectName, sizeof objectName);
	if (NULL == (handle=OpenEvent(EVENT_ALL_ACCESS, FALSE, objectName)))
	{
		SYS_CALL_ERROR(fun, "OpenEvent");
	}
	else
	{
		status = 0;
	}
	uEvent->handle = handle;
	return status;
}

int UEventClose(UEvent *uEvent,
				sys_call_error_fun fun)
{
	int status = -1;
	assert(uEvent);
	if (!CloseHandle(uEvent->handle)) 
	{
		SYS_CALL_ERROR(fun, "CloseHandle");
	}
	else
	{
		uEvent->handle = NULL;
		status = 0;
	}
	return status;
}

int UEventCloseAndUnlink(UEvent *uEvent,
						 sys_call_error_fun fun)
{
	return UEventClose(uEvent, fun);
}

int UEventSet(UEvent *uEvent,
			  sys_call_error_fun fun)
{
	int status = -1;
	assert(uEvent);
	if (!SetEvent(uEvent->handle))
	{
		SYS_CALL_ERROR(fun, "SetEvent");
	}
	else
	{
		status = 0;
	}
	return status;
}

int UEventReset(UEvent *uEvent,
				sys_call_error_fun fun)
{
	int status = -1;
	assert(uEvent);
	if (!ResetEvent(uEvent->handle))
	{
		SYS_CALL_ERROR(fun, "ResetEvent");
	}
	else
	{
		status=0;
	}
	return status;
}

int UEventWait(UEvent *uEvent,
			   sys_call_error_fun fun)
{
	int status = -1;
	assert(uEvent);
	if ( WAIT_FAILED == WaitForSingleObject(uEvent->handle, INFINITE))
	{
		SYS_CALL_ERROR(fun, "WaitForSingleObject");
	}
	else
	{
		status=0;
	}
	return status;
}

#else

/* On Linux events are implemented using SYS V semaphore arrays.
 * For each event an array of SEMARR_SIZE semaphores is created.
 * The SEMIDX_EVENT_TYPE-th semaphore encodes the event
 * type (either manual-reset [SEMVAL_EVENT_TYPE_MANRESET] or
 * auto-reset [SEMVAL_EVENT_TYPE_AUTORESET]). It is expected
 * that this semaphore is never modified during entire event's
 * lifetime.
 * The SEMIDX_EVENT_STATE-th semaphore holds the current event
 * state (either reset [SEMVAL_EVENT_STATE_RESET] or set 
 * [SEMVAL_EVENT_STATE_SET]).
 * Setting/resetting the event is implemented through semctl/SETVAL
 * calls. Waiting on the event implies examination of the
 * SEMIDX_EVENT_TYPE-th semaphore value to determine the proper
 * wait method. Depending on the result of this check either wait
 * (SEMIDX_EVENT_STATE-th unchanged) or wait-and-reset is performed.
 * The later step is performed atomically.
 * SYS V IPC objects has kernel persistence, UnlinkXXX functions
 * were designed to remove an object from the system. Unfortunately
 * the object is removed immediately, it doesn't matter if any
 * process was using the object at the moment the call was made.
 * This is the major deviation between Windows and Linux event
 * implementations. */

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>

#define SEMARR_SIZE                     2

#define SEMIDX_EVENT_STATE              1
#define SEMIDX_EVENT_TYPE               0

#define SEMVAL_EVENT_STATE_SET		(int)7
#define SEMVAL_EVENT_STATE_RESET	(int)3
#define SEMVAL_EVENT_TYPE_AUTORESET	(int)2
#define SEMVAL_EVENT_TYPE_MANRESET	(int)1


int UEventUnlink(global_name gn,
				 sys_call_error_fun fun)
{
	int status = -1;
	UEvent uEvent = {};
	
	if (UEventOpen(&uEvent, gn, fun)==0 && 
		UEventCloseAndUnlink(&uEvent, fun)==0)
	{
		status = 0;
	}
	return status;
}

int UEventCreate(UEvent *uEvent, 
				 USECURITY_ATTRIBUTES* sa,
				 int eventType,
				 int isSet,
				 global_name gn, 
				 sys_call_error_fun fun)
{
     struct gobj_info_t info = {GOBJECT_EVENT, uEvent};
	int status = -1;
	int semid = -1;
	USECURITY_ATTRIBUTES evmode = U_SEDNA_SEMAPHORE_ACCESS_PERMISSIONS_MASK;
	key_t key = IPC_PRIVATE;

	assert(uEvent);
	key = USys5IPCKeyFromGlobalName(gn);
	uEvent->semid = -1;
	if (sa) evmode = *sa;
	evmode |= IPC_CREAT | IPC_EXCL;

    if (UGlobalObjectsGC) { UGlobalObjectsGC->onCleanup(gn, info); };

	if (eventType != U_AUTORESET_EVENT && eventType != U_MANUALRESET_EVENT)
	{
		d_printf1("UEventCreate: unrecognised event type requested\n");
	}
	else if (semid = semget(key, SEMARR_SIZE, evmode), semid == -1) 
	{
		SYS_CALL_ERROR(fun, "semget");
	}
	else
	{
		unsigned short init[SEMARR_SIZE] = {0};
		unsigned short initState = SEMVAL_EVENT_STATE_RESET;
		unsigned short initType = SEMVAL_EVENT_TYPE_MANRESET;
		
		if (eventType == U_AUTORESET_EVENT)
			initType = SEMVAL_EVENT_TYPE_AUTORESET;

		if (isSet)
			initState = SEMVAL_EVENT_STATE_SET;
		
		init[SEMIDX_EVENT_TYPE] = initType;
		init[SEMIDX_EVENT_STATE] = initState;

		if (semctl(semid, 0, SETALL, init) == -1)
			SYS_CALL_ERROR(fun, "semctl");
		else status = 0;
			
	}
	if (status == 0) uEvent->semid = semid;
    
    if (UGlobalObjectsGC) { UGlobalObjectsGC->onCreate(gn, info); };
    
    return status;
}

int UEventOpen(UEvent *uEvent, global_name gn, sys_call_error_fun fun)
{
	int status = -1, semid = -1;
	key_t key = IPC_PRIVATE;
	
	assert(uEvent);
	key = USys5IPCKeyFromGlobalName(gn);
	if (gn.name == NULL)
	{
		d_printf1("UEventOpen: NULL invalid in this context\n");
	}
	else if (semid = semget(key, 0, 0), semid==-1)
	{
		SYS_CALL_ERROR(fun,"semget");
	}
	else
	{
		status = 0;
	}
	if (status == 0) uEvent->semid = semid;
	return status;
}

int UEventClose(UEvent *uEvent,
				sys_call_error_fun fun)
{
	assert(uEvent);
	uEvent->semid = -1;
	return 0;
}

int UEventCloseAndUnlink(UEvent *uEvent,
						 sys_call_error_fun fun)
{
     struct gobj_info_t info = {GOBJECT_EVENT, uEvent};
	int status = -1;
	
	assert(uEvent);
	if (semctl(uEvent->semid, 0, IPC_RMID) == -1)
		SYS_CALL_ERROR(fun,"semctl");
	else status = 0;

    if (UGlobalObjectsGC) { UGlobalObjectsGC->onDestroy(GN_NULL, info); };

    return (status==0) ? UEventClose(uEvent, fun) : status;
}


int UEventSet(UEvent *uEvent,
			  sys_call_error_fun fun)
{
	int status = -1;

	assert(uEvent);
	if (-1 == semctl(uEvent->semid, SEMIDX_EVENT_STATE, SETVAL, SEMVAL_EVENT_STATE_SET))
		SYS_CALL_ERROR(fun, "semctl");
	else status = 0;
	
	return status;
}

int UEventReset(UEvent *uEvent,
				sys_call_error_fun fun)
{
	int status = -1;

	assert(uEvent);
	if (-1 == semctl(uEvent->semid, SEMIDX_EVENT_STATE, SETVAL, SEMVAL_EVENT_STATE_RESET))
		SYS_CALL_ERROR(fun, "semctl");
	else status = 0;
	
	return status;
}

static
int EventWait(UEvent *uEvent, sys_call_error_fun fun)
{
	int status = -1;
	struct sembuf ops[2] = {{0}};

	assert(uEvent);
	ops[0].sem_num = SEMIDX_EVENT_STATE;
	ops[0].sem_op = -(SEMVAL_EVENT_STATE_SET);
	ops[1].sem_num = SEMIDX_EVENT_STATE;
	ops[1].sem_op = SEMVAL_EVENT_STATE_SET;
	
retry:
	if (-1 == semop(uEvent->semid, ops, 2))
	{
		if (errno == EINTR) goto retry;
		SYS_CALL_ERROR(fun, "semop");
	}
	else status = 0;
	
	return status;
}

static
int EventWaitReset(UEvent *uEvent, sys_call_error_fun fun)
{
        int status = -1;
        struct sembuf ops[2] = {{0}};

        assert(uEvent);
        ops[0].sem_num = SEMIDX_EVENT_STATE;
        ops[0].sem_op = -(SEMVAL_EVENT_STATE_SET);
        ops[1].sem_num = SEMIDX_EVENT_STATE;
        ops[1].sem_op = SEMVAL_EVENT_STATE_RESET;

retry:
        if (-1 == semop(uEvent->semid, ops, 2))
	{
		if (errno == EINTR) goto retry;
                SYS_CALL_ERROR(fun, "semop");
	}
        else status = 0;

        return status;
}

int UEventWait(UEvent *uEvent,
			   sys_call_error_fun fun)
{
	int status = -1, evtype = 0;

	assert(uEvent);
	if (evtype = semctl(uEvent->semid, SEMIDX_EVENT_TYPE, GETVAL),
			evtype != SEMVAL_EVENT_TYPE_MANRESET &&
			evtype != SEMVAL_EVENT_TYPE_AUTORESET)
	{
		d_printf1("UEventWait: unexpected semaphore value (probably wrong semid)\n");
	}
	else if (evtype == SEMVAL_EVENT_TYPE_MANRESET)
		status = EventWait(uEvent, fun);
	else
		status = EventWaitReset(uEvent, fun);
	
	return status;
}

#endif

