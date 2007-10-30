#include <assert.h>
#include "uevent.h"

#ifdef _WIN32

int UEventUnlink(global_name gn,
				 sys_call_error_fun fun)
{
	int status = -1;
	if (gn == NULL)
	{
		dprintf1("UEventUnlink: U_INVALID_GLOBAL_NAME is invalid in this context\n");
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
	HANDLE handle = NULL;
	assert(uEvent);
	if (eventType!=U_AUTORESET_EVENT && eventType!=U_MANUALRESET_EVENT)
	{
		dprintf1("UEventCreate: unrecognised event type requested\n");
	}
	else if (NULL == (handle=CreateEvent(sa, eventType==U_MANUALRESET_EVENT, isSet, gn)))
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
	HANDLE handle = NULL;
	assert(uEvent);
	if (NULL == (handle=OpenEvent(EVENT_ALL_ACCESS, FALSE, gn)))
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

int UEventUnlink(global_name gn,
				 sys_call_error_fun fun)
{
	int status = -1;
	UEvent uEvent = {};
	if (UEventOpen(&uEvent, gn, fun)==0 && UEventCloseAndUnlink(&uEvent)==0)
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
	int sem_id = -1;
	assert(uEvent);
	
	uEvent->sem_id = sem_id;
	return status;
}

int UEventOpen(UEvent *uEvent, 
			   global_name gn, 
			   sys_call_error_fun fun);

int UEventClose(UEvent *uEvent,
				sys_call_error_fun fun)
{
	assert(uEvent);
	uEvent->sem_id = 0;
	uEvent->event_type = 0;
	return 0;
}

EXTERN_C 
int UEventCloseAndUnlink(UEvent *uEvent,
						 sys_call_error_fun fun)
{
	return 0;
}


int UEventSet(UEvent *uEvent,
			  sys_call_error_fun fun);

int UEventReset(UEvent *uEvent,
				sys_call_error_fun fun);

int UEventWait(UEvent *uEvent,
			   sys_call_error_fun fun);

#endif
