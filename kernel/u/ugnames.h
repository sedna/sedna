#if (_MSC_VER>1000)
#pragma once
#endif

#ifndef UGNAMES_H_INCLUDED
#define UGNAMES_H_INCLUDED

/*	Introducing a novel approach to cross-platform global names.

	Problem:
		Windows uses strings for naming IPC objects.
		SYS 5 IPC uses key_t integers.
		Posix IPC uses strings.
		We are using both SYS 5 and Posix IPC on Linux.

	Solution:
		We define global_name type to hold a global name.
		Global_name is a c-string and it is the same on all
		platforms. In the 'U' layer the global_name is converted
		into platform-specific IPC name.

	Example:
		base name -							SHMEM_BUFFERS
		global name -						SEDNA1500.DB7.SHMEM_BUFFERS@1802
		UWinIPCNameFromGlobalName() -		SEDNA1500.DB7.SHMEM_BUFFERS
		USys5IPCKeyFromGlobalName() -		1802
		UPosixIPCNameFromGlobalName() -		/SEDNA1500.DB7.SHMEM_BUFFERS

	Generation:
		Global names generation is automated. All global names
		should be listed in globalNamesRegistry table. Entries
		in the table are not for *individual* names but rather
		for *name families*. Consider a shared memory segment
		used as the buffer memory. Every database instance has 
		it's own buffer memory, but only one entry in the registry
		is made for SHMEM_BUFFERS (the basename). 

		The global name is generated with UCreateGlobalName() function.
		The function prepends prefixes to the basename resulting in
		(assume database id is 7) SEDNA1500.DB7.SHMEM_BUFFERS. Finally a '@1802'
		part is appended (1802 is a generated ordinal number).
		The call to CreateGlobalName() with the same 
		parameters produces the same global name. If parameters change
		the generated name is not the same (including the ordinal number).
		
		The ordinal number is used as the source for SYS 5 IPC key.

	Private IPC objects:
		NULL global name by convention stands for "private" object.
		
	Compound names:
		Sometimes it is handy to pass only one name while multiple
		IPC objects are to be created. Compound name is just an
		array of global names packed in the single string, ex:
		"SEDNA1500.DB7.SHMEM_SM_TALK@1852,SEDNA1500.DB7.SEMARR_SM_TALK@1902" */ 

#ifndef EXTERNC
#ifdef __cplusplus
#define EXTERNC extern "C" 
#else
#define EXTERNC
#endif
#endif

#include "u/u.h"

typedef void (* UGlobalObjectEvent) (global_name name, const char * type, void * data, int arg1, int arg2);

typedef struct UGlobalGarbageCollector_tag_ {
    UGlobalObjectEvent onCleanup;
    UGlobalObjectEvent onCreate;
    UGlobalObjectEvent onDestroy;
} UGlobalGarbageCollector;

/*	An item of the global names registry. */ 
typedef struct UGlobalNamesRegistryItem_tag_
{
	const char *basename;	/* used for registry lookup and for names generation */ 
	const char *prefix;		/* when we have multiple global names in the family we need prefix */ 
	int nObjectsMax;		/* how many objects we are going to have */
	int tag; /* User data */
	int rangeBegin;			/* internal */ 
	int rangeEnd;			/* internal */ 
}
UGlobalNamesRegistryItem;

typedef 
const UGlobalNamesRegistryItem *
(* UGlobalNamesRegistrySearchProc)(const UGlobalNamesRegistryItem *registry, 
								   const char *basename);

typedef
void
(* UGlobalNamesRegistryErrorProc)(const char *msg);

EXTERNC
UGlobalGarbageCollector * UGlobalObjectsGC;

/* Initialize global names registry. */ 
EXTERNC
void 
UInitGlobalNamesRegistry(UGlobalNamesRegistryItem *registry,
                         UGlobalNamesRegistrySearchProc searchProc,
                         UGlobalNamesRegistryErrorProc errorProc,
                         int rangeBegin,
                         int rangeEnd);

/* Release allocated resources. */ 
EXTERNC
void
UReleaseGlobalNamesRegistry();

/*	Create a global name. */ 
EXTERNC
const char *
UCreateGlobalName(const char *basename,
				  int objectId,
				  char *buf,
				  size_t bufSize);

/*  Create a global name. */
EXTERNC
const char *
UCreateGlobalNameFromRegistry(const UGlobalNamesRegistryItem * item,
                  int objectId,
                  char *buf,
                  size_t bufSize);

/* Create a compound name (an array of global names). */ 
EXTERNC
const char *
UCreateCompoundName(const char **nameVec,
					size_t namesCount,
					char *buf,
					size_t bufSize);

/* Extract a component of the compound name. */ 
EXTERNC
const char *
UGlobalNameFromCompoundName(const char *compoundName,
							int index,
							char *buf,
							size_t bufSize);

/* Create a Windows IPC object name from the global name. */ 
EXTERNC
const char *UWinIPCNameFromGlobalName(const char *globalName,
									  char *buf,
									  size_t bufSize);

/* Create a SYS 5 IPC key from the global name. */ 
EXTERNC
int USys5IPCKeyFromGlobalName(const char *globalName);

/* Create a Posix IPC name from the global name. */ 
EXTERNC
const char *UPosixIPCNameFromGlobalName(const char *globalName,
										char *buf,
										size_t bufSize);

#endif
