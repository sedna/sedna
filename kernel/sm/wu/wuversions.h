#ifdef _MSC_VER
#pragma once
#endif

#ifndef WUVERSIONS_INCLUDED
#define WUVERSIONS_INCLUDED

#include "wutypes.h"
#include "wubuffers.h"
#include "wusnapshots.h"
#include "wutypes.h"
#include "wuincguard.h"

struct VeResourceDemand
{
	size_t clientStateSize;
	size_t bufferStateSize;
};

#define VE_SETUP_DISABLE_VERSIONS_FLAG				0x01
#define VE_SETUP_DISABLE_CORRECTNESS_CHECKS_FLAG	0x02

struct VeSetup
{
	/* params */ 
	TIMESTAMP initialPersSnapshotTs;
	int flags;

	/* tickets */ 
	TICKET clientStateTicket;
	TICKET bufferStateTicket; 

	/*	Buffer functions */ 
	int (*loadBuffer)(XPTR xptr, int *bufferId, int flags);
	int (*flushBuffer)(int bufferId, int flags);
	int (*getBufferInfo)(int bufferId, BufferInfo *bufferInfo);
	int (*getBufferStateBlock)(int bufferId, TICKET ticket, void **data);
	int (*fixBuffer)(int bufferId, int orMask, int andNotMask);
	int (*protectBuffer)(int bufferId, int orMask, int andNotMask);
	int (*markBufferDirty)(int bufferId, void *base, size_t size, int flags);

	/* copy data functions */ 
	int (*copyBlock)(XPTR dest, XPTR src, int flags);

	/*	Alloc functions */ 
	int (*allocBlock)(XPTR *xptr);
	int (*freeBlock)(XPTR xptr);

	/* Timestamp functions */ 
	int (*getTimestamp)(TIMESTAMP *timestamp);

	/* GC functions */ 
	int (*submitRequestForGc)(TIMESTAMP operationTs, SnRequestForGc *buf, size_t count);

	/* data layout functions */ 
	int (*locateHeader)(int bufferId, VersionsHeader **header);

	/*	Callbacks 
		- onCompleteBlockRelocation - called emediately after block who is not included 
		in the latest snapshot relocates. Block is already written at xptr,
		however it's copy at lxptr still exists.
		May be called in context where clientId!=GetCurrentCLientId(). */ 
	int (*onCompleteBlockRelocation)(int clientId, LXPTR lxptr, XPTR xptr);
};

int VeInitialize();

void VeQueryResourceDemand(VeResourceDemand *resourceDemand);

int VeStartup(VeSetup *setup);

void VeDeinitialize();

int VeOnRegisterClient(int isUsingSnapshot, TIMESTAMP snapshotTs);

int VeOnUnregisterClient();

/* VePutBlockToBuffer*/ 
int VeLoadBuffer(LXPTR lxptr, int *bufferId, int flags);

/* VeAllocateBlock */ 
int VeAllocBlock(LXPTR *lxptr);

/* remove it */ 
int VeInitBlockHeader(LXPTR lxptr, int bufferId);

int VeCreateBlockVersion(LXPTR lxptr);

int VeFreeBlock(LXPTR lxptr);

/*	This is not intended for the direct use but should be rather hooked into Snapshots module. 
	The function is called when the block sheduled for deletion (SnSubmitRequestForGc()) is really
	deleted. At this point we have to update internal data structures. Finally calls setup.freeBlock on the xptr. */ 
int VeReallyFreeBlock(XPTR xptr);

int VeOnCommit();

int VeOnRollback();

int VeOnSnapshotsAdvanced(TIMESTAMP snapshotTs, TIMESTAMP discardedTs);

int VeOnBeginCheckpoint();

int VeOnCompleteCheckpoint(TIMESTAMP persistentTs);

int VeOnFlushBlock(int bufferId);

/* The transaction timestamp associated with the current client; INVALID_TIMESTAMP if the transaction is a pure query. */ 
int VeGetCurrentTs(TIMESTAMP *timestamp);

void VeDbgDump(int reserved);

#endif
