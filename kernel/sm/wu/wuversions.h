#ifdef _MSC_VER
#pragma once
#endif

#ifndef WUVERSIONS_INCLUDED
#define WUVERSIONS_INCLUDED

#include "wutypes.h"
#include "wubuffers.h"
#include "wusnapshots.h"

struct VersionsResourceDemand
{
	size_t clientStateSize;
	size_t bufferStateSize;
};

struct VersionsSetup
{
	TICKET clientStateTicket;
	TICKET bufferStateTicket; 

	/*	Buffer functions 
		- rebindBuffer - changes an xptr associated with buffer identified
		by bufferId. An error code is returned if a buffer associated with
		xptr already exists and it's id is not bufferId. 
		- loadBuffer - puts block identified by xptr in buffer (flags=0). 
		- unloadBuffer - marks buffer as free and flushes if dirty (flags=0).
		- getBufferInfo - stores buffer info in user-provided structure.
		- getBufferStateBlock - get state block stored in internal
		buffer structures.
		- fixBuffer - prevents buffer from beeing ejected.
		- protectBuffer - affect memory protection applied to the buffer
		when it is mapped in TRN next time (debug version also changes protection
		emediately for all TRNs the block is mapped to). 
		- markBufferDirty - either marks buffer dirty or removes this mark. */ 
	int (*rebindBuffer)(int bufferId, XPTR xptr);
	int (*loadBuffer)(XPTR xptr, int *bufferId, int flags);
	int (*unloadBuffer)(int bufferId, int flags);
	int (*getBufferInfo)(int bufferId, BufferInfo *bufferInfo);
	int (*getBufferStateBlock)(int bufferId, TICKET ticket, void **data);
	int (*fixBuffer)(int bufferId, int orMask, int andNotMask);
	int (*protectBuffer)(int bufferId, int orMask, int andNotMask);
	int (*markBufferDirty)(int bufferId, int from, int to, int flags);

	/*	Alloc functions */ 
	int (*allocBlock)(XPTR *xptr);
	int (*freeBlock)(XPTR xptr);

	/* Timestamp functions */ 
	int (*getTimestamp)(TIMESTAMP *timestamp);

	/* GC functions */ 
	int (*acceptRequestForGc)(TIMESTAMP operationTs, SnapshotsRequestForGc *buf, size_t count);

	/*	Callbacks 
		- onCompleteBlockRelocation - called emediately after block who is not included 
		in the latest snapshot relocates. Block is already written at xptr,
		however it's copy at lxptr still exists.
		May be called in context where clientId!=GetCurrentCLientId(). */ 
	int (*onCompleteBlockRelocation)(int clientId, LXPTR lxptr, XPTR xptr);
};

int VeInitialise();

void VeQueryResourceDemand(VersionsResourceDemand *versionsResourceDemand);

int VeStartup(VersionsSetup *setup);

void VeDeinitialise();

int VeOnRegisterClient(TIMESTAMP snapshotTs, int isUsingSnapshot);

int VeOnUnregisterClient();

int VeLoadBuffer(LXPTR lxptr, int *bufferId);

int VeAllocBlock(LXPTR *lxptr);

int VeCreateVersion(LXPTR lxptr);

int VeFreeBlock(LXPTR lxptr);

int VeOnCommit();

int VeOnRollback();

int VeOnSnapshotAdvanced(TIMESTAMP snapshotTs, TIMESTAMP discardedTs);

int VeOnCheckpoint(TIMESTAMP persistentTs);

int VeOnFlushBlock(int bufferId);

int VeGetCurrentClientTs(TIMESTAMP *timestamp);

void VeDbgDump(int reserved);

#endif
