#if (_MSC_VER > 1000)
#pragma once
#endif

#ifndef WUVERSIONS_H_INCLUDED
#define WUVERSIONS_H_INCLUDED

#include "wutypes.h"
#include "wubuffers.h"
#include "wusnapshots.h"
#include "wuincguard.h"

struct VeResourceDemand
{
	size_t clientStateSize;
};

#define VE_SETUP_DISABLE_VERSIONS_FLAG				0x01
#define VE_SETUP_DISABLE_CORRECTNESS_CHECKS_FLAG	0x02

struct VeSetup
{
	/* params */ 
	int flags;

	/* tickets */ 
	TICKET clientStateTicket;

	/*	Buffer functions */ 
	int (*putBlockToBuffer) (XPTR xptr, int *bufferId);
	int (*findBlockInBuffers) (XPTR xptr, int *bufferId);
	int (*markBufferDirty) (int bufferId);
	int (*flushBuffer) (int bufferId, bool sync);
	int (*grantExclusiveAccessToBuffer) (int bufferId);
	int (*revokeExclusiveAccessToBuffer) (int bufferId);
	int (*allocateBlock) (XPTR *xptr, int *bufferId);
	int (*freeBlock) (XPTR xptr);
	int (*allocateBlockAndCopyData) (XPTR *xptr, int *bufferId, int srcBufferId);
	int (*locateVersionsHeader) (int bufferId, VersionsHeader **header);

	/* Callbacks */ 
	int (*onPersVersionRelocating) (LXPTR lxptr, XPTR oldVerXptr, int mode);
};

int VeInitialize();

void VeDeinitialize();

void VeQueryResourceDemand(VeResourceDemand *resourceDemand);

int VeStartup(VeSetup *setup);

int VeShutdown();

int VeOnRegisterClient();

int VeOnUnregisterClient();

#define VE_COMMIT_TRANSACTION						37
#define VE_ROLLBACK_TRANSACTION						29

int VeOnTransactionEnd(int how, TIMESTAMP currentSnapshotTs);

int VeClearTransactionRestrictions(int how);

int VePutBlockToBuffer(LXPTR lxptr, int *bufferId);

int VeAllocateBlock(LXPTR *lxptr, int *bufferId);

int VeCreateBlockVersion(LXPTR lxptr, int *bufferId);

int VeFreeBlock(LXPTR lxptr);

int VeFreeBlockLowAndUpdateRestrictions(XPTR xptr);

int VeOnFlushBuffer(XPTR xptr);

int VeOnCheckpoint();

void VeDbgDump(int flags);

#endif
