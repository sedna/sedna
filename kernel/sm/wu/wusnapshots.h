#ifdef _MSC_VER
#pragma once
#endif

#ifndef WUSNAPSHOTS_INCLUDED
#define WUSNAPSHOTS_INCLUDED

#include "wutypes.h"
#include "wuincguard.h"

struct SnapshotsResourceDemand
{
	size_t clientStateSize;
};

struct SnapshotsSetup
{
	TICKET clientStateTicket;

	int (*freeBlock)(XPTR xptr);
	int (*getTimestamp)(TIMESTAMP *timestamp);
	
	int (*onDiscardSnapshot)(TIMESTAMP snapshotTs);

	TIMESTAMP initialPersSnapshotTs;
};

struct SnapshotsVersion
{
	XPTR xptr;
	LXPTR lxptr;
};

struct SnapshotsRequestForGc
{
	LXPTR lxptr;				/* logical xptr - used for recovery */ 
	XPTR snapshotVersionXptr;	/* physical xptr of the snapshot version */ 
	TIMESTAMP anchorTs;			/* timestamp of the most recent snapshot not-having this version */ 
};

struct SnapshotsOnCheckpointParams
{
	TIMESTAMP persistentSnapshotTs;
	size_t persistentVersionsCount;
	size_t garbageVersionsCount;
	size_t persistentVersionsSent;
	size_t garbageVersionsSent;
	void *userData;
};

struct SnapshotsStats
{
	size_t versionsCount;
	size_t curSnapshotVersionsCount;
	size_t curSnapshotSharedVersionsCount;
	size_t persSnapshotVersionsCount;
	size_t persSnapshotSharedVersionsCount;
};

int ShInitialise();

void ShQueryResourceDemand(SnapshotsResourceDemand *resourceDemand);

int ShStartup(SnapshotsSetup *setup);

int ShShutdown();

void ShDeinitialise();

int ShOnRegisterClient(int isUsingSnapshot, TIMESTAMP *snapshotTs);

int ShOnUnregisterClient();

int ShAcceptRequestForGc(TIMESTAMP operationTs, SnapshotsRequestForGc *buf, size_t count);

int ShAdvanceSnapshots(TIMESTAMP *snapshotTs, TIMESTAMP *discardedTs);

int ShOnBeginCheckpoint(TIMESTAMP *persistentTs);

int ShOnCheckpoint(SnapshotsOnCheckpointParams *params,
				   int(*saveListsProc)(SnapshotsOnCheckpointParams *params, SnapshotsVersion *buf, size_t count, int isGarbage));

int ShOnCompleteCheckpoint();

int ShGatherStats(SnapshotsStats *stats);

int ShCheckIfCanAdvanceSnapshots(int *canAdvance, int *canMakeCurrentSnapshotPersistent);

int ShGetCurrentClientSnapshotTs(TIMESTAMP *timestamp);

void ShDbgDump(int reserved); 

#endif
