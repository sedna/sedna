#ifdef _MSC_VER
#pragma once
#endif

#ifndef WUSNAPSHOTS_INCLUDED
#define WUSNAPSHOTS_INCLUDED

#include "wutypes.h"
#include "wuversions.h"

struct SnapshotsResourceDemand
{
	size_t clientStateSize;
};

struct SnapshotsClientInfo
{
	int isUsingSnapshot;
	TIMESTAMP snapshotTs; /* out */ 
};

struct SnapshotsSetup
{
	TICKET clientStateTicket;

	int (*revertBlock)(VersionsCreateVersionParams *);
	int (*getTimestamp)(TIMESTAMP *timestamp);

	int (*onCanAdvanceSnapshots)();
	int (*onCurrentSnapshotGrowing)(size_t totalBlocksCount, size_t exclusiveBlocksCount);
	int (*onPersistentSnapshotGrowing)(size_t totalBlocksCount, size_t exclusiveBlocksCount);
	int (*onDiscardSnapshot)(TIMESTAMP snapshotTs);
};

struct SnapshotsVersionInfo
{
	LXPTR lxptr;
	XPTR xptr;
	int isGarbage;
};

struct SnapshotsOnCheckpointInfo
{
	TIMESTAMP persistentSnapshotTs;
	size_t persistentSnapshotBlocksCount;
	size_t garbageBlocksCount;
	int *activeClientIds;
	size_t activeClientCount;
	int state; /* 0-begin 2-inprocess 1-end */ 
	void *userData;
};

int ShInitialise();
void ShQueryResourceDemand(SnapshotsResourceDemand *resourceDemand);
int ShStartup(SnapshotsSetup *setup);
int ShShutdown();
void ShDeinitialise();

int ShOnRegisterClient(SnapshotsClientInfo *clientInfo);
int ShOnUnregisterClient();
int ShOnCreateVersion(VersionsCreateVersionParams *);
int ShOnRollback();
int ShOnCommit();
int ShAdvanceSnapshots(TIMESTAMP *snapshotTs, TIMESTAMP *discardedTs);

int ShOnBeginCheckpoint(TIMESTAMP *persistentTs);

int ShOnCheckpoint(SnapshotsOnCheckpointInfo *onCheckpointInfo,
				   int(*saveListsProc)(SnapshotsOnCheckpointInfo *onCheckpointInfo, SnapshotsVersionInfo *buf, size_t count));

int ShOnCompleteCheckpoint();

int WirNotifyCheckpointActivatedAndWaitForSnapshotAdvanced();
int WirNotifyCheckpointFinished();

#endif
