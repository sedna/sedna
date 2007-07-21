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

	int (*freeBlock)(XPTR xptr);
	int (*revertBlock)(VersionsCreateVersionParams *);
	int (*getTimestamp)(TIMESTAMP *timestamp);

	int (*onCanAdvanceSnapshots)();
	int (*onCurrentSnapshotGrowing)(size_t versionsCount, size_t sharedVersionsCount);
	int (*onPersistentSnapshotGrowing)(size_t versionsCount, size_t sharedVersionsCount);
	int (*onDiscardSnapshot)(TIMESTAMP snapshotTs, int *isDiscardingOk);
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
	size_t persistentVersionsCount;
	size_t garbageVersionsCount;
	size_t persistentVersionsSent;
	size_t garbageVersionsSent;
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
