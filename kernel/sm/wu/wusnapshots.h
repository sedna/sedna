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
	int clientId;
	int isUsingSnapshot;
};

struct SnapshotsSetup
{
	TICKET clientStateTicket;

	int (*revertBlock)(VersionsCreateVersionParams *);
	int (*getTimestamp)(TIMESTAMP *timestamp);

	int (*onCanAdvanceSnapshots)();
};

struct SnapshotsVersionInfo
{
	LXPTR lxptr;
	XPTR xptr;
	int isGarbage;
};


int ShInitialise();
int ShStartup(SnapshotsSetup *setup);
int ShDeinitialise();

int ShOnRegisterClient(SnapshotsClientInfo *clientInfo, TIMESTAMP *snapshotTs);
int ShOnUnregisterClient(int clientId);
int ShOnCreateVersion(VersionsCreateVersionParams *);
int ShOnRollback();
int ShOnCommit();
int ShAdvanceSnapshots();
int ShOnCheckpoint(TIMESTAMP *persistentSnapshotTs,
				   int(*)(void *userData, SnapshotsVersionInfo*, size_t size, size_t *sizeRemaining), 
				   void *userData);

#endif
