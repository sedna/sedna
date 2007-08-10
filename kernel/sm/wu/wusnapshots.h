#ifdef _MSC_VER
#pragma once
#endif

#ifndef WUSNAPSHOTS_INCLUDED
#define WUSNAPSHOTS_INCLUDED

#include "wutypes.h"
#include "wuincguard.h"

struct SnResourceDemand
{
	size_t clientStateSize;
};

struct SnSetup
{
	TICKET clientStateTicket;

	int (*freeBlock)(XPTR xptr);
	int (*getTimestamp)(TIMESTAMP *timestamp);
	
	int (*onDiscardSnapshot)(TIMESTAMP snapshotTs);

	TIMESTAMP initialPersSnapshotTs;
};

struct SnVersionEntry
{
	XPTR xptr;
	LXPTR lxptr;
};

struct SnRequestForGc
{
	LXPTR lxptr;				/* logical xptr - used for recovery */ 
	XPTR xptr;					/* physical xptr of the snapshot version */ 
	TIMESTAMP anchorTs;			/* timestamp of the most recent snapshot not-having this version */ 
};

struct SnEnumerateVersionsParams
{
	TIMESTAMP persSnapshotTs;
	size_t persVersionsCount;
	size_t garbageVersionsCount;
	size_t persVersionsSent;
	size_t garbageVersionsSent;
	void *userData;
};

struct SnStats
{
	size_t runawayVersionsCount;
	size_t versionsCount;
	size_t curSnapshotVersionsCount;
	size_t curSnapshotSharedVersionsCount;
	size_t persSnapshotVersionsCount;
	size_t persSnapshotSharedVersionsCount;
};

typedef int (*SnEnumerateVersionsProc)(SnEnumerateVersionsParams *params, 
									   SnVersionEntry *buf, size_t count, int isGarbage);

int SnInitialize();

void SnQueryResourceDemand(SnResourceDemand *resourceDemand);

int SnStartup(SnSetup *setup);

int SnShutdown();

void SnDeinitialize();

int SnOnRegisterClient(int isUsingSnapshot, TIMESTAMP *snapshotTs);

int SnOnUnregisterClient();

int SnAcceptRequestForGc(TIMESTAMP operationTs, SnRequestForGc *buf, size_t count);

int SnAdvanceSnapshots(TIMESTAMP *snapshotTs, TIMESTAMP *discardedTs);

int SnOnBeginCheckpoint(TIMESTAMP *persistentTs);

int SnEnumerateVersionsForCheckpoint(SnEnumerateVersionsParams *params,
									 SnEnumerateVersionsProc enumProc);

int SnOnCompleteCheckpoint();

int SnGatherStats(SnStats *stats);

int SnCheckIfCanAdvanceSnapshots(int *canMakeCurrentSnapshotPersistent);

int SnGetCurrentSnapshotTs(TIMESTAMP *timestamp);

void SnDbgDump(int reserved); 

#endif
