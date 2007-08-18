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

	/* params */ 
	TIMESTAMP initialPersSnapshotTs;
	int areVersionsDisabled;
};

/* garbage entries have lxptr==0 */ 
struct SnVersionEntry
{
	LXPTR lxptr;
	XPTR xptr;
};

/*	SnRequestForGc

	[type==SN_REQUEST_TYPE_NOP]
	Do nothing. 

	[type==SN_REQUEST_TYPE_ALWAYS_DELETE]
	Emediately delete the block identified by xptr.

	[type==SN_REQUEST_TYPE_OLDER_VERSION] 
	Make an older version subject for the GC. Both xptr and lxptr must be valid.
	Xptr identifies the block; it is deleted as soon as the version becomes useless.
	Lxptr is a logical XPTR associated with the version. It is essential for the recovery.
	Finally anchorTs is used to determine snapshots the version belongs to. Every
	snapshot with timestamp in the range [CUR_TIMESTAMP, anchorTs) is owning the version.
	Function updates runawayVersionsCount (available through SnGatherStats) if the version 
	is emediately discarded prior to submission due to no owning snapshot were found.

	[type==SN_REQUEST_TYPE_ZOMBIE]
	Same as SN_REQUEST_TYPE_OLDER_VERSION except runawayVersionsCount not updated
	and the version is always reported as garbage by SnEnumerateVersionsForCheckpoint.
	  
	*/ 

#define SN_REQUEST_TYPE_NOP				0
#define SN_REQUEST_TYPE_ALWAYS_DELETE	1
#define SN_REQUEST_TYPE_OLDER_VERSION	2
#define SN_REQUEST_TYPE_ZOMBIE			3

struct SnRequestForGc
{
	LXPTR lxptr;
	XPTR xptr;
	TIMESTAMP anchorTs;
	int type;
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
