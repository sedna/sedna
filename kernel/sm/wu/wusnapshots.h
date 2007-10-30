#if (_MSC_VER>1000)
#pragma once
#endif

#ifndef WUSNAPSHOTS_H_INCLUDED
#define WUSNAPSHOTS_H_INCLUDED

#include "wusnapshotscs.h"
#include "wuincguard.h"

struct SnResourceDemand
{
	size_t clientStateSize;
};

#define SN_SETUP_DISABLE_VERSIONS_FLAG			0x01

struct SnSetup
{
	/* params */ 
	TIMESTAMP initialPersSnapshotTs;
	int flags;
	int maxClientsCount;

	/* tickets */ 
	TICKET clientStateTicket;

	/* wire */ 
	int (*freeBlock)(XPTR xptr);
	int (*getTimestamp)(TIMESTAMP *timestamp);
	
	int (*onDiscardSnapshot)(TIMESTAMP snapshotTs);
	int (*onBeforeDiscardSnapshot)(TIMESTAMP snapshotTs, int *bDenyDiscarding);
};

/*	SnSubmitRequestForGc

	[type==SN_REQUEST_NOP]
	Do nothing. 

	[type==SN_REQUEST_DISCARD_VERSION]
	Emediately delete the block identified by xptr.

	[type==SN_REQUEST_ADD_NORMAL_VERSION] 
	Make an older version subject for the GC. Both xptr and lxptr must be valid.
	Xptr identifies the block; it is deleted as soon as the version becomes useless.
	Lxptr is a logical XPTR associated with the version. It is essential for the recovery.
	Finally anchorTs is used to determine snapshots the version belongs to. Every
	snapshot with timestamp in the range [CUR_TIMESTAMP, anchorTs] is owning the version.
	Function updates runawayVersionsCount (available through SnGatherSnapshotStats) if the version 
	is emediately discarded prior to submission due to no owning snapshot was found.

	[type==SN_REQUEST_ADD_BOGUS_VERSION]
	Same as SN_REQUEST_ADD_NORMAL_VERSION except runawayVersionsCount not updated
	and the version is always reported as garbage by SnEnumerateVersionsForCheckpoint.
	  
	*/ 

/* SnRequestForGc */ 
#define SN_REQUEST_NOP							0
#define SN_REQUEST_DISCARD_VERSION				1
#define SN_REQUEST_ADD_NORMAL_VERSION			2
#define SN_REQUEST_ADD_BOGUS_VERSION			3

struct SnRequestForGc
{
	LXPTR lxptr;
	XPTR xptr;
	TIMESTAMP anchorTs;
	int type;
};

int SnInitialize();

void SnDeinitialize();

void SnQueryResourceDemand(SnResourceDemand *resourceDemand);

int SnStartup(const SnSetup *setup);

int SnShutdown();

int SnOnRegisterClient(int isUsingSnapshot);

int SnOnUnregisterClient();

int SnOnTransactionEnd(TIMESTAMP *currentSnapshotTs);

int SnSubmitRequestForGc(TIMESTAMP currentSnapshotTs, const SnRequestForGc *buf, size_t count);

int SnTryAdvanceSnapshots(TIMESTAMP *newSnapshotTs);

int SnPurifySnapshots();

int SnOnBeginCheckpoint(TIMESTAMP *persistentTs);

int SnOnCompleteCheckpoint();

int SnEnumerateVersionsForCheckpoint(SnEnumerateVersionsParams *params,
									 SnEnumerateVersionsProc enumProc);

int SnGatherSnapshotStats(SnSnapshotStats *stats);

int SnGetSnapshotTimestamps(TIMESTAMP *curSnapshotTs,
							TIMESTAMP *persSnapshotTs);

/* SnGetTransactionStatusAndType */ 
#define SN_UPDATER_TRANSACTION					1
#define SN_READ_ONLY_TRANSACTON					2
#define SN_COMPLETED_TRANSACTION				3

int SnGetTransactionStatusAndType(int *statusAndType);

int SnGetTransactionSnapshotTs(TIMESTAMP *timestamp);

int SnGetTransactionTs(TIMESTAMP *timestamp);

/* SnExpandDfvHeader */ 
#define SN_LAST_COMMITED_VERSION_TIMESTAMP		(1 + TIMESTAMP_MAX)
#define SN_WORKING_VERSION_TIMESTAMP			(2 + TIMESTAMP_MAX)

int SnExpandDfvHeader(const TIMESTAMP tsIn[],
					  size_t szIn,
					  TIMESTAMP tsOut[],
					  int idOut[],
					  size_t *szOut,
					  TIMESTAMP *anchorTs);

int SnDamageSnapshots(TIMESTAMP timestampFrom);

/* SnDbgDump */ 
#define SN_DUMP_ATIMESTAMPS						0x2000
#define SN_DUMP_SNAPSHOTS						0x4000
#define SN_DUMP_GC_NODES						0x8000
#define SN_DUMP_UNLIMITED_FLAG					0x0001
#define SN_DUMP_SNAPSHOT_ATIMESTAMPS_FLAG		0x0002
#define SN_DUMP_FUTURE_SNAPSHOTS_FLAG			0x0004
#define SN_DUMP_SNAPSHOT_PROPERTIES_FLAG		0x0008

void SnDbgDump(int flags); 

#endif
