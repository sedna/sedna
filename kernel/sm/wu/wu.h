#if (_MSC_VER > 1000)
#pragma once
#endif

#ifndef WU_INCLUDED
#define WU_INCLUDED

#include "wutypes.h"
#include "wudock.h"
#include "wuerr.h"

struct WuVersionEntry
{
	XPTR xptr;
	LXPTR lxptr;
};

struct WuEnumerateVersionsParams
{
	TIMESTAMP persSnapshotTs;
	size_t persVersionsCount;
	size_t garbageVersionsCount;
	size_t persVersionsSent;
	size_t garbageVersionsSent;
	void *userData;
};

typedef int (*WuEnumerateVersionsProc)(WuEnumerateVersionsParams *params, 
									   WuVersionEntry *buf, size_t count, int isGarbage);

struct WuSnapshotStats
{
	TIMESTAMP curSnapshotTs, persSnapshotTs;
	size_t runawayVersionsCount;
	size_t versionsCount;	
	size_t curSnapshotVersionsCount;
	size_t curSnapshotSharedVersionsCount;
	size_t persSnapshotVersionsCount;
	size_t persSnapshotSharedVersionsCount;	
	int isAbleToAdvanceSnapshots;
};

int WuGetTimestamp(TIMESTAMP *ts);

int WuSetTimestamp(TIMESTAMP ts);

void WuInitExn(int isRecoveryMode, int isVersionsDisabled, TIMESTAMP persSnapshotTs=0);

void WuReleaseExn();

void WuNotifyCheckpointActivatedAndWaitForSnapshotAdvancedExn();

void WuEnumerateVersionsForCheckpointExn(WuEnumerateVersionsParams *params,
										 WuEnumerateVersionsProc enumProc);

void WuNotifyCheckpointFinishedExn();

void WuAllocateDataBlockExn(int sid, xptr *p, ramoffs *offs, xptr *swapped);

void WuCreateBlockVersionExn(int sid, xptr p, ramoffs *offs, xptr *swapped);

void WuDeleteBlockExn(int sid, xptr p);

void WuGetBlockExn(int sid, xptr p, ramoffs *offs, xptr *swapped);

void WuOnRegisterTransactionExn(int sid, int isUsingSnapshot,  TIMESTAMP *snapshotTs, int *persHeapIndex);

void WuOnCommitTransactionExn(int sid);

void WuOnRollbackTransactionExn(int sid);

void WuOnUnregisterTransactionExn(int sid);

void WuGatherSnapshotsStatsExn(WuSnapshotStats *);

void WuAdvanceSnapshotsExn();

#define WU_CLIENTS		1
#define WU_VERSIONS		2
#define WU_SNAPSHOTS	4

void WuDbgDump(int componentsSelector, int reserved);

size_t WuFetchSwappedXptrs(XPTR *xptrs);

#ifdef __WUDANG_SOURCES__

int WuInit(int isRecoveryMode, int isVersionsDisabled, TIMESTAMP persSnapshotTs=0);

int WuRelease();

int WuNotifyCheckpointActivatedAndWaitForSnapshotAdvanced();

int WuEnumerateVersionsForCheckpoint(WuEnumerateVersionsParams *params,
									 WuEnumerateVersionsProc enumProc);

int WuNotifyCheckpointFinished();

int WuAllocateDataBlock(int sid, xptr *p, ramoffs *offs, xptr *swapped);

int WuCreateBlockVersion(int sid, xptr p, ramoffs *offs, xptr *swapped);

int WuDeleteBlock(int sid, xptr p);

int WuGetBlock(int sid, xptr p, ramoffs *offs, xptr *swapped);

int WuOnRegisterTransaction(int sid, int isUsingSnapshot, TIMESTAMP *snapshotTs, int *ipcObjectsSetIndex);

int WuOnCommitTransaction(int sid);

int WuOnRollbackTransaction(int sid);

int WuOnUnregisterTransaction(int sid);

int WuGatherSnapshotsStats(WuSnapshotStats *);

int WuAdvanceSnapshots();

#endif

#endif
