#if (_MSC_VER > 1000)
#pragma once
#endif

#ifndef WU_INCLUDED
#define WU_INCLUDED

#include "wutypes.h"
#include "wudock.h"
#include "wuerr.h"

int WuGetTimestamp(TIMESTAMP *ts);

int WuSetTimestamp(TIMESTAMP ts);

struct WuEnumerateVersionsParams
{
	TIMESTAMP persistentSnapshotTs;
	size_t persistentVersionsCount;
	size_t garbageVersionsCount;
	size_t persistentVersionsSent;
	size_t garbageVersionsSent;
	void *userData;
};

struct WuVersionEntry
{
	XPTR xptr;
	LXPTR lxptr;
};

struct WuSnapshotStats
{
	size_t versionsCount;
	size_t runawayVersionsCount;
	size_t curSnapshotVersionsCount;
	size_t curSnapshotSharedVersionsCount;
	size_t persSnapshotVersionsCount;
	size_t persSnapshotSharedVersionsCount;
	TIMESTAMP curSnapshotTs, persSnapshotTs;
	int isAbleToAdvanceSnapshots;
};

void WuInitExn(int isRecoveryMode, int isVersionsDisabled, TIMESTAMP persSnapshotTs=0);

void WuReleaseExn();

void WuNotifyCheckpointActivatedAndWaitForSnapshotAdvancedExn();

void WuEnumerateVersionsForCheckpointExn(WuEnumerateVersionsParams *params,
										 int(*saveListsProc)(WuEnumerateVersionsParams *params, WuVersionEntry *buf, size_t count, int isGarbage));

void WuNotifyCheckpointFinishedExn();

void WuAllocateDataBlockExn(int sid, xptr *p, ramoffs *offs, xptr *swapped);

void WuCreateBlockVersionExn(int sid, xptr p, ramoffs *offs, xptr *swapped);

void WuDeleteBlockExn(int sid, xptr p);

void WuGetBlockExn(int sid, xptr p, ramoffs *offs, xptr *swapped);

void WuOnRegisterTransactionExn(int sid, int isUsingSnapshot,  TIMESTAMP *snapshotTs, int *persHeapIndex);

void WuOnCommitTransactionExn(int sid);

void WuOnRollbackTransactionExn(int sid);

void WuOnUnregisterTransactionExn(int sid);


#ifdef __WUDANG_SOURCES__

int WuInit(int isRecoveryMode, int isVersionsDisabled, TIMESTAMP persSnapshotTs=0);

int WuRelease();

int WuNotifyCheckpointActivatedAndWaitForSnapshotAdvanced();

int WuEnumerateVersionsForCheckpoint(WuEnumerateVersionsParams *params,
									 int(*saveListsProc)(WuEnumerateVersionsParams *params, WuVersionEntry *buf, size_t count, int isGarbage));

int WuNotifyCheckpointFinished();

int WuAllocateDataBlock(int sid, xptr *p, ramoffs *offs, xptr *swapped);

int WuCreateBlockVersion(int sid, xptr p, ramoffs *offs, xptr *swapped);

int WuDeleteBlock(int sid, xptr p);

int WuGetBlock(int sid, xptr p, ramoffs *offs, xptr *swapped);

int WuOnRegisterTransaction(int sid, int isUsingSnapshot, TIMESTAMP *snapshotTs, int *ipcObjectsSetIndex);

int WuOnCommitTransaction(int sid);

int WuOnRollbackTransaction(int sid);

int WuOnUnregisterTransaction(int sid);

int WuGatherSnapshotStats(WuSnapshotStats *);

int WuAdvanceSnapshots();

#endif

#endif
