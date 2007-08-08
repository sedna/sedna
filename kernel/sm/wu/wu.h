#if (_MSC_VER > 1000)
#pragma once
#endif

#ifndef WU_INCLUDED
#define WU_INCLUDED

#include "wutypes.h"
#include "wudock.h"

int WuGetTimestamp(TIMESTAMP *ts);

int WuSetTimestamp(TIMESTAMP ts);

int WuInit(int is_rcv_mode);

int WuRelease();

int WuNotifyCheckpointActivatedAndWaitForSnapshotAdvanced();

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

int WuEnumerateVersionsForCheckpoint(WuEnumerateVersionsParams *params,
									 int(*saveListsProc)(WuEnumerateVersionsParams *params, WuVersionEntry *buf, size_t count, int isGarbage));

int WuNotifyCheckpointFinished();

int WuAllocateBlock(int sid, xptr *p, ramoffs *offs, xptr *swapped);

int WuCreateBlockVersion(int sid, xptr p, ramoffs *offs, xptr *swapped);

int WuDeleteBlock(int sid, xptr p);

int WuGetBlock(int sid, xptr p, ramoffs *offs, xptr *swapped);

int WuOnRegisterTransaction(int sid, int isUsingSnapshot, int *ipcObjectsSetIndex);

int WuOnCommitTransaction(int sid);

int WuOnRollbackTransaction(int sid);

int WuOnUnregisterTransaction(int sid);

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

int WuGatherSnapshotStats(WuSnapshotStats *);

int WuAdvanceSnapshots();

/* reports errors with exceptions instead of error codes */ 

void WuInitExn(int is_rcv_mode);

void WuReleaseExn();

void WuNotifyCheckpointActivatedAndWaitForSnapshotAdvancedExn();

void WuEnumerateVersionsForCheckpointExn(WuEnumerateVersionsParams *params,
										 int(*saveListsProc)(WuEnumerateVersionsParams *params, WuVersionEntry *buf, size_t count, int isGarbage));

void WuNotifyCheckpointFinishedExn();

void WuAllocateBlockExn(int sid, xptr *p, ramoffs *offs, xptr *swapped);

void WuCreateBlockVersionExn(int sid, xptr p, ramoffs *offs, xptr *swapped);

void WuDeleteBlockExn(int sid, xptr p);

void WuGetBlockExn(int sid, xptr p, ramoffs *offs, xptr *swapped);

void WuOnRegisterTransactionExn(int sid, int isUsingSnapshot, int *ipcObjectsSetIndex);

void WuOnCommitTransactionExn(int sid);

void WuOnRollbackTransactionExn(int sid);

void WuOnUnregisterTransactionExn(int sid);

#endif
