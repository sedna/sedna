#if (_MSC_VER > 1000)
#pragma once
#endif

#ifndef WU_INCLUDED
#define WU_INCLUDED

#include "wutypes.h"
#include "wudock.h"
#include "wusnapshotscs.h"
#include "wuerr.h"

typedef SnVersionEntry WuVersionEntry;

typedef SnEnumerateVersionsParams WuEnumerateVersionsParams;

typedef SnEnumerateVersionsProc WuEnumerateVersionsProc;

typedef SnSnapshotStats WuSnapshotStats;

int WuGetTimestamp(TIMESTAMP *ts);

int WuSetTimestamp(TIMESTAMP ts);

void WuInitExn(int isRecoveryMode, int isVersionsDisabled, TIMESTAMP persSnapshotTs=0);

void WuReleaseExn();

void WuOnBeginCheckpointExn();

void WuEnumerateVersionsForCheckpointExn(WuEnumerateVersionsParams *params,
										 WuEnumerateVersionsProc enumProc);

void WuOnCompleteCheckpointExn();

void WuAllocateDataBlockExn(int sid, xptr *p, ramoffs *offs, xptr *swapped);

void WuAllocateTempBlockExn(int sid, xptr *p, ramoffs *offs, xptr *swapped);

void WuCreateBlockVersionExn(int sid, xptr p, ramoffs *offs, xptr *swapped);

void WuDeleteBlockExn(int sid, xptr p);

void WuGetBlockExn(int sid, xptr p, ramoffs *offs, xptr *swapped);

void WuOnRegisterTransactionExn(int sid, int isUsingSnapshot,  TIMESTAMP *snapshotTs, int *persHeapIndex);

void WuOnCommitTransactionExn(int sid);

void WuOnRollbackTransactionExn(int sid);

void WuOnUnregisterTransactionExn(int sid);

void WuGatherSnapshotsStatsExn(WuSnapshotStats *);

bool WuTryAdvanceSnapshotsExn();

#define WU_DBG_DUMP_CLIENTS		1
#define WU_DBG_DUMP_VERSIONS	2
#define WU_DBG_DUMP_SNAPSHOTS	4

void WuDbgDump(int componentsSelector, int reserved);

size_t WuFetchSwappedXptrs(XPTR **xptrs);

#ifdef __WUDANG_SOURCES__

int WuInit(int isRecoveryMode, int isVersionsDisabled, TIMESTAMP persSnapshotTs=0);

int WuRelease();

int WuOnBeginCheckpoint();

int WuEnumerateVersionsForCheckpoint(WuEnumerateVersionsParams *params,
									 WuEnumerateVersionsProc enumProc);

int WuOnCompleteCheckpoint();

int WuAllocateDataBlock(int sid, xptr *p, ramoffs *offs, xptr *swapped);

int WuAllocateTempBlock(int sid, xptr *p, ramoffs *offs, xptr *swapped);

int WuCreateBlockVersion(int sid, xptr p, ramoffs *offs, xptr *swapped);

int WuDeleteBlock(int sid, xptr p);

int WuGetBlock(int sid, xptr p, ramoffs *offs, xptr *swapped);

int WuOnRegisterTransaction(int sid, int isUsingSnapshot, TIMESTAMP *snapshotTs, int *ipcObjectsSetIndex);

int WuOnCommitTransaction(int sid);

int WuOnRollbackTransaction(int sid);

int WuOnUnregisterTransaction(int sid);

int WuGatherSnapshotsStats(WuSnapshotStats *);

int WuTryAdvanceSnapshots(int *bSuccess);

#endif

#endif
