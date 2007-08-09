#define __WUDANG_SOURCES__

#include "wuaux.h"
#include <assert.h>
#include "wu.h"
#include "wuerr.h"
#include "wuclients.h"
#include "wusnapshots.h"
#include "wuversions.h"
#include "wudock.h"

#include "common/base.h"
#include "sm/llmgr/llmgr.h"
#include "sm/bufmgr/bm_core.h"
#include "sm/bufmgr/blk_mngmt.h"
#include "common/sm_vmm_data.h"
#include "common/u/u.h"
#include "common/u/umutex.h"
#include "sm/bufmgr/bm_functions.h"
#include <windows.h>

/* global variables */ 

static TIMESTAMP timestamp = ~(TIMESTAMP)0;
static uMutexType gMutex;
static HANDLE hSnapshotsAdvancedEvent = NULL;
static TIMESTAMP curSnapshotTs=0, persSnapshotTs=0;

/* utility functions */ 

static
int InitSynchObjects()
{
	int success = 0;
	if (uMutexInit(&gMutex,__sys_call_error)!=0) {}
	else
	{
		hSnapshotsAdvancedEvent = CreateEvent(NULL,FALSE,FALSE,NULL);
		if (hSnapshotsAdvancedEvent)
		{
			success=1;
		}
		else
		{
			uMutexDestroy(&gMutex,__sys_call_error);
		}
	}
	return success;
}

static
int DeinitSynchObjects()
{
	int failure = 0;

	if (!CloseHandle(hSnapshotsAdvancedEvent)) failure = 1;

	if (!uMutexDestroy(&gMutex,__sys_call_error)) failure = 1;

	return (failure == 0);
}

static
int BufferIdFromRamoffs(ramoffs ofs)
{
	assert(ofs % PAGE_SIZE == 0);
	return ofs / PAGE_SIZE;
}

static
ramoffs RamoffsFromBufferId(int id)
{
	return id * PAGE_SIZE;
}

/* wiring functions */ 

static
int LoadBuffer(XPTR bigXptr, int *bufferId, int flags)
{
	int success=0;
	xptr lilXptr=WuExternaliseXptr(bigXptr);
	ramoffs ofs;
	assert(bufferId); *bufferId=-1;
	try
	{
		put_block_to_buffer(-1,lilXptr,&ofs,true);
		*bufferId = BufferIdFromRamoffs(ofs);
		success=1;
	}
	WU_CATCH_EXCEPTIONS()
	return success;
}

static
int FlushBuffer(int bufferId, int flags)
{
	int success = 0;
	ramoffs ofs = RamoffsFromBufferId(bufferId);
	vmm_sm_blk_hdr *header = (vmm_sm_blk_hdr*)OffsetPtr(buf_mem_addr,RamoffsFromBufferId(bufferId));
	try
	{
		flush_buffer(ofs,false);
		success = 1;
	}
	WU_CATCH_EXCEPTIONS()
	return success;
}

static
int GetBufferInfo(int bufferId, BufferInfo *bufferInfo)
{
	WuSetLastErrorMacro(WUERR_NOT_IMPLEMENTED);
	return 0;
}

static
int GetBufferStateBlock(int bufferId, TICKET ticket, void **data)
{
	WuSetLastErrorMacro(WUERR_NOT_IMPLEMENTED);
	return 0;
}

static
int FixBuffer(int bufferId, int orMask, int andNotMask)
{
	WuSetLastErrorMacro(WUERR_NOT_IMPLEMENTED);
	return 0;
}

static
int ProtectBuffer(int bufferId, int orMask, int andNotMask)
{
	vmm_sm_blk_hdr *header = (vmm_sm_blk_hdr*)OffsetPtr(buf_mem_addr,RamoffsFromBufferId(bufferId));
	int mask=orMask&~andNotMask;
	if (mask&32)
	{
		header->trid_wr_access=ClGetCurrentClientId();
	}
	else
	{
		header->trid_wr_access=-1;
	}
	return 1;
}

static
int MarkBufferDirty(int bufferId, void *base, size_t size, int flags)
{
	vmm_sm_blk_hdr *header = (vmm_sm_blk_hdr*)OffsetPtr(buf_mem_addr,RamoffsFromBufferId(bufferId));
	header->is_changed = true;
	return 1;
}

static
int CopyBlock(XPTR bigDest, XPTR bigSrc, int flags)
{
	xptr lilDest = WuExternaliseXptr(bigDest), lilSrc = WuExternaliseXptr(bigSrc);
	ramoffs ofsSrc=0, ofsDest=0;
	vmm_sm_blk_hdr *header = NULL;
	int success = 0, repairRequired = 0;
	if (bigDest == bigSrc)
	{
		WuSetLastErrorMacro(WUERR_BAD_PARAMS);
	}
	else try
	{
		put_block_to_buffer(-1,lilSrc,&ofsSrc,true);
		if (used_mem.find_remove(ofsSrc))
		{
			repairRequired = 1;
			put_block_to_buffer(-1,lilDest,&ofsDest,true);
			memcpy(header=(vmm_sm_blk_hdr *)OffsetPtr(buf_mem_addr,ofsDest),
				   OffsetPtr(buf_mem_addr,ofsSrc),
				   PAGE_SIZE);
			header->is_changed = true;
			if (flags&1)
			{
				flush_buffer(ofsDest,false);
				header->is_changed = false;
			}
		}
	}
	WU_CATCH_EXCEPTIONS()
	if (repairRequired) used_mem.push(ofsSrc);
	return success;
}

static
int AllocBlock(XPTR *bigXptr)
{
	int success=0;
	xptr lilXptr;
	assert(bigXptr); *bigXptr=0;
	try
	{
		new_data_block(&lilXptr);
		*bigXptr=WuInternaliseXptr(lilXptr);
		success=1;
	}
	WU_CATCH_EXCEPTIONS()
	return success;
}

static
int FreeBlock(XPTR bigXptr)
{
	int success=0;
	xptr lilXptr;
	lilXptr = WuExternaliseXptr(bigXptr);
	try
	{
		delete_data_block(lilXptr);
		success=1;
	}
	WU_CATCH_EXCEPTIONS();
	return success;
}

static
int LocateHeader(int bufferId, VersionsHeader **veHeader)
{
	assert(veHeader);
	vmm_sm_blk_hdr *header = (vmm_sm_blk_hdr*)OffsetPtr(buf_mem_addr,RamoffsFromBufferId(bufferId));
	*veHeader = &header->versionsHeader;
	return 1;
}

static
int OnCompleteBlockRelocation(int clientId, LXPTR lxptr, XPTR xptr)
{
	int success=0;
	WuVersionEntry versionEntry = {lxptr, xptr};
	try
	{
		ll_add_pers_snapshot_block_info(&versionEntry);
		success=1;
	}
	WU_CATCH_EXCEPTIONS()
	return success;
}

static
int OnDiscardSnapshot(TIMESTAMP snapshotTs)
{
	/* kill file or something */ 
	WuSetLastErrorMacro(WUERR_NOT_IMPLEMENTED);
	return 0;
}

/* public api */ 

int WuGetTimestamp(TIMESTAMP *ts)
{
	int success = 0;
	assert(ts); *ts=0;
	if (timestamp == ~(TIMESTAMP)0)
	{
		WuSetLastErrorMacro(WUERR_MAX_TIMESTAMP_VALUE_EXCEEDED);
	}
	else
	{
		*ts=timestamp++;
		success = 1;
	}
	return success;
}

int WuSetTimestamp(TIMESTAMP ts)
{
	timestamp = ts;
	return 1;
}

int WuInit(int isRecoveryMode, int isVersionsDisabled, TIMESTAMP persSnapshotTs)
{
	ClientsSetup clientsSetup = {CHARISMA_MAX_TRNS_NUMBER, 0x10000};
	VersionsSetup versionsSetup =
	{
		NULL,
		NULL,

		LoadBuffer,
		FlushBuffer,
		GetBufferInfo,
		GetBufferStateBlock,
		FixBuffer,
		ProtectBuffer,
		MarkBufferDirty,
		CopyBlock,
		AllocBlock,
		FreeBlock,
		WuGetTimestamp,
		ShAcceptRequestForGc,
		LocateHeader,
		OnCompleteBlockRelocation
	};
	SnapshotsSetup snapshotsSetup =
	{
		NULL,

		FreeBlock,
		WuGetTimestamp,
		OnDiscardSnapshot,
		persSnapshotTs
	};
	VersionsResourceDemand versionsResourceDemand;
	SnapshotsResourceDemand snapshotsResourceDemand;
	int success=0;

	if (!InitSynchObjects()) {}
	else if (!ClInitialise()) {}
	else if (!ShInitialise()) {}
	else if (!VeInitialise()) {}
	else
	{
		VeQueryResourceDemand(&versionsResourceDemand);
		ShQueryResourceDemand(&snapshotsResourceDemand);

		if (!ClReserveStateBlocks(&versionsSetup.clientStateTicket, versionsResourceDemand.clientStateSize)) {}
		else if (!ClReserveStateBlocks(&snapshotsSetup.clientStateTicket, snapshotsResourceDemand.clientStateSize)) {}
		else if (!ClStartup(&clientsSetup)) {}
		else if (!VeStartup(&versionsSetup)) {}
		else if (!ShStartup(&snapshotsSetup)) {}
		else
		{
			success = 1;
		}
	}
	if (!success)
	{
		ShDeinitialise();
		VeDeinitialise();
		ClDeinitialise();
	}
	return success;
}

int WuRelease()
{
	int failure=0;
	if (!ShShutdown()) failure=1;
	ShDeinitialise();
	VeDeinitialise();
	ClDeinitialise();
	if (!DeinitSynchObjects()) failure=1;
	return (failure==0);
}

int WuNotifyCheckpointActivatedAndWaitForSnapshotAdvanced()
{
	int failure = 0, success = 0, lockedok = 0;

	while (!failure && !lockedok)
	{
		if (uMutexLock(&gMutex,__sys_call_error)!=0) { failure=1; }
		else
		{
			if (curSnapshotTs==persSnapshotTs)
			{
				uMutexUnlock(&gMutex,__sys_call_error);
				if (WaitForSingleObject(hSnapshotsAdvancedEvent,INFINITE)!=WAIT_OBJECT_0)
				{
					failure=1;
				}
				else if (!SetEvent(hSnapshotsAdvancedEvent)) 
				{
					failure=1;
				}
			}
			else
			{
				lockedok=1;
				uMutexUnlock(&gMutex,__sys_call_error);
			}
		}
	}

	if (lockedok)
	{
		if (!ShOnBeginCheckpoint(&persSnapshotTs)) {}
		else if (!VeOnBeginCheckpoint()) {}
		else
		{
			success=1;
		}
		uMutexUnlock(&gMutex,__sys_call_error);
	}	 
	return success; 
}

struct WuEnumerateVersionsParamsAdapter
{
	WuEnumerateVersionsParams *params;
	int(*saveListsProc)(WuEnumerateVersionsParams *params, WuVersionEntry *buf, size_t count, int isGarbage);
};

static
int helperProc(SnapshotsOnCheckpointParams *params, SnapshotsVersion *buf, size_t count, int isGarbage)
{
	int success=0;
	WuEnumerateVersionsParamsAdapter *adapter = (WuEnumerateVersionsParamsAdapter *)params->userData;

	assert(adapter && adapter->params && adapter->saveListsProc);

	adapter->params->persistentSnapshotTs = params->persistentSnapshotTs;
	adapter->params->persistentVersionsCount = params->persistentVersionsCount;
	adapter->params->garbageVersionsCount = params->garbageVersionsCount;
	adapter->params->persistentVersionsSent = params->persistentVersionsSent;
	adapter->params->garbageVersionsSent = params->garbageVersionsSent;

	try
	{
		success = adapter->saveListsProc(adapter->params, (WuVersionEntry *)buf, count, isGarbage);
	}
	WU_CATCH_EXCEPTIONS()

	return success;
}

int WuEnumerateVersionsForCheckpoint(WuEnumerateVersionsParams *params,
									 int(*saveListsProc)(WuEnumerateVersionsParams *params, WuVersionEntry *buf, size_t count, int isGarbage))
{

	int success;
	assert(params);
	SnapshotsOnCheckpointParams params2;
	WuEnumerateVersionsParamsAdapter adapter = {params, saveListsProc};
	params2.userData = &adapter;
	
	if (uMutexLock(&gMutex,__sys_call_error)!=0) {}
	else
	{
		if (!ShOnCheckpoint(&params2,helperProc)) {}
		else
		{
			success = 1;
		}
		uMutexUnlock(&gMutex,__sys_call_error);
	}

	return success;
}

int WuNotifyCheckpointFinished()
{
	int success = 0;
	if (uMutexLock(&gMutex, __sys_call_error) !=0 ) {}
	else
	{
		if (!VeOnCompleteCheckpoint(persSnapshotTs)) {}
		else if (!ShOnCompleteCheckpoint()) {}
		else
		{
			success = 1;
		}
		uMutexUnlock(&gMutex, __sys_call_error);
	}
	return success;
}	

int WuAllocateDataBlock(int sid, xptr *p, ramoffs *offs, xptr *swapped)
{
	int success=0, bufferId=0;
	LXPTR lxptr;

	assert(p && offs && swapped);
	if (uMutexLock(&gMutex,__sys_call_error)!=0) {}
	else
	{
		if (!ClSetCurrentClientId(sid)) {}
		else
		{
			*p=XNULL;
			*offs=0;
			*swapped=XNULL;
			if (!VeAllocBlock(&lxptr)) {}
			else if (!LoadBuffer(lxptr,&bufferId,0)) {}
			{
				*p=WuExternaliseXptr(lxptr);
				*offs=RamoffsFromBufferId(bufferId);
				ProtectBuffer(bufferId,32,0);
				success=1;
			}
			ClSetCurrentClientId(-1);
		}
		uMutexUnlock(&gMutex, __sys_call_error);
	}
	return success;
}

int WuCreateBlockVersion(int sid, xptr p, ramoffs *offs, xptr *swapped)
{
	LXPTR lxptr=0;
	int success=0, bufferId=0;

	assert(offs && swapped);
	*offs=0;
	*swapped=XNULL;
	if (uMutexLock(&gMutex,__sys_call_error)!=0) {}
	else
	{
		if (IS_TMP_BLOCK(p))
		{
			WuSetLastErrorMacro(WUERR_VERSIONS_UNSUPPORTED_FOR_THIS_BLOCK_TYPE);
		}
		else
		{
			lxptr=WuInternaliseXptr(p);
			if (!VeCreateVersion(lxptr)) {}
			else if (!VeLoadBuffer(lxptr,&bufferId,0)) {}
			else
			{
				*offs=RamoffsFromBufferId(bufferId);
				success=1;
			}
		}
		uMutexUnlock(&gMutex, __sys_call_error);
	}
	return success;
}

int WuDeleteBlock(int sid, xptr p)
{
	LXPTR lxptr=0;
	int success=0, bufferId=0;

	if (uMutexLock(&gMutex,__sys_call_error)!=0) {}
	else
	{
		if (!ClSetCurrentClientId(sid)) {}
		else
		{
			if (IS_TMP_BLOCK(p))
			{
				try
				{
					bm_delete_block(sid,p);
					success=1;
				}
				WU_CATCH_EXCEPTIONS();
			}
			else
			{
				lxptr = WuInternaliseXptr(p);
				if (VeFreeBlock(lxptr)) 
				{
					success=1;
				}
			}
			ClSetCurrentClientId(-1);
		}
		uMutexUnlock(&gMutex, __sys_call_error);
	}
	return success;
}

int WuGetBlock(int sid, xptr p, ramoffs *offs, xptr *swapped)
{
	LXPTR lxptr=0;
	int success=0, bufferId=0;
	
	assert(offs && swapped);
	if (uMutexLock(&gMutex,__sys_call_error)!=0) {}
	else
	{
		if (!ClSetCurrentClientId(sid)) {}
		else
		{
			lxptr = WuInternaliseXptr(p);
			*offs=0;
			*swapped=XNULL;
			if (IS_DATA_BLOCK(p))
			{
				if (VeLoadBuffer(lxptr,&bufferId,0))
				{
					success=1;
				}
			}
			else
			{
				if(LoadBuffer(lxptr,&bufferId,0))
				{
					ProtectBuffer(bufferId,0,0);
					success=1;
				}
			}
					
			if (success)
			{
				*offs=RamoffsFromBufferId(bufferId);
			}
			ClSetCurrentClientId(-1);
		}
		uMutexUnlock(&gMutex, __sys_call_error);
	}
	return success;
}

int WuOnRegisterTransaction(int sid, int isUsingSnapshot, TIMESTAMP *snapshotTs, int *ipcObjectsSetIndex)
{
	int success=0;

	assert(snapshotTs && ipcObjectsSetIndex);
	if (uMutexLock(&gMutex,__sys_call_error)!=0) {}
	else
	{
		if (!ClRegisterClient(&sid,1)) {}
		else
		{
			if (!ClSetCurrentClientId(sid)) {}
			else
			{
				if (!ShOnRegisterClient(isUsingSnapshot,snapshotTs)) {}
				if (!VeOnRegisterClient(isUsingSnapshot,*snapshotTs)) {}
				if (!ClMarkClientReady(sid)) {}
				else
				{
					success=1;
				}
				ClSetCurrentClientId(-1);
			}
			if (!success) ClUnregisterClient(sid);
		}
		uMutexUnlock(&gMutex, __sys_call_error);
	}
	return success;
}

int WuOnCommitTransaction(int sid)
{
	int success=0;

	if (uMutexLock(&gMutex,__sys_call_error)!=0) {}
	else
	{
		if (!ClSetCurrentClientId(sid)) {}
		else
		{
			if (!VeOnCommit()) {}
			else
			{
				success=1;
			}
			ClSetCurrentClientId(-1);
		}
		uMutexUnlock(&gMutex, __sys_call_error);
	}
	return success;
}

int WuOnRollbackTransaction(int sid)
{
	int success=0;

	if (uMutexLock(&gMutex,__sys_call_error)!=0) {}
	else
	{
		if (!ClSetCurrentClientId(sid)) {}
		else
		{
			if (!VeOnRollback()) {}
			else
			{
				success=1;
			}
			ClSetCurrentClientId(-1);
		}		
		uMutexUnlock(&gMutex, __sys_call_error);
	}
	return success;
}

int WuOnUnregisterTransaction(int sid)
{
	int success=0;

	if (uMutexLock(&gMutex,__sys_call_error)!=0) {}
	else
	{
		if (!ClSetCurrentClientId(sid)) {}
		else
		{
			if (!ClMarkClientLeaving(sid)) {}
			else if (!VeOnUnregisterClient()) {}
			else if (!ShOnUnregisterClient()) {}
			else
			{
				success=1;
			}
			ClSetCurrentClientId(-1);
			success=ClUnregisterClient(sid);
		}
		uMutexUnlock(&gMutex, __sys_call_error);
	}
	return success;
}

int WuGatherSnapshotStats(WuSnapshotStats *stats)
{
	SnapshotsStats snStats;
	int success=0;

	assert(stats);
	if (uMutexLock(&gMutex,__sys_call_error)!=0) {}
	else
	{
		if (!ShCheckIfCanAdvanceSnapshots(&stats->isAbleToAdvanceSnapshots,NULL)){}
		else if (!ShGatherStats(&snStats)) {}
		else
		{
			stats->versionsCount = snStats.versionsCount;
			stats->runawayVersionsCount = 0;
			stats->curSnapshotVersionsCount = snStats.curSnapshotVersionsCount;
			stats->curSnapshotSharedVersionsCount = snStats.curSnapshotSharedVersionsCount;
			stats->persSnapshotVersionsCount = snStats.persSnapshotVersionsCount;
			stats->persSnapshotSharedVersionsCount = snStats.persSnapshotSharedVersionsCount;
			stats->curSnapshotTs = curSnapshotTs;
			stats->persSnapshotTs = persSnapshotTs;
			success=1;
		}
		uMutexUnlock(&gMutex, __sys_call_error);
	}
	if (!success) memset(stats,0,sizeof *stats);
	return success;
}

int WuAdvanceSnapshots()
{
	TIMESTAMP discardedSnapshotTs=0;
	int success=0;

	if (uMutexLock(&gMutex,__sys_call_error)!=0) {}
	else
	{
		if (!SetEvent(hSnapshotsAdvancedEvent)) {}
		else if (!ShAdvanceSnapshots(&curSnapshotTs,&discardedSnapshotTs)) {}
		else if (!VeOnSnapshotAdvanced(curSnapshotTs,discardedSnapshotTs)) {}
		{
			success=1;
		}
		uMutexUnlock(&gMutex, __sys_call_error);
	}

	return success;
}

/* public api, exn adapters */ 

void WuInitExn(int isRecoveryMode, int isVersionsDisabled, TIMESTAMP persSnapshotTs)
{
	if (!WuInit(isRecoveryMode, isVersionsDisabled, persSnapshotTs)) WuThrowException();
}

void WuReleaseExn()
{
	if (!WuRelease()) WuThrowException();
}

void WuNotifyCheckpointActivatedAndWaitForSnapshotAdvancedExn()
{
	if (!WuNotifyCheckpointActivatedAndWaitForSnapshotAdvanced()) WuThrowException();
}

void WuEnumerateVersionsForCheckpointExn(WuEnumerateVersionsParams *params,
										 int(*saveListsProc)(WuEnumerateVersionsParams *params, WuVersionEntry *buf, size_t count, int isGarbage))
{
	if (!WuEnumerateVersionsForCheckpoint(params,saveListsProc)) WuThrowException();
}

void WuNotifyCheckpointFinishedExn()
{
	if (!WuNotifyCheckpointFinished()) WuThrowException();
}

void WuAllocateDataBlockExn(int sid, xptr *p, ramoffs *offs, xptr *swapped)
{
	if (!WuAllocateDataBlock(sid, p, offs, swapped)) WuThrowException();
}

void WuCreateBlockVersionExn(int sid, xptr p, ramoffs *offs, xptr *swapped)
{
	if (!WuCreateBlockVersion(sid, p, offs, swapped)) WuThrowException();
}


void WuDeleteBlockExn(int sid, xptr p)
{
	if (!WuDeleteBlock(sid, p)) WuThrowException();
}

void WuGetBlockExn(int sid, xptr p, ramoffs *offs, xptr *swapped)
{
	if (!WuGetBlock(sid, p, offs, swapped)) WuThrowException();
}

void WuOnRegisterTransactionExn(int sid, int isUsingSnapshot, TIMESTAMP *snapshotTs, int *persHeapIndex)
{
	if (!WuOnRegisterTransaction(sid, isUsingSnapshot, snapshotTs, persHeapIndex)) WuThrowException();
}

void WuOnCommitTransactionExn(int sid)
{
	if (!WuOnCommitTransaction(sid)) WuThrowException();
}

void WuOnRollbackTransactionExn(int sid)
{
	if (!WuOnRollbackTransaction(sid)) WuThrowException();
}

void WuOnUnregisterTransactionExn(int sid)
{
	if (!WuOnUnregisterTransaction(sid)) WuThrowException();
}

