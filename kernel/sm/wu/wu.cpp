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

static
TIMESTAMP timestamp = 1000;

int WuGetTimestamp(TIMESTAMP *ts)
{
	int success = 0;
	assert(ts); *ts=0;
	if (!timestamp == !(TIMESTAMP)0)
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
	WuSetLastErrorMacro(WUERR_NOT_IMPLEMENTED);
	return 0;
}

static
int MarkBufferDirty(int bufferId, void *base, size_t size, int flags)
{
	vmm_sm_blk_hdr *header = (vmm_sm_blk_hdr*)OffsetPtr(buf_mem_addr,RamoffsFromBufferId(bufferId));
	header->is_changed = true;
	return 1;
}

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

int LocateHeader(int bufferId, VersionsHeader **veHeader)
{
	assert(veHeader);
	vmm_sm_blk_hdr *header = (vmm_sm_blk_hdr*)OffsetPtr(buf_mem_addr,RamoffsFromBufferId(bufferId));
	*veHeader = &header->versionsHeader;
	return 1;
}

int OnCompleteBlockRelocation(int clientId, LXPTR lxptr, XPTR xptr)
{
	int success=0;
	SnapshotsVersion snapshotVersion = {lxptr, xptr};
	try
	{
		ll_add_pers_snapshot_block_info(&snapshotVersion);
		success=1;
	}
	WU_CATCH_EXCEPTIONS()
	return success;
}

int OnDiscardSnapshot(TIMESTAMP snapshotTs)
{
	/* kill file or something */ 
	return 1;
}

int WuInit(int is_rcv_mode)
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
		OnDiscardSnapshot
	};
	VersionsResourceDemand versionsResourceDemand;
	SnapshotsResourceDemand snapshotsResourceDemand;
	int success=0;

	if (!ClInitialise()) {}
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
	int success=0;
	if (!ShShutdown()) {}
	else
	{
		success=1;
	}
	ShDeinitialise();
	VeDeinitialise();
	ClDeinitialise();
	return success;
}

int WuNotifyCheckpointActivatedAndWaitForSnapshotAdvanced()
{
	return 1;
}

struct WuJunction
{
	SnapshotsOnCheckpointParams *params;
	int(*saveListsProc)(SnapshotsOnCheckpointParams *params, SnapshotsVersion *buf, size_t count, int isGarbage);
};

static
int helperProc(SnapshotsOnCheckpointParams *params2, SnapshotsVersion *buf, size_t count, int isGarbage)
{
	int success=0;
	WuJunction *junction = (WuJunction *)params2->userData;

	assert(junction && junction->params && junction->saveListsProc);

	junction->params->persistentSnapshotTs = params2->persistentSnapshotTs;
	junction->params->persistentVersionsCount = params2->persistentVersionsCount;
	junction->params->garbageVersionsCount = params2->garbageVersionsCount;
	junction->params->persistentVersionsSent = params2->persistentVersionsSent;
	junction->params->garbageVersionsSent = params2->garbageVersionsSent;

	try
	{
		success = junction->saveListsProc(junction->params, buf, count, isGarbage);
	}
	WU_CATCH_EXCEPTIONS()

	return success;
}

int WuEnumerateVersionsForCheckpoint(SnapshotsOnCheckpointParams *params,
									 int(*saveListsProc)(SnapshotsOnCheckpointParams *params, SnapshotsVersion *buf, size_t count, int isGarbage))
{
	assert(params);
	SnapshotsOnCheckpointParams params2;
	WuJunction junction = {params, saveListsProc};
	params2.userData = &junction;
	return ShOnCheckpoint(&params2,helperProc);
}

int WuNotifyCheckpointFinished()
{
	return 1;
}	

void WuInitExn(int is_rcv_mode)
{
	if (!WuInit(is_rcv_mode)) WuThrowException();
}

void WuReleaseExn()
{
	if (!WuRelease()) WuThrowException();
}

void WuNotifyCheckpointActivatedAndWaitForSnapshotAdvancedExn()
{
	if (!WuNotifyCheckpointActivatedAndWaitForSnapshotAdvanced()) WuThrowException();
}

void WuNotifyCheckpointFinishedExn()
{
	if (!WuNotifyCheckpointFinished()) WuThrowException();
}

void WuEnumerateVersionsForCheckpointExn(SnapshotsOnCheckpointParams *params,
										 int(*saveListsProc)(SnapshotsOnCheckpointParams *params, SnapshotsVersion *buf, size_t count, int isGarbage))
{
	if (!WuEnumerateVersionsForCheckpoint(params,saveListsProc)) WuThrowException();
}
