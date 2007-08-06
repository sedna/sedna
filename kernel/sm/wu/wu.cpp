#include "wuaux.h"
#include <assert.h>
#include "wu.h"
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
int LoadBuffer(XPTR xptr, int *bufferId, int flags)
{
	WUERROR(WUERR_NOT_IMPLEMENTED);
	return 0;
}

static
int FlushBuffer(int bufferId, int flags)
{
	WUERROR(WUERR_NOT_IMPLEMENTED);
	return 0;
}

static
int GetBufferInfo(int bufferId, BufferInfo *bufferInfo)
{
	WUERROR(WUERR_NOT_IMPLEMENTED);
	return 0;
}

static
int GetBufferStateBlock(int bufferId, TICKET ticket, void **data)
{
	WUERROR(WUERR_NOT_IMPLEMENTED);
	return 0;
}

static
int FixBuffer(int bufferId, int orMask, int andNotMask)
{
	WUERROR(WUERR_NOT_IMPLEMENTED);
	return 0;
}

static
int ProtectBuffer(int bufferId, int orMask, int andNotMask)
{
	WUERROR(WUERR_NOT_IMPLEMENTED);
	return 0;
}

static
int MarkBufferDirty(int bufferId, void *base, size_t size, int flags)
{
	WUERROR(WUERR_NOT_IMPLEMENTED);
	return 0;
}

int CopyBlock(XPTR dest, XPTR src, int flags)
{
	WUERROR(WUERR_NOT_IMPLEMENTED);
	return 0;
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

int LocateHeader(int bufferId, VersionsHeader **header)
{
	/*
	ramoffs rofs = (ramofs)bufferId;
	put_block_to_buffer(ClGetCurrentClientId(),
		);
		*/ 

	assert(header);
	return 1;
}

int OnCompleteBlockRelocation(int clientId, LXPTR lxptr, XPTR xptr)
{
	int success=0;
	SnapshotsVersion snapshotVersion = {lxptr, xptr};
	try
	{
		ll_add_pers_snapshot_block_info(0,&snapshotVersion);
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
