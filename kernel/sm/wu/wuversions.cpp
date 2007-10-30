#define __WUDANG_SOURCES__

#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <list>
#include <hash_map>
#include <algorithm>
#include "wuaux.h"
#include "wuerr.h"
#include "wuversions.h"
#include "wuclients.h"

#define VE_SNAPSHOTS_COUNT		3
#define VE_MAX_CLIENTS_COUNT	0x10000
#define VE_BUFSZ				1024

typedef SnRequestForGc VeVersionManipAction;

#define VE_ACTION_TYPE_CREATE_1ST_VERSION		SN_REQUEST_NOP
#define VE_ACTION_TYPE_CREATE_VERSION			SN_REQUEST_ADD_NORMAL_VERSION
#define VE_ACTION_TYPE_DELETE_VERSION			SN_REQUEST_ADD_BOGUS_VERSION

struct VeRestrictionEntry
{
	XPTR xptr;
	TIMESTAMP deletorTs;
	int creatorId;
	VeRestrictionEntry *next;
};

struct VeClientState
{
	TIMESTAMP snapshotTs;
	TIMESTAMP clientTs;
	std::list<VeVersionManipAction> *versionManipActions;
	VeRestrictionEntry *restrictions;
};

/* functions from other modules */ 
static int ImpGetCurrentStateBlock (VeClientState **ptr);
static int ImpGetSnapshotTimestamps (TIMESTAMP *curSnapshotTs, TIMESTAMP *persSnapshotTs);
static int ImpGetTransactionStatusAndType (int *statusAndType);
static int ImpGetTransactionSnapshotTs (TIMESTAMP *timestamp);
static int ImpGetTransactionTs (TIMESTAMP *timestamp);
static int ImpExpandDfvHeader (const TIMESTAMP tsIn[],
							   size_t szIn,
							   TIMESTAMP tsOut[],
							   int idOut[],
							   size_t *szOut);

static int ImpDamageSnapshots(TIMESTAMP timestampMax);

/* global state */ 

static int isInitialized = 0;
static TICKET ticket = NULL;
static VeSetup setup = {};
static TIMESTAMP persSnapshotTs = INVALID_TIMESTAMP;
static VeSnapshotsList snapshotsList = {};

/* utility functions */ 

static
int ValidateHeader(VersionsHeader *hdr)
{
	int success=0, i=0;

	assert(hdr);
	if (hdr->xptr[0]==0 || !IsValidTimestamp(hdr->creatorTs[0]))
	{
		/* must have at least one valid entry */ 
	}
	else
	{
		for (i=1; i<VE_VERSIONS_COUNT; ++i)
		{
			if (hdr->xptr[i]==0 || 
				!IsValidTimestamp(hdr->creatorTs[i]) || 
				hdr->creatorTs[i]>=hdr->creatorTs[i-1]) break;
		}
		while (i<VE_VERSIONS_COUNT && hdr->xptr[i]==0 && hdr->creatorTs[i]==INVALID_TIMESTAMP) ++i;
		success = (i == VE_VERSIONS_COUNT);
	}
	return success;
}

static 
void ResetHeader(VersionsHeader *hdr)
{
	int i=0;

	assert(hdr);
	for (i=0; i<VE_VERSIONS_COUNT; ++i)
	{
		hdr->xptr[i] = 0;
		hdr->creatorTs[i] = INVALID_TIMESTAMP;
	}
}

/* public API */ 

int VeInitialize()
{
	isInitialized = 1;
	return 1;
}

void VeQueryResourceDemand(VeResourceDemand *resourceDemand)
{
	assert(resourceDemand);
	resourceDemand->clientStateSize = sizeof (VeClientState);
	resourceDemand->bufferStateSize = 1;
}

int VeStartup(VeSetup *psetup)
{
	assert(psetup);
	setup = *psetup;
	ticket = setup.clientStateTicket;
	return 1;
}

void VeDeinitialize()
{
	isInitialized = 0;
}

int VeOnRegisterClient()
{
	static const VeClientState stateInitializer = {INVALID_TIMESTAMP, INVALID_TIMESTAMP, NULL, NULL};
	int success=0;
	VeClientState *state=NULL;
	VeSnapshot *snapshot=NULL;

	if (!ClGetCurrentStateBlock((void**)&state,ticket)) {}
	else 
	{
		*state = stateInitializer;
		if (isUsingSnapshot && (setup.flags & VE_SETUP_DISABLE_VERSIONS_FLAG))
		{
			WuSetLastErrorMacro(WUERR_VERSIONS_DISABLED);
		}
		else if (isUsingSnapshot && !GetSnapshotByTimestamp(&snapshotsList, &snapshot, NULL, snapshotTs)) 
		{
			/* invalid timestamp, error code already set by GetSnapshotByTimestamp() */ 
		}
		else if (isUsingSnapshot && snapshot->isDamaged)
		{
			WuSetLastErrorMacro(WUERR_UNABLE_TO_USE_DAMAGED_SNAPSHOT);
		}
		else if (isUsingSnapshot)
		{
			state->snapshotTs = snapshotTs;
			snapshot->occupancy ++;
			success = 1;
		}
		else if (setup.getTimestamp(&state->clientTs))
		{
			state->versionManipActions = new std::list<VeVersionManipAction>();
			*(snapshotsList.first.activeTransactionsTsEnd++) = state->clientTs;
			success = 1;
		}
	}
	return success;
}

int VeOnUnregisterClient()
{
	int success=0;
	TIMESTAMP *pos = NULL;
	VeClientState *state = NULL;
	VeSnapshot *snapshot = NULL;

	if (!ClGetCurrentStateBlock((void**)&state,ticket)) {}
	else if (IsPureQueryState(state) && !GetSnapshotByTimestamp(&snapshotsList, &snapshot, NULL, state->snapshotTs)) 
	{
		/* it is probably an internal error */ 
	}
	else if (IsUpdaterState(state))
	{
		delete state->versionManipActions;
		state->versionManipActions = NULL;
		pos = std::lower_bound(snapshotsList.first.activeTransactionsTsBegin,
							   snapshotsList.first.activeTransactionsTsEnd, 
							   state->clientTs);
		assert (pos<snapshotsList.first.activeTransactionsTsEnd && *pos==state->clientTs);
		memmove(pos,pos+1,CalcPtrDistance(pos,--snapshotsList.first.activeTransactionsTsEnd));
		success = 1;
	}
	else
	{
		assert(snapshot);
		snapshot->occupancy --;
		success = 1;
	}
	return success;
}

int VeLoadBuffer(LXPTR lxptr, int *pBufferId, int flags)
{
	VeClientState *state = NULL;
	VersionsHeader *header = NULL;
	VeMapping mapping = {};
	VeSnapshot *snapshot = NULL;
	int success = 0, bufferId = 0, ordinal = 0;

	assert(pBufferId);
	*pBufferId=0;

	if (!ClGetCurrentStateBlock((void**)&state,ticket)) {}
	else
	{
		if (!setup.loadBuffer(lxptr, &bufferId, 0)) {}
		else if (!setup.locateHeader(bufferId, &header)) {}
		else if (!ValidateHeader(header) || header->xptr[0] != lxptr)
		{
			WuSetLastErrorMacro(WUERR_PERS_DATA_VALIDATION_FAILED);
		}
		else
		{
			MakeMappingFromHeader(&snapshotsList,&mapping,header,INVALID_TIMESTAMP);
			if (IsUpdaterState(state))
			{
				if (mapping.isWorkingVersionPresent)
				{
					if (mapping.workingVersionTs != state->clientTs)
					{
						/* working version was created by another transaction */ 
						WuSetLastErrorMacro(WUERR_WORKING_VERSION_CREATED_BY_ALLY);
					}
					else if (mapping.validDataBegin > 0)
					{
						/* working version was created by the calling transaction, however it already deleted this block */ 
						WuSetLastErrorMacro(WUERR_NO_APROPRIATE_VERSION);
					}
					else
					{
						setup.protectBuffer(bufferId,32,0);
						success=1;
					}
				}
				else if (mapping.validDataBegin>1)
				{
					WuSetLastErrorMacro(WUERR_NO_APROPRIATE_VERSION);
				}
				else
				{
					int protection = 0;
					/*	if versions are disabled we always grant exclusive access to the calling transaction and
						hence avoid VeCreateVersion being called */ 
					if (setup.flags && VE_SETUP_DISABLE_VERSIONS_FLAG) protection = 32;
					setup.protectBuffer(bufferId,protection,0);
					success = 1;
				}
				if (success) *pBufferId = bufferId; 
			}
			else
			{
				ordinal=GetSnapshotByTimestamp(&snapshotsList,&snapshot,NULL,state->snapshotTs);
				assert(ordinal>=2);
				if (ordinal < mapping.validDataBegin || mapping.validDataEnd <= ordinal)
				{
					WuSetLastErrorMacro(WUERR_NO_APROPRIATE_VERSION);
				}
				else if (!setup.loadBuffer(mapping.version[ordinal], &bufferId, 0)) {}
				else if (!setup.locateHeader(bufferId, &header)) {}
				else if (!ValidateHeader(header) || header->xptr[0]!=lxptr)
				{
					WuSetLastErrorMacro(WUERR_PERS_DATA_VALIDATION_FAILED);
				}
				else
				{
					success=1;
					*pBufferId = bufferId;
				}
			}
		}
	}
	return success;
}

int VeInitBlockHeader(LXPTR xptr, int bufferId)
{
	int success = 0, okStatus = 0;
	VeClientState *state=NULL;
	VersionsHeader *header=NULL;

	if (!ClGetCurrentStateBlock((void**)&state,ticket)) {}
	else if (!setup.locateHeader(bufferId, &header)) {}
	else
	{
		ResetHeader(header,0);
		header->xptr[0] = xptr;
		header->creatorTs[0] = state->clientTs;
		success = 1;
	}
	return success;
}

int VeAllocBlock(LXPTR *lxptr)
{
	/* TODO: need special execution path when versions are disabled */ 
	XPTR xptr = 0;
	VeClientState *state = NULL;
	VersionsHeader *header = NULL;
	VeVersionManipAction action = {};
	int success=0, okStatus=0, bufferId=0;

	assert(lxptr);
	if (!ClGetCurrentStateBlock((void**)&state,ticket)) {}
	else
	{
		if (IsPureQueryState(state))
		{
			WuSetLastErrorMacro(WUERR_SNAPSHOTS_ARE_READ_ONLY);
		}
		else if (!setup.allocBlock(&xptr)) {}
		else if (!setup.loadBuffer(xptr,&bufferId,1)) {}
		else if (!VeInitBlockHeader(xptr, bufferId)) {}
		else
		{	
			okStatus = setup.markBufferDirty(bufferId, header, sizeof *header, 0);
			assert(okStatus);

			action.type = VE_ACTION_TYPE_CREATE_1ST_VERSION;
			action.lxptr = xptr;
			action.xptr = xptr;
			action.anchorTs = 0;

			state->versionManipActions->push_back(action);			
			success = 1;
			*lxptr = xptr;
		}
	}
	return success;
}

int VeCreateBlockVersion(LXPTR lxptr)
{
	VeClientState *state = NULL;
	VersionsHeader header = {}, *pheader = NULL;
	VeMapping mapping = {};
	VeVersionManipAction action = {};
	VeSnapshot *snapshot = NULL;
	XPTR xptr = 0;
	int success = 0, okStatus = 0, persOrdinal = 0, isSpecial = 0, bufferId = 0;

	if (!ClGetCurrentStateBlock((void**)&state,ticket)) {}
	else
	{
		if (setup.flags & VE_SETUP_DISABLE_VERSIONS_FLAG)
		{
			/* if versions are disabled, leave with error */ 
			WuSetLastErrorMacro(WUERR_VERSIONS_DISABLED);
		}
		else if (IsPureQueryState(state))
		{
			WuSetLastErrorMacro(WUERR_SNAPSHOTS_ARE_READ_ONLY);
		}
		else if (!setup.loadBuffer(lxptr,&bufferId,0)) {}
		else if (!setup.locateHeader(bufferId, &pheader)) {}
		else if (!ValidateHeader(pheader) || pheader->xptr[0]!=lxptr)
		{
			WuSetLastErrorMacro(WUERR_PERS_DATA_VALIDATION_FAILED);
		}
		else
		{
			header=*pheader;
			MakeMappingFromHeader(&snapshotsList,&mapping,pheader,~(TIMESTAMP)0);			
			if (mapping.validDataBegin>1)
			{
				WuSetLastErrorMacro(WUERR_NO_APROPRIATE_VERSION);
			}
			if (mapping.isWorkingVersionPresent)
			{
				if (mapping.workingVersionTs != state->clientTs)
				{
					WuSetLastErrorMacro(WUERR_WORKING_VERSION_CREATED_BY_ALLY);
				}
				else
				{
					WuSetLastErrorMacro(WUERR_WORKING_VERSION_ALREADY_CREATED);
				}
			}
			else if (!setup.allocBlock(&xptr)) {}
			else
			{
				persOrdinal=GetSnapshotByTimestamp(&snapshotsList,&snapshot,NULL,persSnapshotTs);
				isSpecial=(persOrdinal!=0 && mapping.version[1]==mapping.version[persOrdinal]);
				if (!setup.copyBlock(xptr,lxptr,isSpecial)) {}
				else if (!setup.loadBuffer(lxptr,&bufferId,0)) {}
				else if (!PushNewVersionIntoHeader(&snapshotsList, &header, xptr, state->clientTs)) {}
				else
				{
					if (!setup.locateHeader(bufferId,&pheader)) {}
					else if (isSpecial && !setup.onCompleteBlockRelocation(0,lxptr,xptr)) {}
					else
					{
						action.type = VE_ACTION_TYPE_CREATE_VERSION;
						action.lxptr = lxptr;
						action.xptr = xptr;
						action.anchorTs = mapping.anchorTs;

						state->versionManipActions->push_back(action);
						*pheader=header;
						okStatus = setup.markBufferDirty(bufferId, pheader, sizeof *pheader, 0);
						assert(okStatus);
						success=1;
					}					
				}
				if (!success) setup.freeBlock(xptr);
			}
		}
	}
	return success;
}

int VeFreeBlock(LXPTR lxptr)
{
	VeClientState *state = NULL;
	VeSnapshot *snapshot = NULL;
	VersionsHeader *header = NULL;
	VeMapping mapping = {};
	VeVersionManipAction action = {};
	int success = 0, okStatus = 0, bufferId = 0;

	if (!ClGetCurrentStateBlock((void**)&state,ticket)) {}
	else
	{
		if (IsPureQueryState(state))
		{
			WuSetLastErrorMacro(WUERR_SNAPSHOTS_ARE_READ_ONLY);
		}
		else if (!setup.loadBuffer(lxptr,&bufferId,0)) {}
		else if (!setup.locateHeader(bufferId, &header)) {}
		else if (!ValidateHeader(header) || header->xptr[0]!=lxptr)
		{
			WuSetLastErrorMacro(WUERR_PERS_DATA_VALIDATION_FAILED);
		}
		else
		{
			/*	TODO:
				We probably should not attempt to *unconditionally* put block to buffer.
				If the block is in buffer we can preciesly determine the snapshots that
				depend on the block. If block is not available from buffers, we should
				speculatively assume that every snapshot depends on the block. The
				checks should be done on the auxilary data structures, not the versions
				header.*/ 
			MakeMappingFromHeader(&snapshotsList,&mapping,header,INVALID_TIMESTAMP);
			if (!mapping.isWorkingVersionPresent)
			{
				WuSetLastErrorMacro(WUERR_OPERATION_REQUIRES_WORKING_VERSION);
			}
			else if (mapping.workingVersionTs != state->clientTs)
			{
				WuSetLastErrorMacro(WUERR_WORKING_VERSION_CREATED_BY_ALLY);
			}
			else if (mapping.validDataBegin>0)
			{
				/* we never get here */ 
				WuSetLastErrorMacro(WUERR_NO_APROPRIATE_VERSION);
			}
			else
			{
				GetSnapshotByOrdinalNumber(&snapshotsList,&snapshot,mapping.validDataEnd);
				assert(snapshot);

				action.type = VE_ACTION_TYPE_DELETE_VERSION;
				action.lxptr = lxptr;
				action.xptr = lxptr;
				action.anchorTs = snapshot->timestamp;

				state->versionManipActions->push_back(action);
				success = 1;
			}
		}
	}
	return success;
}

int VeOnCommit()
{
	/* When versions are disabled we have empty list thus no special code path required. */ 
	VeClientState *state=NULL;
	SnRequestForGc buf[VE_BUFSZ], *ibuf=buf, *ebuf=buf+VE_BUFSZ;
	std::list<VeVersionManipAction>::iterator i;
	int success = 0, failure = 0;

	if (!ClGetCurrentStateBlock((void**)&state,ticket)) {}
	else if (IsPureQueryState(state))
	{
		success=1;
	}
	else
	{
		i=state->versionManipActions->begin();
		while(i!=state->versionManipActions->end() && !failure)
		{
			if (ibuf<ebuf)
			{
				*ibuf++ = *i++;
			}
			else
			{
				failure = (0 == setup.submitRequestForGc(buf,ibuf-buf));
				ibuf=buf;
			}
		}
		if (!failure)
		{
			/* probably we have something left in the buf */ 
			failure = (0 == setup.submitRequestForGc(buf,ibuf-buf));
		}
		success = (0==failure);
	}	
	return success;
}

int VeOnRollback()
{	
	VeClientState *state=NULL;
	std::list<VeVersionManipAction>::iterator i;
	SnRequestForGc buf[VE_BUFSZ], *ibuf=buf, *ebuf=buf+VE_BUFSZ;
	int success = 0, failure = 0;

	if (!ClGetCurrentStateBlock((void**)&state,ticket)) {}
	else if (setup.flags & VE_SETUP_DISABLE_VERSIONS_FLAG)
	{
		/*	If versions are disabled we can't rollback and will report error. */ 
		WuSetLastErrorMacro(WUERR_VERSIONS_DISABLED);
	}
	else if (IsPureQueryState(state))
	{
		/* If the transaction is a pure query (ie it runs on read-only snapshot) we have nothing to revert. */ 
		success = 1;
	}
	else
	{
		i=state->versionManipActions->begin();
		while (i!=state->versionManipActions->end() && !failure)
		{
			if (ibuf<ebuf)
			{
				switch (i->type)
				{
				case VE_ACTION_TYPE_CREATE_VERSION:
					if (!setup.copyBlock(i->lxptr, i->xptr, 0)) 
					{
						failure=1;
						break;
					}
					if (i->anchorTs <= persSnapshotTs)
					{
						/* we do not need this block any more (same branch if this is the 1st version) */ 
				case VE_ACTION_TYPE_CREATE_1ST_VERSION:
						*ibuf=*i;
						ibuf->type = SN_REQUEST_DISCARD_VERSION;
						ibuf++;
					}
					else
					{
						/*	we should not emediately delete the block since it is already logged as a 
							relocated copy of the block from the persistent snapshot */ 
						*ibuf++=*i;
					}
					break;
				case VE_ACTION_TYPE_DELETE_VERSION:
					/* no action required */ 
					break;
				default:
					/* wrong type code */ 
					assert(0);
				}
				++i;
			}
			else
			{
				failure = (0 == setup.submitRequestForGc(buf,ibuf-buf));
				ibuf = buf;
			}
		}
		if (!failure) 
		{
			/* probably we have something left in buf */ 
			failure = (0 == setup.submitRequestForGc(buf,ibuf-buf));
		}
		success = (failure == 0);
	}
	return success;
}

void VeDbgDump(int reserved)
{
	fprintf(stderr,"VeDbgDump (not implemented)\n\n"); 
}
