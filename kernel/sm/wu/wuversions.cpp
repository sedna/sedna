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

#define VE_ACTION_TYPE_CREATE_1ST_VERSION		SN_REQUEST_TYPE_NOP
#define VE_ACTION_TYPE_CREATE_VERSION			SN_REQUEST_TYPE_OLDER_VERSION
#define VE_ACTION_TYPE_MARK_VERSION_ZOMBIE		SN_REQUEST_TYPE_ZOMBIE

/*	version[0] - xptr of the working version
	version[1] - xptr of the last commited version
	version[2] - xptr of the version in #1 snapshot
	version[3] - xptr of the version in #2 snapshot
	version[n] - xptr of the version in #(n-1) snapshot

	workingVersionTs - timestamp of the working version (0 if no working version exist)
	anchorTs - timestamp of the most recent snapshot NOT HAVING last commited version in it
	*/ 
struct VeMapping
{
	XPTR version[VE_SNAPSHOTS_COUNT+2];
	TIMESTAMP workingVersionTs, anchorTs;
	int validDataBegin;
	int validDataEnd;
	int isWorkingVersionPresent;
};

struct VeSnapshot
{
	TIMESTAMP timestamp;	
	TIMESTAMP *activeTransactionsTsBegin, *activeTransactionsTsEnd;
	int occupancy;
	int isDamaged;
	VeSnapshot *next;
};

struct VeSnapshotsList
{
	void *mem;
	size_t clientsCount;
	VeSnapshot first, last, *freeList;	
};

struct VeClientState
{
	union
	{
		TIMESTAMP snapshotTs;
		TIMESTAMP clientTs;
	};
	std::list<VeVersionManipAction> *versionManipActions;
};

/*	We need 2 properties: (1) deleting a block version doesn't require writing either 
	the requested version or other versions of the same block back to HDD and
	(2) version deletion doesn't require puting version into buffer. These requirement are
	essential to reduce the additional buffer manager load imposed by versions as much as possible.

	It means that we CAN NOT use data in version header to perform all desired correctness checks on the 
	operations issued by transactions. The list of operation along with the correctness checks that
	require auxilary data structures follows.

	[VeAllocateBlock]	- None.
	[VeGetBlockVersion] - Is the version selected by version selection algoritm a "live" version or the
						  block was already freed and only snapshot versions remain?
	[VeCreateBlockVersion] - Same as for VeGetBlockVersion.
	[VeFreeBlock]		- Wasn't the block already freed?
						  Do we have rights to free this block (no other active transaction 
						  created a working version)?
						  Assuming that block versions are divided into "master" version and
						  "slave" versions indistinguishable by xptrs; is VeFreeBlock called on
						  the master version?

	The well-behaving transactions should never perform incorrect operations; the correctness 
	checks are primarily intended for debugging purposes. They may be disabled to improve the 
	performance.

	The solution is to maintain a hash table of VeRestrictionEntry structures (xptr is the key).
	Every time a transaction creates a working versions (technically speaking, it creates another
	slave version and initialises it after the master version) 2 entries are posted to the hash table,
	one for the master version (deletorTs=~(TIMESTAMP)0, creatorId=ClGetCurrentClientId()), the other for
	the slave version (deletorTs=~(TIMESTAMP)0, creatorId=-1). When the transaction commits entries
	for master versions are removed. Entries for slave versions are removed as soon as these versions
	are discarded due to snapshots advance. Whenever the master version is freed an entry
	(deletorTs=VeGetCurrentTs(), creatorId=ClGetCurrentClientId()) is posted (possibly overwriting an
	existing entry).

	Looking up deletorTs in the hash table by the given xptr tells whether the version is "live" or
	not. This check also prevents multiple VeFreeBlock on the same xptr. Non-negative creatorId tells
	us that a working version was created. If creatorId is different from the transaction's own id 
	the transaction is not permited to free the block. Finally if an entry for the given xptr is 
	found and creatorId==-1 the xptr indentifies a slave version which is not allowed to be freed.

*/ 

struct VeRestrictionEntry
{
	XPTR xptr;
	TIMESTAMP deletorTs;
	int creatorId;
};

/* global state */ 

static int isInitialized = 0;
static TICKET ticket=NULL;
static VeSetup setup;
static TIMESTAMP persSnapshotTs=0;
static VeSnapshotsList snapshotsList;

/* utility functions */ 

static
inline 
int IsUpdaterState(VeClientState *state)
{
	assert(state);
	return state->versionManipActions != NULL;
};

static
inline 
int IsPureQueryState(VeClientState *state)
{
	assert(state);
	return state->versionManipActions == NULL;
};

static void
ResetSnapshotsList(VeSnapshotsList *lst)
{
	assert(lst);
	lst->mem=NULL;
	lst->clientsCount=0;
	lst->freeList=NULL;

	lst->first.timestamp=~(TIMESTAMP)0;
	lst->first.activeTransactionsTsBegin=NULL;
	lst->first.activeTransactionsTsEnd=NULL;
	lst->first.occupancy=INT_MAX;
	lst->first.isDamaged=0;
	lst->first.next=&(lst->last);
	
	lst->last.timestamp=0;
	lst->last.activeTransactionsTsBegin=NULL;
	lst->last.activeTransactionsTsEnd=NULL;
	lst->last.occupancy=INT_MAX;
	lst->last.isDamaged=0;
	lst->last.next=NULL;
}

static void
DeinitSnapshotsList(VeSnapshotsList *lst)
{
	assert(lst);
	assert(lst->first.occupancy==INT_MAX && lst->last.occupancy==INT_MAX);
	free(lst->mem);
	ResetSnapshotsList(lst);
}

static int 
InitSnapshotsList(VeSnapshotsList *lst, size_t clientsCount)
{
	int success=0;
	size_t snapshotsSize=0, timestampsSize=0;
	TIMESTAMP *ti=NULL;
	VeSnapshot *si=NULL;
	int i=0;

	assert(lst);
	snapshotsSize=(sizeof(VeSnapshot))*VE_SNAPSHOTS_COUNT;
	timestampsSize=(sizeof(TIMESTAMP)*clientsCount)*(VE_SNAPSHOTS_COUNT+1);
	ResetSnapshotsList(lst);

	if (clientsCount>VE_MAX_CLIENTS_COUNT)
	{
		WuSetLastErrorMacro(WUERR_MAX_NUMBER_OF_CLIENTS_EXCEEDED);
	}
	else if(!(lst->mem=malloc(snapshotsSize+timestampsSize)))
	{
		WuSetLastErrorMacro(WUERR_NO_MEMORY);
	}
	else
	{
		lst->clientsCount=clientsCount;
		si=(VeSnapshot*)lst->mem;
		ti=(TIMESTAMP*)OffsetPtr(lst->mem,snapshotsSize);
		memset(ti,0,timestampsSize);
		
		lst->first.activeTransactionsTsBegin=lst->first.activeTransactionsTsEnd=ti; ti+=clientsCount;
		lst->freeList=si;

		for (i=0;i<VE_SNAPSHOTS_COUNT;++i,++si)
		{
			si->timestamp=0;
			si->activeTransactionsTsBegin=si->activeTransactionsTsEnd=ti; 
			ti+=clientsCount;
			si->occupancy=0;
			si->isDamaged=0;
			si->next=si+1;
		}
		si[-1].next=NULL;
		success=1;
	}
	return success;
}

static
int FindSnapshotByTimestamp(VeSnapshotsList *lst,
							VeSnapshot **result, VeSnapshot **prev,
							TIMESTAMP ts)
{
	VeSnapshot *jt=NULL, *it=&(lst->first);
	int pos=0, success=0;

	assert(lst && result);
	*result=NULL;

	if (ts==~(TIMESTAMP)0)
	{
		WuSetLastErrorMacro(WUERR_BAD_TIMESTAMP);
	}
	else 	
	{
		while(it && it->timestamp>ts)
		{
			jt=it;
			it=it->next;
			++pos;
		}
		assert(it);
		*result=it;
		if (prev) *prev=jt;
		success = 1+pos;
	}
	return success;
}

static
int GetSnapshotByTimestamp(VeSnapshotsList *lst, 
						   VeSnapshot **result, VeSnapshot **prev, 
						   TIMESTAMP ts)
{
	int success=0;
	VeSnapshot *it=NULL, *jt=NULL;
	assert(lst && result);
	*result=NULL;
	
	if (ts == 0)
	{
		WuSetLastErrorMacro(WUERR_BAD_TIMESTAMP);
	}
	else
	{
		success=FindSnapshotByTimestamp(lst,&it,&jt,ts);
		assert(it);
		if (it->timestamp != ts)
		{
			it=NULL;
			jt=NULL;
			success=0;
			WuSetLastErrorMacro(WUERR_NO_SNAPSHOT_WITH_THIS_TIMESTAMP);
		}
	}
	*result=it;
	if (prev) *prev=jt;
	return success;
}

static
int GetSnapshotByOrdinalNumber(VeSnapshotsList *lst,
							   VeSnapshot **result,
							   int ordinal)
{
	int success=0;
	VeSnapshot *it=&(lst->first);
	assert(lst && result);
	while (it && ordinal>1) 
	{
		it=it->next;
		--ordinal;
	}
	if (!it || ordinal!=1)
	{
		WuSetLastErrorMacro(WUERR_NO_SNAPSHOT_WITH_THIS_ORDINAL);
	}
	else
	{
		*result=it;
		success=1;
	}
	return success;
}

static 
int DiscardSnapshot(VeSnapshotsList *lst, TIMESTAMP ts)
{
	int success=0; 
	VeSnapshot *beforeVictim=NULL, *victim=NULL;
	assert(lst);
	if (!GetSnapshotByTimestamp(lst,&victim,&beforeVictim,ts))
	{
		; /* something wrong */ 
	}
	else if (victim->occupancy>0)
	{
		WuSetLastErrorMacro(WUERR_UNABLE_TO_DISCARD_SNAPSHOT_IN_USE);
	}
	else
	{
		assert(beforeVictim && victim && victim->next);
		beforeVictim->next = victim->next;
		victim->next = lst->freeList;
		lst->freeList = victim;
		victim->timestamp = 0;
		victim->occupancy = 0;
		victim->isDamaged = 0;
		memset(victim->activeTransactionsTsBegin,0,lst->clientsCount*sizeof(TIMESTAMP));
		victim->activeTransactionsTsEnd=victim->activeTransactionsTsBegin;
		success=1;
	}
	return success;
}

static 
int CreateSnapshot(VeSnapshotsList *lst, TIMESTAMP ts)
{
	int success=0;
	VeSnapshot *beforeInsertionPt=NULL, *insertionPt=NULL, *newSh=NULL;
	assert(lst);
	if (!lst->freeList)
	{
		WuSetLastErrorMacro(WUERR_MAX_NUMBER_OF_SNAPSHOTS_EXCEEDED);
	}
	else if (ts==0 || ts==~(TIMESTAMP)0)
	{
		WuSetLastErrorMacro(WUERR_BAD_TIMESTAMP);
	}
	else
	{
		FindSnapshotByTimestamp(lst,&insertionPt,&beforeInsertionPt,ts);
		assert(insertionPt && beforeInsertionPt);
		if (insertionPt->timestamp==ts)
		{
			WuSetLastErrorMacro(WUERR_SNAPSHOT_WITH_THIS_TIMESTAMP_ALREADY_EXISTS);
		}
		else
		{
			newSh = lst->freeList;
			lst->freeList = lst->freeList->next;
			beforeInsertionPt->next = newSh;
			newSh->next = insertionPt;
			newSh->timestamp = ts;
			success = 1;
		}
	}
	return success;
}

static
int ValidateHeader(VersionsHeader *hdr)
{
	int success=0, i=0;

	assert(hdr);
	if (hdr->xptr[0]==0 || hdr->creatorTs[0]==0 || hdr->creatorTs[0]==~(TIMESTAMP)0)
	{
		/* must have at least one valid entry */ 
	}
	else
	{
		for (i=1; i<VE_VERSIONS_COUNT; ++i)
		{
			if (hdr->xptr[i]==0 || hdr->creatorTs[i]>=hdr->creatorTs[i-1] || hdr->creatorTs[i]==0) break;
		}
		while (i<VE_VERSIONS_COUNT && hdr->xptr[i]==0 && hdr->creatorTs[i]==~(TIMESTAMP)0) ++i;
		success = (i == VE_VERSIONS_COUNT);
	}
	return success;
}

inline
static
int IsVersionYoungerThanSnapshot(VeSnapshot *sh, 
								 TIMESTAMP creatorTs)
{
	assert(sh);
	return creatorTs>sh->timestamp || std::binary_search(sh->activeTransactionsTsBegin, sh->activeTransactionsTsEnd, creatorTs);
}

static 
void ResetHeader(VersionsHeader *hdr, int start)
{
	int i=0;

	assert(hdr && start>=0);
	for (i=start; i<VE_VERSIONS_COUNT; ++i)
	{
		hdr->xptr[i] = 0;
		hdr->creatorTs[i] = ~(TIMESTAMP)0;
	}
}

static
void MakeMappingFromHeader(VeSnapshotsList *lst, 
						   VeMapping *map, 
						   VersionsHeader *hdr,
						   TIMESTAMP deletorTs)
{
	static const VeMapping initializer = {{},0,~(TIMESTAMP)0,-1,-1,0};
	VeSnapshot *it=NULL;
	int i=0, g=0, p=0;

	assert(lst && map && hdr && ValidateHeader(hdr));
	*map=initializer;

	it=&(lst->first);
	if (IsVersionYoungerThanSnapshot(it,hdr->creatorTs[0]))
	{
		/*	have working version */ 
		map->version[0] = hdr->xptr[0];
		map->isWorkingVersionPresent = 1;
		map->workingVersionTs = hdr->creatorTs[0];
		map->validDataBegin=0;
		p=1; g=1;
	}
	else
	{
		/*	have no working version, need anchorTs (the timestamp of the MOST RECENT snapshot 
			NOT HAVING the last commited version in it) */ 
		p=1; g=0;
		map->validDataBegin=1;
		for (; it->next && !IsVersionYoungerThanSnapshot(it,hdr->creatorTs[0]); it=it->next, ++p)
		{
			map->version[p] = hdr->xptr[0];
		}
		map->anchorTs = it->timestamp;
		g=1;
	}

	/* fill remaining version slots */ 
	for (; it->next; it=it->next, ++p)
	{
		while (g<VE_VERSIONS_COUNT && IsVersionYoungerThanSnapshot(it,hdr->creatorTs[g])) ++g;
		if (g>=VE_VERSIONS_COUNT) break;
		map->version[p] = hdr->xptr[g];
	}	
	map->validDataEnd = p;
	
	/* update validDataBegin based on deletorTs */ 
	if (deletorTs == ~(TIMESTAMP)0) {;}
	else if (IsVersionYoungerThanSnapshot(&(lst->first),deletorTs))
	{
		assert(map->isWorkingVersionPresent && deletorTs == map->workingVersionTs);
		map->version[0]=0;
		map->validDataBegin=1;
	}
	else
	{
		assert(!map->isWorkingVersionPresent);
		map->version[0]=0; p=1; it=lst->first.next;
		while (it->next && !IsVersionYoungerThanSnapshot(it,deletorTs) && p<map->validDataEnd)
		{
			map->version[p]=0;
			++p; it=it->next;
		}
		map->validDataBegin=p;
	}
}

static
int PushNewVersionIntoHeader(VeSnapshotsList *lst,
							 VersionsHeader *hdr,
							 XPTR xptr,
							 TIMESTAMP creatorTs)
{
	VeSnapshot *it = NULL;
	VersionsHeader hdrResult = {};
	int success = 0, failure = 0, p = 0, g = 0;

	assert(lst && hdr);
	
	/* compact entries in the hdr */ 
	if (IsVersionYoungerThanSnapshot(&(lst->first),hdr->creatorTs[0]))
	{
		WuSetLastErrorMacro(WUERR_WORKING_VERSION_ALREADY_CREATED);
		failure = 1;
	}
	else
	{
		g=0; p=1;
		for (it=&(lst->first); it->next && !failure; it=it->next)
		{
			/* for each neighboring snapshots pair keep the most recent version in-between */ 
			while (g<VE_VERSIONS_COUNT && IsVersionYoungerThanSnapshot(it,hdr->creatorTs[g])) ++g;
			if (g==VE_VERSIONS_COUNT) break;
			if (IsVersionYoungerThanSnapshot(it->next,hdr->creatorTs[g]))
			{
				if (p<VE_VERSIONS_COUNT)
				{
					hdrResult.xptr[p] = hdr->xptr[g];
					hdrResult.creatorTs[p] = hdr->creatorTs[g];
					++p;
				}
				else
				{
					/* no room left in hdrResult to put version info, marking the snapshot as damaged */ 
					if (it->occupancy > 0)
					{
						WuSetLastErrorMacro(WUERR_UNABLE_TO_DAMAGE_SNAPSHOT_IN_USE);
						failure=1;
					}
					else
					{
						it->isDamaged = 1;
					}
				}
			}
		}
		/* filling the tail properly */ 
		for (; p<VE_VERSIONS_COUNT; ++p)
		{
			hdrResult.xptr[p]=0;
			hdrResult.creatorTs[p]=~(TIMESTAMP)0;
		}
		/* finally putting xptr and creatorTs in the header */ 
		hdrResult.creatorTs[0] = creatorTs;
		hdrResult.xptr[0] = hdrResult.xptr[1];
		hdrResult.xptr[1] = xptr;
	}

	if (!failure)
	{
		*hdr = hdrResult;
		success = 1;
	}
	return success;
}

/* public API */ 

int VeInitialize()
{
	ResetSnapshotsList(&snapshotsList);
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
	TIMESTAMP curTs=0, initialPersSnapshotTs=0;
	int success = 0;

	assert(psetup);
	setup = *psetup;
	ticket = setup.clientStateTicket;
	initialPersSnapshotTs = setup.initialPersSnapshotTs;
	if (!InitSnapshotsList(&snapshotsList, ClQueryMaxClientsCount())) {}
	else
	{		
		if (initialPersSnapshotTs == 0)
		{
			success=1;
		}
		else
		{
			if (!setup.getTimestamp(&curTs) || initialPersSnapshotTs >= curTs)
			{
				WuSetLastErrorMacro(WUERR_BAD_TIMESTAMP);
			}
			else if (!CreateSnapshot(&snapshotsList,initialPersSnapshotTs)){}
			else
			{
				persSnapshotTs=initialPersSnapshotTs;
				success=1;
			}
		}
	}
	return success;
}

void VeDeinitialize()
{
	if (isInitialized)
	{
		DeinitSnapshotsList(&snapshotsList);
	}
	isInitialized = 0;
}

int VeOnRegisterClient(int isUsingSnapshot, TIMESTAMP snapshotTs)
{
	int success=0;
	VeClientState *state=NULL;
	VeSnapshot *snapshot=NULL;

	if (!ClGetCurrentStateBlock((void**)&state,ticket)) {}
	else if (isUsingSnapshot && (0==snapshotTs || ~(TIMESTAMP)0==snapshotTs))
	{
		WuSetLastErrorMacro(WUERR_BAD_TIMESTAMP);
	}
	else if (isUsingSnapshot && !GetSnapshotByTimestamp(&snapshotsList, &snapshot, NULL, snapshotTs)) {}
	else if (isUsingSnapshot && snapshot->isDamaged)
	{
		WuSetLastErrorMacro(WUERR_UNABLE_TO_USE_DAMAGED_SNAPSHOT);
	}
	else if (isUsingSnapshot)
	{
		state->versionManipActions = NULL;
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
	return success;
}

int VeOnUnregisterClient()
{
	int success=0;
	TIMESTAMP *pos = NULL;
	VeClientState *state=NULL;
	VeSnapshot *snapshot=NULL;

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
	VeMapping mapping;
	VeSnapshot *snapshot=NULL;
	int success = 0, isReady = 0, bufferId=0, ordinal=0;

	assert(pBufferId);
	*pBufferId=0;

	if (!ClGetCurrentStateBlock((void**)&state,ticket)) {}
	else
	{
		ClIsClientReady(&isReady,ClGetCurrentClientId());
		if (!isReady)
		{
			WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
		}
		else if (!setup.loadBuffer(lxptr, &bufferId, 0)) {}
		else if (!setup.locateHeader(bufferId, &header)) {}
		else if (!ValidateHeader(header) || header->xptr[0] != lxptr)
		{
			WuSetLastErrorMacro(WUERR_PERS_DATA_VALIDATION_FAILED);
		}
		else
		{
			MakeMappingFromHeader(&snapshotsList,&mapping,header,~(TIMESTAMP)0);
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
						/* working version was created by the calling transaction, however it already deleted the block */ 
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
					success = 1;
					setup.protectBuffer(bufferId,0,0);
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
	XPTR xptr=0;
	VeClientState *state=NULL;
	VersionsHeader *header=NULL;
	VeVersionManipAction action={};
	int success=0, okStatus=0, bufferId=0, isReady=0;

	assert(lxptr);
	if (!ClGetCurrentStateBlock((void**)&state,ticket)) {}
	else
	{
		okStatus = ClIsClientReady(&isReady,ClGetCurrentClientId()); assert(okStatus);
		if (!isReady)
		{
			WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
		}
		else if (IsPureQueryState(state))
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

int VeCreateVersion(LXPTR lxptr)
{
	VeClientState *state=NULL;
	VersionsHeader header={}, *pheader=NULL;
	VeMapping mapping={};
	VeVersionManipAction action={};
	VeSnapshot *snapshot=NULL;
	XPTR xptr=0;
	int success = 0, okStatus = 0, isReady = 0, persOrdinal = 0, isSpecial = 0, bufferId = 0;

	if (!ClGetCurrentStateBlock((void**)&state,ticket)) {}
	else
	{
		okStatus = ClIsClientReady(&isReady,ClGetCurrentClientId()); assert(okStatus);
		if (!isReady)
		{
			WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
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
					else if (isSpecial && !setup.onCompleteBlockRelocation(ClGetCurrentClientId(),lxptr,xptr)) {}
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
	VeClientState *state=NULL;
	VeSnapshot *snapshot = NULL;
	VersionsHeader *header = NULL;
	VeMapping mapping = {};
	VeVersionManipAction action = {};
	int success = 0, okStatus = 0, isReady = 0, bufferId = 0;

	if (!ClGetCurrentStateBlock((void**)&state,ticket)) {}
	else
	{
		okStatus = ClIsClientReady(&isReady,ClGetCurrentClientId()); assert(okStatus);
		if (!isReady)
		{
			WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
		}
		else if (IsPureQueryState(state))
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
			MakeMappingFromHeader(&snapshotsList,&mapping,header,~(TIMESTAMP)0);
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

				action.type = VE_ACTION_TYPE_MARK_VERSION_ZOMBIE;
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
				failure = (0 == setup.acceptRequestForGc(~(TIMESTAMP)0,buf,ibuf-buf));
				ibuf=buf;
			}
		}
		if (!failure)
		{
			/* probably we have something left in buf */ 
			failure = (0 == setup.acceptRequestForGc(~(TIMESTAMP)0,buf,ibuf-buf));
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
	else if (IsPureQueryState(state))
	{
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
						ibuf->type = SN_REQUEST_TYPE_ALWAYS_DELETE;
						ibuf++;
					}
					else
					{
						/*	we should not emediately delete the block since it is already logged as a 
							relocated copy of the block from the persistent snapshot */ 
						*ibuf++=*i;
					}
					break;
				case VE_ACTION_TYPE_MARK_VERSION_ZOMBIE:
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
				failure = (0 == setup.acceptRequestForGc(~(TIMESTAMP)0,buf,ibuf-buf));
				ibuf = buf;
			}
		}
		if (!failure) 
		{
			/* probably we have something left in buf */ 
			failure = (0 == setup.acceptRequestForGc(~(TIMESTAMP)0,buf,ibuf-buf));
		}
		success = (failure == 0);
	}
	return success;
}

int VeOnSnapshotsAdvanced(TIMESTAMP snapshotTs, TIMESTAMP discardedTs)
{
	VeSnapshot *snapshot = NULL;
	size_t size = 0;
	int success=0;

	if (discardedTs!=0 && !DiscardSnapshot(&snapshotsList, discardedTs)) {}
	else if (!CreateSnapshot(&snapshotsList, snapshotTs)) {}
	else if (!GetSnapshotByTimestamp(&snapshotsList, &snapshot, NULL, snapshotTs)) {}
	else
	{
		size = (size_t)(snapshotsList.first.activeTransactionsTsEnd-snapshotsList.first.activeTransactionsTsBegin);
		snapshot->activeTransactionsTsEnd = snapshot->activeTransactionsTsBegin+size;
		memcpy(snapshot->activeTransactionsTsBegin,
			   snapshotsList.first.activeTransactionsTsBegin,
			   size*sizeof(TIMESTAMP));

		success=1;
	}
	return success;
}

int VeOnBeginCheckpoint()
{
	return 1;
}

int VeOnCompleteCheckpoint(TIMESTAMP persistentTs)
{
	VeSnapshot *snapshot = NULL;
	int success = 0;

	if (GetSnapshotByTimestamp(&snapshotsList,&snapshot,NULL,persistentTs))
	{
		persSnapshotTs=persistentTs;
		success = 1;
	}
	return success;
}

int VeOnFlushBlock(int bufferId)
{
	return 1;
}

int VeGetCurrentTs(TIMESTAMP *timestamp)
{
	int success=0;
	VeClientState *state=NULL;

	assert(timestamp);
	*timestamp=0;
	if (ClGetCurrentStateBlock((void**)state, ticket) && IsUpdaterState(state))
	{
		*timestamp = state->clientTs;
	}
	return 0;
}

void VeDbgDump(int reserved)
{
	fprintf(stderr,"VeDbgDump (not implemented)\n\n"); 
}
