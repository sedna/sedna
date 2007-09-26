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
#define VE_ACTION_TYPE_DELETE_VERSION			SN_REQUEST_TYPE_ZOMBIE

/*	version[0] - xptr of the working version
	version[1] - xptr of the last commited version
	version[2] - xptr of the version in #1 snapshot
	version[3] - xptr of the version in #2 snapshot
	version[n] - xptr of the version in #(n-1) snapshot

	workingVersionTs - timestamp of the working version (INVALID_TIMESTAMP if no working version exist)
	anchorTs - timestamp of the most recent snapshot NOT having the last commited version in it 
	[validDataBegin, validDataEnd) - the range of valid entries in versions[] array
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
	/*	this snapshot ts */ 
	TIMESTAMP *activeTransactionsTsBegin, *activeTransactionsTsEnd;
	/*	ordered array of active transactions timestamps */ 
	int occupancy;
	/*	total number of transactions using this snapshot */ 
	int isDamaged;
	/*	indicates whether this snapshot is usable by transactions (see PushNewVersionIntoHeader() for details) */ 
	VeSnapshot *next;
};

struct VeSnapshotsList
{
	void *mem;
	/*	call free() on the mem pointer to discard all dynamic data associated with this snapshotsList */ 
	size_t clientsCount;
	/*	clientsCount is the maximum number of simultaneously running transactions */ 
	VeSnapshot first, last, *freeList;	
	/*	first and last snapshot are the bogus ones:
		the first snapshot represents "now" (timestamp is INVALID_TIMESTAMP, all currently active transactions listed)
		the last snapshot represents "infinite past" (timestamp=0, no active transactions) 
		freeList is the linked list of snapshots structures that are currently unused */ 
};

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

/* global state */ 

static int isInitialized = 0;
static TICKET ticket = NULL;
static VeSetup setup = {};
static TIMESTAMP persSnapshotTs = INVALID_TIMESTAMP;
static VeSnapshotsList snapshotsList = {};

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

	lst->first.timestamp=INVALID_TIMESTAMP;
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
			si->timestamp=INVALID_TIMESTAMP;
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

/*	Finds a snapshot that has a timestamp less or equal to ts. 
	Ts must be a valid timestamp. Function always finds the
	matching snapshot and both result and prev are not NULL
	since two bogus snapshots exist - one with INVALID_TIMESTAMP
	at the head of the list (INVALID_TIMESTAMP is the greatest 
	integer value possible for TIMESTAMP type, inaceptable as 
	a timestamp) and one with timestamp=0 at the list end.
	Returns an ordinal number of the snapshot obtained
	(see GetSnapshotByOrdinalNumber for details) or 0 on failure. */ 
static
int FindSnapshotByTimestamp(VeSnapshotsList *lst,
							VeSnapshot **result, VeSnapshot **prev,
							TIMESTAMP ts)
{
	VeSnapshot *jt=NULL, *it=&(lst->first);
	int pos=0, success=0;

	assert(lst && result);
	*result=NULL;

	if (!IsValidTimestamp(ts))
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

/*	Finds a snapshot with timestamp equal to ts on the list.
	Only valid timestamps are accepted.
	Returns an ordinal number of the snapshot obtained
	(see GetSnapshotByOrdinalNumber for details) or 0 on failure.*/ 
static
int GetSnapshotByTimestamp(VeSnapshotsList *lst, 
						   VeSnapshot **result, VeSnapshot **prev, 
						   TIMESTAMP ts)
{
	int success=0, ordinal=0;
	VeSnapshot *it=NULL, *jt=NULL;
	assert(lst && result);
	*result=NULL;
	
	ordinal=FindSnapshotByTimestamp(lst,&it,&jt,ts);
	if (ordinal)
	{
		assert(it);
		if (it->timestamp != ts)
		{
			it=NULL;
			jt=NULL;
			WuSetLastErrorMacro(WUERR_NO_SNAPSHOT_WITH_THIS_TIMESTAMP);
		}
		else
		{
			success=ordinal;
		}
	}
	*result=it;
	if (prev) *prev=jt;
	return success;
}

/*	Gets the snapshot by the ordinal number. 
	Snapshots on the list are numbered sequentially, numbering starts from 1.
	The bogus snapshots are not hidden and can be obtained via this interface.
	If "normal" snapshots exist, the first "normal" snapshot ordinal is 2. */ 
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
		victim->timestamp = INVALID_TIMESTAMP;
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
	else if (!IsValidTimestamp(ts))
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
	if (hdr->xptr[0]==0 || !IsValidTimestamp(hdr->creatorTs[0]))
	{
		/* must have at least one valid entry */ 
	}
	else
	{
		for (i=1; i<VE_VERSIONS_COUNT; ++i)
		{
			if (hdr->xptr[i]==0 || !IsValidTimestamp(hdr->creatorTs[i]) || hdr->creatorTs[i]>=hdr->creatorTs[i-1]) break;
		}
		while (i<VE_VERSIONS_COUNT && hdr->xptr[i]==0 && hdr->creatorTs[i]==INVALID_TIMESTAMP) ++i;
		success = (i == VE_VERSIONS_COUNT);
	}
	return success;
}

/*	Vacant slots in the versions header is younger than any snapshots since their creatorTs is INVALID_TIMESTAMP
	which is the greatest possible TIMESTAMP value.*/ 
inline
static
int IsVersionYoungerThanSnapshot(VeSnapshot *sh, 
								 TIMESTAMP creatorTs)
{
	assert(sh);
	/* creatorTs==sh->timestamp - only if both variables have INVALID_TIMESTAMP value */ 
	return creatorTs>=sh->timestamp || std::binary_search(sh->activeTransactionsTsBegin, sh->activeTransactionsTsEnd, creatorTs);
}

static 
void ResetHeader(VersionsHeader *hdr, int start)
{
	int i=0;

	assert(hdr && start>=0);
	for (i=start; i<VE_VERSIONS_COUNT; ++i)
	{
		hdr->xptr[i] = 0;
		hdr->creatorTs[i] = INVALID_TIMESTAMP;
	}
}

static
void MakeMappingFromHeader(VeSnapshotsList *lst, 
						   VeMapping *map, 
						   VersionsHeader *hdr,
						   TIMESTAMP deletorTs)
{
	static const VeMapping initializer = {{},INVALID_TIMESTAMP,INVALID_TIMESTAMP,-1,-1,0};
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
		p=1; g=0;
		map->validDataBegin=1;
	}

	/* init anchorTs (the timestamp of the MOST RECENT snapshot NOT HAVING the last commited version in it) */ 
	for (; it->next && !IsVersionYoungerThanSnapshot(it,hdr->creatorTs[g]); it=it->next, ++p)
	{
		map->version[p] = hdr->xptr[g];
	}
	map->anchorTs = it->timestamp; ++g;

	/* fill remaining version slots */ 
	for (; it->next; it=it->next, ++p)
	{
		while (g<VE_VERSIONS_COUNT && IsVersionYoungerThanSnapshot(it,hdr->creatorTs[g])) ++g;
		if (g>=VE_VERSIONS_COUNT) break;
		map->version[p] = hdr->xptr[g];
	}	
	map->validDataEnd = p;
	
	/* update validDataBegin based on deletorTs */ 
	if (deletorTs == INVALID_TIMESTAMP) {;}
	else if (IsVersionYoungerThanSnapshot(&(lst->first),deletorTs))
	{
		/*	Deletion was performed by the currently active transaction. We allow to perform the deletion
			without explicit working version creation, however a last commited version must exist if working
			version is not present. */ 
		assert(map->validDataBegin <= 1);
		map->version[0]=0;
		map->validDataBegin=1;
	}
	else
	{
		/*	Deletion was performed in the previous epoch. */ 
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
			hdrResult.creatorTs[p]=INVALID_TIMESTAMP;
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
		if (initialPersSnapshotTs == INVALID_TIMESTAMP)
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
		persSnapshotTs = INVALID_TIMESTAMP;
	}
	isInitialized = 0;
}

int VeOnRegisterClient(int isUsingSnapshot, TIMESTAMP snapshotTs)
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
				failure = (0 == setup.submitRequestForGc(INVALID_TIMESTAMP,buf,ibuf-buf));
				ibuf=buf;
			}
		}
		if (!failure)
		{
			/* probably we have something left in the buf */ 
			failure = (0 == setup.submitRequestForGc(INVALID_TIMESTAMP,buf,ibuf-buf));
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
				failure = (0 == setup.submitRequestForGc(INVALID_TIMESTAMP,buf,ibuf-buf));
				ibuf = buf;
			}
		}
		if (!failure) 
		{
			/* probably we have something left in buf */ 
			failure = (0 == setup.submitRequestForGc(INVALID_TIMESTAMP,buf,ibuf-buf));
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

	if (discardedTs!=INVALID_TIMESTAMP && discardedTs==persSnapshotTs)
	{
		WuSetLastErrorMacro(WUERR_UNABLE_TO_DISCARD_SPECIAL_SNAPSHOT);
	}
	else if (discardedTs!=INVALID_TIMESTAMP && !DiscardSnapshot(&snapshotsList, discardedTs)) 
	{
	}
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
	*timestamp=INVALID_TIMESTAMP;
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
