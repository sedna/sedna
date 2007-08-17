#define __WUDANG_SOURCES__

#include <assert.h>
#include <limits.h>
#include <list>
#include <stdio.h>
#include "wuaux.h"
#include "wuerr.h"
#include "wuversions.h"
#include "wuclients.h"

#define VE_MAX_CLIENTS_COUNT	0x10000
#define VE_BUFSZ				1024

struct VeMappingEntry
{
	XPTR xptr;
	TIMESTAMP creatorTs;
	int creator;
};

struct VeMapping
{
	VeMappingEntry version[VE_SNAPSHOTS_COUNT+2];
	int anchor;
	int validDataBegin;
	int validDataEnd;
	int publicDataBegin;
	int publicDataEnd;
};

struct VeSnapshot
{
	TIMESTAMP timestamp;	
	TIMESTAMP *clientTs;
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
	std::list<SnRequestForGc> *pushedVersions; /* NULL if read-only transaction */ 
};

/* global state */ 

static int isInitialized = 0;
static TICKET ticket=NULL;
static VeSetup setup;
static TIMESTAMP persSnapshotTs=0;
static VeSnapshotsList snapshotsList;

/* utility functions */ 

static void
ResetSnapshotsList(VeSnapshotsList *lst)
{
	assert(lst);
	lst->mem=NULL;
	lst->clientsCount=0;
	lst->freeList=NULL;

	lst->first.timestamp=~(TIMESTAMP)0;
	lst->first.clientTs=NULL;
	lst->first.occupancy=INT_MAX;
	lst->first.isDamaged=0;
	lst->first.next=&(lst->last);
	
	lst->last.timestamp=0;
	lst->last.clientTs=NULL;
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
		
		lst->first.clientTs=ti; ti+=clientsCount;
		lst->freeList=si;

		for (i=0;i<VE_SNAPSHOTS_COUNT;++i,++si)
		{
			si->timestamp=0;
			si->clientTs=ti; ti+=clientsCount;
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
		memset(victim->clientTs,0,lst->clientsCount*sizeof(TIMESTAMP));
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
	int success=1, i=0;

	return success;
}

static
int ValidateMapping(VeMapping *map)
{
	int success=1, i=0;

	return success;
}

inline
static
int IsVersionYoungerThanSnapshot(VeSnapshotsList *lst, 
								 VeSnapshot *sh, 
								 TIMESTAMP creatorTs,
								 int creator)
{
	assert(sh && lst);
	/*	Creator MAY be greater than lst->clientsCount if database restarted and clientsCount 
		decreased (someone rebuilt the binaries for instance). Creator MAY be -1 if
		we are checking "nonexistent" version against the snapshot. */ 
	return 
		creatorTs>sh->timestamp || 
		creator>=0 && creator<lst->clientsCount && creatorTs==sh->clientTs[creator];
}

static
void ResetMapping(VeMapping *map)
{
	static const VeMappingEntry initC = {0,0,-1};
	int i=0;

	assert(map);
	map->anchor = 0;
	map->validDataBegin = 0;
	map->validDataEnd = 0;
	map->publicDataBegin = 0;
	map->publicDataEnd = 0;
	for (i=0; i<VE_SNAPSHOTS_COUNT+2; ++i) map->version[i] = initC;
}

static 
void ResetHeader(VersionsHeader *hdr, int start)
{
	int i=0;

	assert(hdr && start>=0);
	for (i=start; i<VE_VERSIONS_COUNT; ++i)
	{
		hdr->xptr[i] = 0;
		hdr->creatorTs[i] = 0;
		hdr->creator[i] = -1;
	}
	hdr->isZombie = 0;
}

static
void MakeMappingFromHeader(VeSnapshotsList *lst, 
						   VeMapping *map, 
						   VersionsHeader *hdr)
{
	static const VeMappingEntry initC = {0,0,-1};
	VeSnapshot *it=NULL;
	int i=0, g=0, p=0;

	assert(lst && map && hdr && ValidateHeader(hdr));
	ResetMapping(map);

	/* process head */ 
	it=&(lst->first);
	if (IsVersionYoungerThanSnapshot(lst,it,hdr->creatorTs[0],hdr->creator[0]))
	{
		map->version[0].xptr = hdr->xptr[0]; 
		map->version[0].creatorTs = hdr->creatorTs[0];
		map->version[0].creator = hdr->creator[0];
		p=1; g=1;
		map->validDataBegin=0;
	}
	else
	{
		map->version[0]=initC; p=1; g=0;
		if (!hdr->isZombie) { map->validDataBegin=1; }
		else
		{
			assert(it && it->next);
			while(p<VE_SNAPSHOTS_COUNT+2 && !IsVersionYoungerThanSnapshot(lst,it->next,hdr->creatorTs[0],hdr->creator[0]))
			{
				map->version[p] = initC;
				++p;
				it = it->next;
				assert(it && it->next);
			}
			map->validDataBegin = p;
		}
	}

	/* process tail */ 
	for (; it->next; it=it->next, ++p)
	{
		while (g<VE_VERSIONS_COUNT && IsVersionYoungerThanSnapshot(lst,it,hdr->creatorTs[g],hdr->creator[g])) ++g;
		if (g>=VE_VERSIONS_COUNT) break;
		if (g==0) map->anchor=p+1;
		map->version[p].xptr = hdr->xptr[g];
		map->version[p].creatorTs = hdr->creatorTs[g];
		map->version[p].creator = hdr->creator[g];
	}	

	/* misc jobs */ 
	map->validDataEnd = p;
	for(; p<VE_SNAPSHOTS_COUNT+2; ++p) map->version[p] = initC;
	map->publicDataBegin = map->validDataBegin+(hdr->isZombie ? 1 : 0);
	map->publicDataEnd = map->validDataEnd;

	/* fix anchor */ 
	if (map->publicDataBegin>1) map->anchor=0;
}

static
int PushNewVersionIntoHeader(VeSnapshotsList *lst,
							 VersionsHeader *hdr,
							 XPTR xptr,
							 TIMESTAMP creatorTs,
							 int creator)
{
	VeSnapshot *snapshot = NULL;
	int success = 0, failure = 0, i = 0;
	assert(lst && hdr && ValidateHeader(hdr));

	if (hdr->creator[VE_VERSIONS_COUNT-1]!=-1)
	{
		snapshot = &(lst->first);
		while (!IsVersionYoungerThanSnapshot(lst,snapshot,hdr->creatorTs[VE_VERSIONS_COUNT-1],hdr->creator[VE_VERSIONS_COUNT-1]))
		{
			snapshot=snapshot->next;
			assert(snapshot);
		}
		while (snapshot != &lst->last && !failure)
		{
			if (snapshot->occupancy > 0)
			{
				WuSetLastErrorMacro(WUERR_UNABLE_TO_DAMAGE_SNAPSHOT_IN_USE);
				failure=1;
			}
			else
			{
				snapshot->isDamaged |= 1;
			}
			snapshot = snapshot->next;
		}
	}

	if (!failure)
	{
		memmove(hdr->xptr+1,hdr->xptr,(VE_VERSIONS_COUNT-1)*sizeof(XPTR));
		memmove(hdr->creatorTs+1,hdr->creatorTs,(VE_VERSIONS_COUNT-1)*sizeof(TIMESTAMP));
		memmove(hdr->creator+1,hdr->creator,(VE_VERSIONS_COUNT-1)*sizeof(int));
		hdr->xptr[1]=xptr;
		hdr->creatorTs[0]=creatorTs;
		hdr->creator[0]=creator;
		if (!ValidateHeader(hdr))
		{
			WuSetLastErrorMacro(WUERR_GENERAL_ERROR);
		}
		else 
		{
			success=1;
		}
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
		state->pushedVersions = NULL;
		state->snapshotTs = snapshotTs;
		snapshot->occupancy ++;
		success = 1;
	}
	else if (setup.getTimestamp(&state->clientTs))
	{
		state->pushedVersions = new std::list<SnRequestForGc>();
		snapshotsList.first.clientTs[ClGetCurrentClientId()] = state->clientTs;

		success = 1;
	}
	return success;
}

int VeOnUnregisterClient()
{
	int success=0;
	VeClientState *state=NULL;
	VeSnapshot *snapshot=NULL;

	if (!ClGetCurrentStateBlock((void**)&state,ticket)) {}
	else if (!state->pushedVersions && !GetSnapshotByTimestamp(&snapshotsList, &snapshot, NULL, state->snapshotTs)) {}
	else if (state->pushedVersions)
	{
		delete state->pushedVersions;
		state->pushedVersions = NULL;
		snapshotsList.first.clientTs[ClGetCurrentClientId()] = 0;
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
			MakeMappingFromHeader(&snapshotsList,&mapping,header);
			if (state->pushedVersions)
			{
				if (mapping.validDataBegin == 0)
				{
					if (mapping.version[0].creator != ClGetCurrentClientId())
					{
						WuSetLastErrorMacro(WUERR_WORKING_VERSION_CREATED_BY_ALLY);
					}
					else if (mapping.publicDataBegin > 0)
					{
						WuSetLastErrorMacro(WUERR_NO_APROPRIATE_VERSION);
					}
					else
					{
						setup.protectBuffer(bufferId,32,0);
						success=1;
					}
				}
				else if (mapping.publicDataBegin>1)
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
				if (ordinal < mapping.publicDataBegin || mapping.publicDataEnd <= ordinal)
				{
					WuSetLastErrorMacro(WUERR_NO_APROPRIATE_VERSION);
				}
				else if (!setup.loadBuffer(mapping.version[ordinal].xptr, &bufferId, 0)) {}
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
		header->creator[0] = ClGetCurrentClientId();
		success = 1;
	}
	return success;
}

int VeAllocBlock(LXPTR *lxptr)
{
	XPTR xptr=0;
	VeClientState *state=NULL;
	VersionsHeader *header=NULL;
	SnRequestForGc pushedVersion;
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
		else if (state->pushedVersions == NULL)
		{
			WuSetLastErrorMacro(WUERR_SNAPSHOTS_ARE_READ_ONLY);
		}
		else if (!setup.allocBlock(&xptr)) {}
		else if (!setup.loadBuffer(xptr,&bufferId,1)) {}
		else if (!VeInitBlockHeader(xptr, bufferId)) {}
		else
		{			
			pushedVersion.lxptr = xptr;
			pushedVersion.xptr = 0;
			pushedVersion.anchorTs = ~(TIMESTAMP)0;
			state->pushedVersions->push_back(pushedVersion);			
			success = 1;
			*lxptr = xptr;
		}
	}
	return success;
}

int VeCreateVersion(LXPTR lxptr)
{
	VeClientState *state=NULL;
	VersionsHeader header, *pheader=NULL;
	VeMapping mapping;
	SnRequestForGc pushedVersion;
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
		else if (state->pushedVersions == NULL)
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
			MakeMappingFromHeader(&snapshotsList,&mapping,pheader);
			pushedVersion.lxptr=lxptr;
			pushedVersion.xptr=0;
			GetSnapshotByOrdinalNumber(&snapshotsList,&snapshot,mapping.anchor);
			pushedVersion.anchorTs=snapshot->timestamp;
			if (mapping.publicDataBegin>1)
			{
				WuSetLastErrorMacro(WUERR_NO_APROPRIATE_VERSION);
			}
			if (mapping.validDataBegin==0)
			{
				if (mapping.version[0].creator != ClGetCurrentClientId())
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
				isSpecial=(persOrdinal!=0 && mapping.version[1].xptr==mapping.version[persOrdinal].xptr);
				if (!setup.copyBlock(xptr,lxptr,isSpecial)) {}
				else if (!setup.loadBuffer(lxptr,&bufferId,0)) {}
				else if (!PushNewVersionIntoHeader(&snapshotsList, &header, xptr, state->clientTs, ClGetCurrentClientId())) {}
				else
				{
					if (!setup.locateHeader(bufferId,&pheader)) {}
					else if (isSpecial && !setup.onCompleteBlockRelocation(ClGetCurrentClientId(),lxptr,xptr)) {}
					else
					{
						pushedVersion.xptr=xptr;
						state->pushedVersions->push_back(pushedVersion);
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
	VeMapping mapping;
	SnRequestForGc pushedVersion;
	int success = 0, okStatus = 0, isReady = 0, bufferId = 0;

	if (!ClGetCurrentStateBlock((void**)&state,ticket)) {}
	else
	{
		okStatus = ClIsClientReady(&isReady,ClGetCurrentClientId()); assert(okStatus);
		if (!isReady)
		{
			WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
		}
		else if (state->pushedVersions == NULL)
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
			MakeMappingFromHeader(&snapshotsList,&mapping,header);
			if (mapping.publicDataBegin>1)
			{
				WuSetLastErrorMacro(WUERR_NO_APROPRIATE_VERSION);
			}
			if (mapping.validDataBegin>0)
			{
				WuSetLastErrorMacro(WUERR_OPERATION_REQUIRES_WORKING_VERSION);
			}
			else if (mapping.version[0].creator != ClGetCurrentClientId())
			{
				WuSetLastErrorMacro(WUERR_WORKING_VERSION_CREATED_BY_ALLY);
			}
			else
			{
				header->isZombie=1;
				if (setup.markBufferDirty(bufferId,header,sizeof *header,0))
				{
					GetSnapshotByOrdinalNumber(&snapshotsList,&snapshot,mapping.validDataEnd);
					assert(snapshot);
					pushedVersion.lxptr = ~(LXPTR)0;
					pushedVersion.xptr = lxptr;
					pushedVersion.anchorTs = snapshot->timestamp;
					state->pushedVersions->push_back(pushedVersion);
					success = 1;
				}
			}
		}
	}
	return success;
}

int VeOnCommit()
{
	VeClientState *state=NULL;
	SnRequestForGc buf[VE_BUFSZ], *ibuf=buf, *ebuf=buf+VE_BUFSZ;
	std::list<SnRequestForGc>::iterator i;
	int success = 0, failure = 0;

	if (!ClGetCurrentStateBlock((void**)&state,ticket)) {}
	else if (state->pushedVersions==NULL)
	{
		success=1;
	}
	else
	{
		i=state->pushedVersions->begin();
		while(i!=state->pushedVersions->end() && !failure)
		{
			ibuf=buf;
			for (;ibuf<ebuf && i!=state->pushedVersions->end();++i)
			{
				if (i->anchorTs != ~(TIMESTAMP)0)
				{
					ibuf->lxptr = i->lxptr;
					ibuf->xptr = i->xptr;
					ibuf->anchorTs = i->anchorTs;
					++ibuf;
				}
			}
			failure = (0 == setup.acceptRequestForGc(~(TIMESTAMP)0,buf,ibuf-buf));
		}
		success = (0==failure);
	}	
	return success;
}

int VeOnRollback()
{
	VeClientState *state=NULL;
	std::list<SnRequestForGc>::iterator i;
	SnRequestForGc buf[VE_BUFSZ], *ibuf=buf, *ebuf=buf+VE_BUFSZ;
	int success = 0, failure = 0;

	if (!ClGetCurrentStateBlock((void**)&state,ticket)) {}
	else if (state->pushedVersions == NULL)
	{
		success = 1;
	}
	else
	{
		for (i=state->pushedVersions->begin(); i!=state->pushedVersions->end() && !failure; ++i)
		{
			if (i->anchorTs!=~(TIMESTAMP)0) /* not the first version */ 
			{
				failure = (setup.copyBlock(i->lxptr, i->xptr, 0) == 0); 
			}
			if (!failure)
			{
				if (i->lxptr == ~(LXPTR)0) {}
				else if (persSnapshotTs < i->anchorTs)
				{
					 if (ibuf>=ebuf)
					 {
						 failure = (0 == setup.acceptRequestForGc(~(TIMESTAMP)0,buf,ibuf-buf));
						 ibuf = buf;
					 }
					 ibuf->lxptr = i->lxptr;
					 ibuf->xptr = i->xptr;
					 ibuf->anchorTs = i->anchorTs;
					 ++ibuf;
				}
				else
				{
					failure = (setup.freeBlock(i->lxptr) == 0);
				}
			}
		}
		if (!failure) 
		{
			failure = (0 == setup.acceptRequestForGc(~(TIMESTAMP)0,buf,ibuf-buf));
			ibuf = buf;
		}
		success = (failure == 0);
	}
	return success;
}

static 
int setupTsProc(ClEnumerateClientsParams *params, int clientId)
{
	VeSnapshot *snapshot = NULL;
	assert(params->userData && clientId>=0 && clientId < snapshotsList.clientsCount);
	snapshot = (VeSnapshot*)params->userData;
	snapshot->clientTs[clientId] = snapshotsList.first.clientTs[clientId];
	return 1;
}

int VeOnSnapshotsAdvanced(TIMESTAMP snapshotTs, TIMESTAMP discardedTs)
{
	ClEnumerateClientsParams params;
	int success=0;

	if (discardedTs!=0 && !DiscardSnapshot(&snapshotsList, discardedTs)) {}
	else if (!CreateSnapshot(&snapshotsList, snapshotTs)) {}
	else if (!GetSnapshotByTimestamp(&snapshotsList, (VeSnapshot **)&(params.userData), NULL, snapshotTs)) {}
	else if (!ClEnumerateClients(&params, setupTsProc)) {}
	else
	{
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
	if (ClGetCurrentStateBlock((void**)state, ticket) && state->pushedVersions)
	{
		*timestamp = state->clientTs;
	}
	return 0;
}

void VeDbgDump(int reserved)
{
	fprintf(stderr,"VeDbgDump (not implemented)\n\n"); 
}
