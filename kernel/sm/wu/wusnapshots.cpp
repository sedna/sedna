#define __WUDANG_SOURCES__

#include <stdio.h>
#include <limits.h>
#include <list>
#include <algorithm>
#include "wutypes.h"
#include "wuclients.h"
#include "wusnapshots.h"
#include "wuaux.h"
#include "wuerr.h"
#include "wudock.h"

#define SN_SNAPSHOTS_COUNT		3
#define SN_BUFSZ				1024
#define SN_DUMP_LIMIT			8
#define SN_MAX_CLIENTS_COUNT	0x10000
#define SN_MAX_SNAPSHOTS_COUNT	0x100

/* defs */ 

struct SnClientState
{
	union
	{
		TIMESTAMP snapshotTs;
		TIMESTAMP clientTs;
		TIMESTAMP timestamp;
	};
	int statusAndType;
};

struct SnGcNode
{
	SnGcNode *next;
	std::list<SnVersionEntry> entries;
	size_t garbageCount;
};

struct SnSnapshot
{
	TIMESTAMP timestamp;			/*	timestamp assigned to snapshot (future snapshots
												are timestamped with INVALID_TIMESTAMP */ 
	SnSnapshot *next;				/*	snapshots are arranged in a single linked list
												sorted by their timestamps in decreasing order
												(future snapshots first, then other types of sh) */ 
	TIMESTAMP 
		*tsBegin,					/*	ordered array of timestamps of those transactions */ 
		*tsEnd;						/*			which were active when snapshot was taken */ 

	SnGcNode *gcChain;				/*	each snapshot has a single linked chain of gcList nodes
												attached; the first list contains pushed versions
												belonging to this snapshot ONLY; the second list
												contains pushed versions belonging to this snapshot
												AND the one in front of this snapshot, etc. */ 
	int type;						/*	snapshot type - see below */ 
	int occupancy;					/*	the number of clients using this snapshot */  
	int isDamaged;					/*	if queries can run on this snapshot, see SnCompactDFVHeader */ 
};

/*	NOTE!
	Types are combined with bitwize operators and tested against masks, 
	ensure that opA & opB == 0  unless opA==opB.	*/ 

/*	Just the normal snapshot - has a timestamp assigned and 0+ clients using it; regular 
	snapshot is	automaticly discarded as soon as it's occupancy counter reaches zero AND 
	a more recent snapshot different from SN_FUTURE_SNAPSHOT exists. Can be turned into
	SN_NEXT_PERSISTENT_SNAPSHOT during checkpoint or SN_FUTURE_SNAPSHOT when discarded. */ 
#define SN_REGULAR_SNAPSHOT				0x01

/*	Persistent snapshot is never discarded. Clients are allowed to use it as if it was 
	SN_REGULAR_SNAPSHOT. It is turned into SN_PREV_PERSISTENT_SNAPSHOT during checkpoint. */ 
#define SN_PERSISTENT_SNAPSHOT			0x02

/*	The most recent SN_REGULAR_SNAPSHOT is turned into SN_PERSISTENT_SNAPSHOT as the
	result of SnOnBeginCheckpoint call. The former persistent snapshot is turned into 
	SN_PREV_PERSISTENT_SNAPSHOT and is later turned into SN_REGULAR_SNAPSHOT when 
	SnOnCompleteCheckpoint is called. */ 
#define SN_PREV_PERSISTENT_SNAPSHOT		0x04

/*	There is a fixed number of snapshots allowed - SN_SNAPSHOTS_COUNT. When particular snapshot
	is currently unused it is marked SN_FUTURE_SNAPSHOT and placed in the head of snapshots
	list. This is required for simple GcNodes management. */ 
#define SN_FUTURE_SNAPSHOT				0x10

struct SnSnapshotsList
{
	void *mem;
	SnSnapshot *leadingSnapshot;
	TIMESTAMP *tsBegin, *tsEnd;
	int nClientsMax;
	int nSnapshotsMax;
	size_t runawayCount;
};

struct SnInternalSnapshotStats
{
	/*	global stats */ 
	size_t nVersions;
	size_t nGarbageVersions;

	/*	stats on versions belonging to the particular snapshot 
		(versions shared between the particular snapshot and other snapshots also counted )*/ 
	size_t nSnapshotVersions;
	size_t nSnapshotGarbageVersions;

	/*	stats on versions shared between the particular snapshot and other snapshots */ 
	size_t nSnapshotSharedVersions;
	size_t nSnapshotSharedGarbageVersions;
};

struct SnEnumerateVersionsBuf
{
	size_t bufferSz;
	SnVersionEntry *normalBufEnd, *garbageBufEnd, *normalBufPos, *garbageBufPos;
	SnEnumerateVersionsParams *params;
	SnEnumerateVersionsProc enumProc;
};

/* functions from other modules */ 

static int ImpFreeBlock (XPTR xptr);
static int ImpGetTimestamp (TIMESTAMP *timestamp);
static int ImpOnDiscardSnapshot (TIMESTAMP snapshotTs);
static int ImpOnBeforeDiscardSnapshot (TIMESTAMP snapshotTs, int *bDenyDiscard);
static int ImpGetCurrentStateBlock (SnClientState **ptr);

/* helper functions */ 

static
int InitSnapshotsList(SnSnapshotsList *lst,
					  int nClientsMaxParam,
					  int nSnapshotsMaxParam)
{
	int success = 0;

	assert(lst);
	if (nClientsMaxParam<1 || nClientsMaxParam>SN_MAX_CLIENTS_COUNT ||
		nSnapshotsMaxParam<1 || nSnapshotsMaxParam>SN_MAX_SNAPSHOTS_COUNT)
	{
		WuSetLastErrorMacro(WUERR_BAD_PARAMS);
	}
	else
	{
		void *mem = NULL;
		int nGcNodes = (1+nSnapshotsMaxParam)*nSnapshotsMaxParam/2;
		size_t snapshotsSz = sizeof(SnSnapshot)*nSnapshotsMaxParam;
		size_t gcNodesSz = sizeof(SnGcNode)*nGcNodes;
		size_t timestampsSz = sizeof(TIMESTAMP)*nClientsMaxParam;

		mem = malloc(gcNodesSz + snapshotsSz + timestampsSz);
		if (!mem)
		{
			WuSetLastErrorMacro(WUERR_NO_MEMORY);
		}
		else
		{
			static const SnSnapshotsList initializer = {};

			int i=0, j=0;
			/* DestroySnapshotsList depends on the order of sections in mem */ 
			SnGcNode *gcNodesPool = (SnGcNode*)mem;
			SnSnapshot *snapshotsPool = (SnSnapshot*)OffsetPtr(mem,gcNodesSz);
			TIMESTAMP *timestampsPool = (TIMESTAMP*)OffsetPtr(mem,gcNodesSz+snapshotsSz);
			SnSnapshot *pSn = NULL;
			SnGcNode *pGn = NULL;

			/* gcNodesPool */ 
			for (i=0; i<nGcNodes; ++i)
			{
				/* placement new - we have non POD member in SnGcNode (std::list) */ 
				pGn = new (gcNodesPool+i) SnGcNode;
				pGn->next = NULL;
				pGn->garbageCount = 0;
			}

			/* lst */ 
			*lst=initializer;
			lst->mem = mem;
			lst->tsBegin = lst->tsEnd = timestampsPool;
			lst->nClientsMax = nClientsMaxParam;
			lst->nSnapshotsMax = nSnapshotsMaxParam;
			
			/* otherSnapshots*/ 
			lst->leadingSnapshot = snapshotsPool;

			for (i=0; i<nSnapshotsMaxParam; ++i)
			{
				static const SnSnapshot initializer = {};
				pSn = snapshotsPool+i;
				*pSn = initializer;
				pSn->next = pSn+1;
				pSn->timestamp = INVALID_TIMESTAMP;
				pSn->type = SN_FUTURE_SNAPSHOT;

				pGn = gcNodesPool+(i+1)*i/2;
				pSn->gcChain = pGn;
				for (j=0; j<=i; ++j)
				{
					/* occupancy & stuff initialized earlier */ 
					pGn->next=pGn+1;
					++pGn;
				}
				pGn[-1].next=NULL;
			}
			pSn->next = NULL;

			/* yes! */ 
			success=1;
		}
	}
	return success;
}

static
void DestroySnapshotsList(SnSnapshotsList *lst)
{
	static const SnSnapshotsList initializer = {};

	assert(lst);
	if (lst->mem)
	{
		int i=0, nGcNodes = (1+lst->nSnapshotsMax)*lst->nSnapshotsMax/2;
		SnGcNode *gcNodesPool = (SnGcNode*)lst->mem;
		for(i=0; i<nGcNodes; ++i)
		{
			/* SnGcNode is non-POD */ 
			gcNodesPool[i].~SnGcNode();
		}
		free(lst->mem);
	}
	*lst = initializer;
}

/*	Free all blocks in list. */ 
static
int PurgeVersions(SnGcNode *pGn)
{
	int success=0;
	assert(pGn);
	while (!pGn->entries.empty())
	{
		if (!ImpFreeBlock(pGn->entries.front().xptr)) break;
		else pGn->entries.pop_front();
	}
	success = (pGn->entries.empty());
	if (success)
	{
		pGn->garbageCount = 0;
	}
	return success;
}

/*	Get snapshot by timestamp, returns 0 on failure (*result==NULL)
	and element ordinal number (1-based) on success. If prev not NULL 
	and function  succeeded *prev is assigned a pointer to the element 
	followed by *result in the list.	*/ 
static
int GetSnapshotByTimestamp2(SnSnapshot *hd,
							SnSnapshot **resultPtr, 
							SnSnapshot **prevPtr, 
							TIMESTAMP ts)
{
	int success=0, ordinal=1;
	SnSnapshot *prev=NULL;

	assert(resultPtr);
	while (hd && hd->timestamp!=ts)
	{
		prev=hd; hd=hd->next; ++ordinal;
	}
	if (hd && hd->timestamp==ts) 
	{
		success=ordinal;
	}
	else 
	{
		WuSetLastErrorMacro(WUERR_NO_SNAPSHOT_WITH_THIS_TIMESTAMP);
		prev=NULL;
	}
	*resultPtr = hd;
	if (prevPtr)
	{
		*prevPtr = prev;
	}
	return success;
}

/*	Get snapshot from snapshots list by timestamp, see GetSnapshotByTimestamp2 for details.	*/ 
static
int GetSnapshotByTimestamp(SnSnapshotsList *snLst,
						   SnSnapshot **result, 
						   SnSnapshot **prev, 
						   TIMESTAMP ts)
{
	assert(snLst);
	return GetSnapshotByTimestamp2(snLst->leadingSnapshot, result, prev, ts);
}

/*	Get snapshot by type, returns 0 on failure (*result==NULL)
	and element ordinal number (1-based) on success. If prev not NULL 
	and function  succeeded *prev is assigned a pointer to the element 
	followed by *result in the list.	*/ 
static
int GetSnapshotByType2(SnSnapshot *hd,
					   SnSnapshot **resultPtr, 
					   SnSnapshot **prevPtr, 
					   int typeMask)
{
	int success=0, ordinal=1;
	SnSnapshot *prev=NULL;

	assert(resultPtr);
	while (hd && !(hd->type & typeMask))
	{
		prev=hd; hd=hd->next; ++ordinal;
	}
	if (hd && (hd->type & typeMask)) 
	{
		success=ordinal;
	}
	else 
	{
		WuSetLastErrorMacro(WUERR_NO_SNAPSHOT_WITH_THIS_TYPE);
		prev=NULL;
	}
	*resultPtr = hd;
	if (prevPtr)
	{
		*prevPtr = prev;
	}
	return success;
}

/*	Get snapshot from snapshots list by type, see GetSnapshotByType2 for details.	*/ 
static
int GetSnapshotByType(SnSnapshotsList *snLst,
					  SnSnapshot **result, 
					  SnSnapshot **prev, 
					  int typeMask)
{
	assert(snLst);
	return GetSnapshotByType2(snLst->leadingSnapshot, result, prev, typeMask);
}

/*	Get "current" snapshot - i.e. the most recent non-fake snapshot. */ 
static
SnSnapshot *GetCurrentSnapshot(SnSnapshotsList *snLst)
{
	SnSnapshot *current=NULL;
	GetSnapshotByType(snLst, &current, NULL, ~SN_FUTURE_SNAPSHOT);
	return current;
}
					  
/*	Get a pointer to SnGcNode list item by index (starting from 0). 
	If the node with requested index exist, *p is pointing to it and function returns 1.
	If index is out of bounds function retuns 0 and p is pointing either to the first node 
	(index<0) or to the last node (index>=length). If hd==NULL, function returns 0 and *p is NULL. */ 
static 
int GetGcNodeByIndex(SnGcNode *hd, SnGcNode **resultPtr, int index)
{
	SnGcNode *prevOr1st=hd;

	assert(resultPtr);
	while (hd && index>0)
	{
		prevOr1st=hd;
		hd=hd->next;
		--index;
	}
	if (hd && index==0)
	{
		*resultPtr=hd;
		return 1;
	}
	else
	{
		*resultPtr=prevOr1st;
		return 0;
	}
}

/*	Merge 2 Gc nodes. Src contents are appended to dest and src is cleared. */ 
static
void MergeNodesAppendReset(SnGcNode *dest, SnGcNode *src)
{
	assert(dest && src);
	dest->garbageCount+=src->garbageCount; 
	src->garbageCount=0;
	dest->entries.splice(dest->entries.end(),src->entries);
	src->entries.clear();
}

/*	Merges node identified by mergeIndex (starting from 0) with the next one, 
	resulting in the chain going 1 node shorter. An 'empty' node is appended to 
	the chain to restore length. Function doesn't allocate or free nodes - it just 
	changes the order of nodes in the chain and manipulates their contents.	*/ 
static 
void RotateGcChain(SnGcNode *hd, int mergeIndex)
{
	SnGcNode *toMerge=NULL, *last=NULL, *moved=NULL;
	int okStatus=0;

	assert(hd);
	okStatus = GetGcNodeByIndex(hd,&toMerge,mergeIndex);
	assert(okStatus && toMerge && toMerge->next);
	
	/* rotating */ 
	moved=toMerge->next;
	toMerge->next=moved->next;
	moved->next=NULL;
	GetGcNodeByIndex(hd,&last,INT_MAX);
	last->next=moved;

	/* merging */ 
	MergeNodesAppendReset(toMerge, moved);
}

/*	Performs actions prior to snapshot destruction. Hd points to the snapshots list
	head. If beforeVictim not NULL beforeVictim->next is the snapshot beeng discarded (victim).
	If beforeVictim==NULL we assume hd points to the snapshot beeng discarded.
	Gc nodes attached to snapshots in the range [hd, victim] are rearranged 
	and all the  necessary merging occurs. Function rearranges snapshots in the 
	range [hd, victim] and returns a pointer to the list new head.
	Gc nodes after victim are not rearranged (call RotateGcChain for this).
	Function doesn't change victim fields (type & timestamp & etc), do it
	before calling the function.
	*/ 
static
SnSnapshot *RotateSnapshots(SnSnapshot *hd, 
							SnSnapshot *beforeVictim)
{
	assert(hd);

	if (beforeVictim)
	{
		SnSnapshot *victim = NULL, *unaffected = NULL, *sniter = NULL;
		SnGcNode *detached = NULL, *merged = NULL, *gniter = NULL;

		victim = beforeVictim->next;
		unaffected = victim->next;

		/* detach gcChain fragment */ 
		detached = victim->gcChain->next;
		victim->gcChain->next = NULL;
		/* merge */ 
		merged = beforeVictim->gcChain;
		gniter = detached;
		while(merged && gniter)
		{
			MergeNodesAppendReset(merged, gniter);
			merged = merged->next;
			gniter = gniter->next;
		}
		assert(!merged && !gniter); /* chains have equal length */ 
		/* rotate */ 
		beforeVictim->next = unaffected;
		victim->next = hd;
		hd = victim;
		/* repair chains with gniters from detached fragment */ 
		for (sniter = hd->next; sniter!=unaffected; sniter=sniter->next)
		{
			SnGcNode *last = NULL;

			assert(detached);
			GetGcNodeByIndex(sniter->gcChain,&last,INT_MAX);
			gniter = detached;
			detached = detached->next;
			gniter->next = NULL;
			last->next = gniter;
		}
		assert(detached == NULL); /* all detached nodes re-connected */ 
	}
	return hd;
}

/*	Highlevel discard snapshot function. */ 
static
int DiscardSnapshot(SnSnapshotsList *snLst,
					TIMESTAMP ts)
{
	int success=0;
	SnSnapshot *victim=NULL, *beforeVictim=NULL;

	assert(snLst);
	if (!GetSnapshotByTimestamp(snLst, &victim, &beforeVictim, ts))
	{
		; /* bad timestamp or something */ 
	}
	else if (victim->type != SN_REGULAR_SNAPSHOT)
	{
		WuSetLastErrorMacro(WUERR_UNABLE_TO_DISCARD_SPECIAL_SNAPSHOT);
	}
	else if (victim->occupancy > 0)
	{
		WuSetLastErrorMacro(WUERR_UNABLE_TO_DISCARD_SNAPSHOT_IN_USE);
	}
	else if (PurgeVersions(victim->gcChain))
	{
		SnSnapshot *iter=NULL;
		int i=0;
		/* reset victim */ 
		victim->timestamp = INVALID_TIMESTAMP;
		victim->type = SN_FUTURE_SNAPSHOT;
		victim->isDamaged = 0;
		victim->occupancy = 0;
		free(victim->tsBegin);
		victim->tsBegin = victim->tsEnd = NULL;
		/* rotate chains after victim */ 
		for (iter = victim->next; iter!=NULL; ++i, iter=iter->next)
		{
			RotateGcChain(iter->gcChain, i);
		}
		/* rotate snapshots - victim becomes leading */ 
		snLst->leadingSnapshot = RotateSnapshots(snLst->leadingSnapshot, beforeVictim);
		success=1;
	}
	return success;
}

/* Highlevel create snapshot function. */ 
static
int CreateSnapshot(SnSnapshotsList *snLst,
				   TIMESTAMP ts,
				   int type)
{
	int success=0;
	TIMESTAMP *tsPool = NULL;
	SnSnapshot *firstNonFutureSn=NULL, *lastFutureSn=NULL;
	size_t tsPoolSz = 0;

	assert(ts && snLst);
	tsPoolSz = sizeof(TIMESTAMP) * (snLst->tsEnd - snLst->tsBegin);
	if (!IsValidTimestamp(ts))
	{
		WuSetLastErrorMacro(WUERR_BAD_TIMESTAMP);
	}
	else if (snLst->leadingSnapshot->type != SN_FUTURE_SNAPSHOT)
	{
		WuSetLastErrorMacro(WUERR_MAX_NUMBER_OF_SNAPSHOTS_EXCEEDED);
	}
	else if (tsPoolSz && !(tsPool = (TIMESTAMP*)malloc(tsPoolSz)))
	{
		WuSetLastErrorMacro(WUERR_NO_MEMORY);
	}
	else
	{
		/* copy current active timestamps */ 
		memcpy(tsPool,snLst->tsBegin,tsPoolSz);
		/* select snapshot to reuse */ 
		GetSnapshotByType(snLst, &firstNonFutureSn, &lastFutureSn, ~SN_FUTURE_SNAPSHOT);
		if (!lastFutureSn)
		{
			/* all snapshots are future ones */ 
			lastFutureSn = snLst->leadingSnapshot;
			while (lastFutureSn->next) lastFutureSn = lastFutureSn->next;
			assert(lastFutureSn->type == SN_FUTURE_SNAPSHOT);
		}
		/* setup snapshot */ 
		lastFutureSn->timestamp = ts;
		lastFutureSn->tsBegin = tsPool;
		lastFutureSn->tsEnd = (TIMESTAMP*)OffsetPtr(tsPool,tsPoolSz);
		lastFutureSn->type = type;
		lastFutureSn->occupancy = 0;
		lastFutureSn->isDamaged = 0;
		success = 1;
	}
	if (!success)
	{
		free(tsPool); tsPool = NULL;
	}
	return success;
}

/*	See VisitNodes. */ 
typedef int (VisitNodeProc)(SnGcNode *node, 
							void *userData, 
							int bNodeBelongsToSnapshot, 
							int bNodeShared);

/*	Visit every gc node in unspecified order. For each node visitNodeProc is 
	called. Proc recieves pointer to the gc node (1 param), a pointer to
	user-provided data (2 param), a flag indicating that node belongs to the snapshot
	identified by timestamp (3 param) and another flag indicating that node is 
	is owned by ANY 2+ snapshots.
	If Ts is not INVALID_TIMESTAMP and no snapshot with the timestamp exists function fails.
	If visitNodeProc fails the function fails as well. */ 
static
int VisitNodes(SnSnapshotsList *snLst,
			   TIMESTAMP ts,
			   VisitNodeProc visitNodeProc,
			   void *userData)
{
	int pos=INT_MAX, success=0;
	SnSnapshot *sniter;

	assert (snLst && visitNodeProc);
	if (ts != INVALID_TIMESTAMP && (pos = GetSnapshotByTimestamp(snLst,&sniter,NULL,ts)-1)<0)
	{
		/* no snapshot with this timestamp */ 
	}
	else
	{
		int x=0;
		for (sniter = snLst->leadingSnapshot; 
			 sniter != NULL;
			 x++, sniter = sniter->next)
		{			
			int y=0;
			SnGcNode *gniter = NULL;
			for (gniter = sniter->gcChain; gniter; ++y, gniter=gniter->next)
			{
				if (!visitNodeProc(gniter, userData, x>=pos && y>=x-pos, y>0)) goto failed;
			}
		}
		success=1;
failed:
		;
	}
	return success;
}

/*	See GatherInternalStats. */ 
int GatherInternalStatsProc(SnGcNode *node,
							void *userData,
							int bNodeBelongsToSnapshot, 
							int bNodeShared)
{
	size_t nVersions=0, nGarbageVersions=0;
	SnInternalSnapshotStats *stats = (SnInternalSnapshotStats *)userData;

	assert (node && stats);
	nVersions = node->entries.size();
	nGarbageVersions = node->garbageCount;
	stats->nVersions += nVersions;
	stats->nGarbageVersions +=nGarbageVersions;
	if (bNodeBelongsToSnapshot)
	{
		stats->nSnapshotVersions += nVersions;
		stats->nSnapshotGarbageVersions += nGarbageVersions;
		if (bNodeShared)
		{
			stats->nSnapshotSharedVersions += nVersions;
			stats->nSnapshotSharedGarbageVersions += nGarbageVersions;
		}
	}
	return 1;
}

/*	Gather stats on total number of versions in Gc chains and on the
	number of versions belonging to the snapshot identified by ts. */ 
static
int GatherInternalStats(SnSnapshotsList *snLst,
						SnInternalSnapshotStats *stats,
						TIMESTAMP ts)
{
	static const SnInternalSnapshotStats initializer = {};
	assert(stats);
	*stats = initializer;
	return VisitNodes(snLst, ts, GatherInternalStatsProc, stats);
}

static 
void InitEnumerateVersionsBuf(SnEnumerateVersionsBuf *enumBuf,
							  SnEnumerateVersionsParams *params,
							  SnEnumerateVersionsProc enumProc,
							  SnVersionEntry *buf,
							  size_t bufSz)
{
	assert(enumBuf && params && enumProc && buf && bufSz>1);
	bufSz/=2;
	enumBuf->bufferSz = bufSz;
	enumBuf->normalBufPos = buf;
	enumBuf->garbageBufPos = buf + bufSz;
	enumBuf->normalBufEnd = enumBuf->normalBufPos + bufSz;
	enumBuf->garbageBufEnd = enumBuf->garbageBufPos + bufSz;
	enumBuf->params = params;
	enumBuf->enumProc = enumProc;
}

static 
int FlushEnumerateVersionsBuf(SnEnumerateVersionsBuf *enumBuf,
							  int bFlushAll)
{
	int success = 0;
	size_t normalCount = 0, garbageCount = 0;
	assert(enumBuf);

	normalCount = enumBuf->bufferSz - (size_t)(enumBuf->normalBufEnd - enumBuf->normalBufPos);
	garbageCount = enumBuf->bufferSz - (size_t)(enumBuf->garbageBufEnd - enumBuf->garbageBufPos); 


	/* flush normal buf */ 
	if ((bFlushAll && normalCount>0) || normalCount==enumBuf->bufferSz)
	{
		if (!enumBuf->enumProc(enumBuf->params, 
							   enumBuf->normalBufPos - normalCount, 
							   normalCount, 0)) 
			goto failed;
		enumBuf->params->persVersionsSent+=normalCount;
		enumBuf->normalBufPos-=normalCount;
	}
	/* flush garbage buf */ 
	if ((bFlushAll && garbageCount>0) || garbageCount==enumBuf->bufferSz)
	{
		if (!enumBuf->enumProc(enumBuf->params, 
							   enumBuf->garbageBufPos - garbageCount, 
							   garbageCount, 1)) 
			goto failed;
		enumBuf->params->garbageVersionsSent+=garbageCount;
		enumBuf->garbageBufPos-=garbageCount;
	}
	success=1;
failed:
	return success;
}

static inline
int BufferVersionEntry(SnEnumerateVersionsBuf *enumBuf,
					   SnVersionEntry entry,
					   int isGarbage)
{
	if ((isGarbage && enumBuf->garbageBufPos == enumBuf->garbageBufEnd) ||
		(!isGarbage && enumBuf->normalBufPos == enumBuf->normalBufEnd))
	{
		if (!FlushEnumerateVersionsBuf(enumBuf, 0)) return 0;
	}
	(isGarbage ? enumBuf->garbageBufPos : enumBuf->normalBufPos) ++ [0] = entry;
	return 1;
}

int EnumerateVersionsForCheckpointProc(SnGcNode *node,
									   void *userData,
									   int bNodeBelongsToSnapshot, 
									   int bNodeShared)
{
	SnEnumerateVersionsBuf *enumBuf = (SnEnumerateVersionsBuf *)userData;
	std::list<SnVersionEntry>::iterator it;
	
	assert(node && enumBuf);

	for (it = node->entries.begin(); it != node->entries.end(); ++it)
	{
		if (!BufferVersionEntry(enumBuf, *it, !bNodeBelongsToSnapshot || it->lxptr==0))
			return 0;
	}
	return 1;
}

/* Mark all snapshots without transactions as damaged. */ 
int AutoDamageSnapshotsAndScanForGarbage(SnSnapshotsList *snLst,
										 int isKeepingCurrentSnapshot)
{
	SnSnapshot *snapshot = NULL;

	assert(snLst);
	snapshot = GetCurrentSnapshot(snLst);
	if (snapshot)
	{
		/* skip the current snapshot */ 
		if (isKeepingCurrentSnapshot) snapshot = snapshot->next;
		/* mark all other snapshots as damaged */ 
		while (snapshot)
		{
			if (snapshot->occupancy==0) snapshot->isDamaged=1;
			snapshot = snapshot->next;
		}
	}
	return 1;
};

/*	Discard all those snapshots that can be discarded now. */ 
static
int PurifySnapshots(SnSnapshotsList *snLst,
					int isKeepingCurrentSnapshot)
{
	int failure=0;
	SnSnapshot *current=NULL;

	assert(snLst);
	current=GetCurrentSnapshot(snLst);
	if (current && isKeepingCurrentSnapshot) current=current->next;
	while (!failure && current)
	{
		int bDenyDiscarding=0;
		TIMESTAMP ts=current->timestamp;

		bDenyDiscarding = (current->occupancy!=0 || current->type!=SN_REGULAR_SNAPSHOT);
		if(!bDenyDiscarding && !ImpOnBeforeDiscardSnapshot(ts,&bDenyDiscarding))
		{
			failure=1;
		}
		else if (current->timestamp!=ts)
		{
			/* callback discarded snapshot pointed by current ptr, exiting*/ 
			current=NULL;
		}
		else if (bDenyDiscarding)
		{
			current=current->next;
		}
		else if (!ImpOnDiscardSnapshot(ts))
		{
			failure=1;
		}
		else if (current->timestamp!=ts)
		{
			/*	callback discarded snapshot pointed by current ptr, exiting */ 
			current=NULL;				
		}
		else
		{
			current=current->next;
			if (!DiscardSnapshot(snLst,ts)) failure=1;
		}
	}
	return failure==0 && AutoDamageSnapshotsAndScanForGarbage(snLst, isKeepingCurrentSnapshot);
}

static
int ValidateRequestForGc(const SnRequestForGc *req)
{
	int success = 0;
	assert(req);
	if (req->type != SN_REQUEST_NOP && 
		req->type != SN_REQUEST_DISCARD_VERSION && 
		req->type != SN_REQUEST_ADD_NORMAL_VERSION &&
		req->type != SN_REQUEST_ADD_BOGUS_VERSION)
	{
		/* bad type */ 
	}
	else if (req->type != SN_REQUEST_NOP && req->xptr == 0)
	{
		/* xptr must be always valid */ 
	}
	else if (req->type == SN_REQUEST_ADD_NORMAL_VERSION && req->lxptr == 0)
	{
		/* must have valid lxptr either */ 
	}
	else if (!IsValidTimestamp(req->anchorTs) && req->anchorTs!=INVALID_TIMESTAMP && 
		(req->type == SN_REQUEST_ADD_NORMAL_VERSION || req->type == SN_REQUEST_ADD_BOGUS_VERSION))
	{
		/* must have valid anchor timestamp */ 
	}
	else
	{
		success=1;
	}
	return success;
}
 
TIMESTAMP SnGetPersistentSnapshotTimestamp()
{
    TIMESTAMP persTs = INVALID_TIMESTAMP;
    SnGetSnapshotTimestamps(NULL, &persTs);
    
    return persTs;
}

static
int SubmitRequestForGc(SnSnapshotsList *snLst, 
					   const SnSnapshot *pSn, 
					   SnRequestForGc request)
{
	int success=0;

	assert(snLst);
	if (!ValidateRequestForGc(&request)) { WuSetLastErrorMacro(WUERR_BAD_PARAMS); }
	else if (request.type == SN_REQUEST_NOP)
	{
        wulog(("WULOG: Commit: ignoring new block allocation: lxptr = %"PRI_XPTR, request.lxptr));
		success = 1;
	}
	else
	{
		int level = 0;
		SnGcNode *pGn = NULL;
		SnVersionEntry entry = {};

		/*	no snapshot depends on the version? */ 
		if (!pSn || pSn->timestamp < request.anchorTs)
		{
			pSn = NULL;
		}
		/*	find the last snapshot depending on the version (pSn is the first one) */ 
		else while (pSn->next && (pSn->next->timestamp >= request.anchorTs ))
		{
			pSn = pSn->next;
			++level;
		}

		/*	if no snapshot depends on the version feel free to discard it */ 
		if (!pSn)
		{
			if (request.type == SN_REQUEST_ADD_NORMAL_VERSION) snLst->runawayCount++;
			request.type = SN_REQUEST_DISCARD_VERSION;
		}
		else
		{
			GetGcNodeByIndex(pSn->gcChain,&pGn,level);
		}
		
		switch(request.type)
		{
		case SN_REQUEST_DISCARD_VERSION:
	        wulog(("WULOG: Commit: deleted old version: lxptr = %"PRI_XPTR", xptr = %"PRI_XPTR", anchorTs = %"PRIx64", persTs = %"PRIx64,
	                request.lxptr, request.xptr, request.anchorTs, SnGetPersistentSnapshotTimestamp()));
	        
			success = ImpFreeBlock(request.xptr);
			break;
		case SN_REQUEST_ADD_BOGUS_VERSION:
            wulog(("WULOG: Commit: deleted block stays as navigator: lxptr = %"PRI_XPTR", anchorTs = %"PRIx64", depTs = %"PRIx64", persTs = %"PRIx64,
                    request.lxptr, request.anchorTs, pSn->timestamp, SnGetPersistentSnapshotTimestamp()));
            
			pGn->garbageCount++;
			/* fallthrough */ 
		case SN_REQUEST_ADD_NORMAL_VERSION:
			entry.xptr = request.xptr;
			entry.lxptr = (request.type == SN_REQUEST_ADD_BOGUS_VERSION ? 0 : request.lxptr);
			pGn->entries.push_back(entry);
			success = 1;
			
            if (request.type == SN_REQUEST_ADD_NORMAL_VERSION)
            {
                wulog(("WULOG: Commit: added new version: lxptr = %"PRI_XPTR", xptr = %"PRI_XPTR,
                        request.lxptr, request.xptr));
            }
			break;
		default:
			WuSetLastErrorMacro(WUERR_BAD_PARAMS);
		}
	}
	return success;
}

/* insert new entry in the ordered array of currently active transaction timestamps */ 
static 
int AddActiveTimestamp(SnSnapshotsList *snLst,
					   TIMESTAMP ts)
{
	int success=0;
	assert(snLst);
	if (!IsValidTimestamp(ts))
	{
		WuSetLastErrorMacro(WUERR_BAD_TIMESTAMP);
	}
	else if (snLst->tsEnd == snLst->tsBegin + snLst->nClientsMax)
	{
		WuSetLastErrorMacro(WUERR_MAX_NUMBER_OF_CLIENTS_EXCEEDED);
	}
	else
	{
		*(snLst->tsEnd++)=ts;
		success=1;
	}
	return success;
}

static 
int RemoveActiveTimestamp(SnSnapshotsList *snLst,
						  TIMESTAMP ts)
{
	int success=0;
	TIMESTAMP *tsptr = NULL;

	assert(snLst);
	tsptr=std::lower_bound(snLst->tsBegin, snLst->tsEnd, ts);
	if (tsptr==snLst->tsEnd || *tsptr!=ts)
	{
		WuSetLastErrorMacro(WUERR_GENERAL_ERROR);
	}
	else
	{
		size_t tailsz = sizeof(TIMESTAMP)*(snLst->tsEnd-tsptr-1);
		memmove(tsptr, tsptr+1, tailsz);
		--snLst->tsEnd;
	}
	return success;
}

static inline
int IsVersionYoungerThanSnapshot(SnSnapshot *pSn, 
								 TIMESTAMP ts)
{
	assert(pSn && (pSn->tsBegin == pSn->tsEnd || pSn->tsBegin));
	return ts > pSn->timestamp || std::binary_search(pSn->tsBegin, pSn->tsEnd, ts);
}

static
void InitFakeHeadSnapshot(SnSnapshotsList *snLst,
						  SnSnapshot *pSn)
{
	assert (snLst && pSn);
	pSn->timestamp = SN_LAST_COMMITED_VERSION_TIMESTAMP;
	pSn->type = 0;
	pSn->next = GetCurrentSnapshot (snLst);
	pSn->tsBegin = snLst->tsBegin;
	pSn->tsEnd = snLst->tsEnd;
}


int EnsureCanAdvanceSnapshots(SnSnapshotsList *snLst)
{
	int count = 0;
	SnSnapshot *snapshot = NULL;

	assert(snLst);
	snapshot = GetCurrentSnapshot(snLst);
	while (snapshot)
	{
		count += (!snapshot->isDamaged);
		snapshot = snapshot->next;
	}
	return (count < SN_SNAPSHOTS_COUNT-1 && snLst->leadingSnapshot->type == SN_FUTURE_SNAPSHOT);
}

void DbgDumpTimestamps(const TIMESTAMP *begin,
					   const TIMESTAMP *end,
					   const char *padding, 
					   const char *padding1st)
{
	assert(begin || begin==end);
	if (!padding) padding = "";
	if (!padding1st) padding1st = padding;
	int lineno = 0;

	if (begin>=end)
    {
	    wulog(("\n"));
    }

	while (begin<end)
	{
		wulog(("%s",(lineno==0 ? padding1st : padding)));
		for (int i=0; i<3 && begin<end; ++i, ++begin)
		{
			wulog((" %016"I64FMT"x", *begin));
		}
		wulog(("\n")); ++lineno;
	}
}

void DbgDumpSnapshots(SnSnapshotsList *snapshots, 
					  int flags, 
					  const char *padding, 
					  const char *padding1st)
{
	assert(snapshots);
	char timestampsPadding[128]="";
	if (!padding) padding = "";
	if (!padding1st) padding1st = padding;
	int lineno = 0;
	strcpy(timestampsPadding, padding);
	strcat(timestampsPadding, "   TS:");

	SnSnapshot *sniter = snapshots->leadingSnapshot;
	if (0 == (flags & SN_DUMP_FUTURE_SNAPSHOTS_FLAG)) 
	{
		sniter = GetCurrentSnapshot(snapshots);
	}
	if (!sniter) 
	{
		wulog(("\n"));
	}
	else if (0 == (flags & SN_DUMP_SNAPSHOT_ATIMESTAMPS_FLAG) &&
			 0 == (flags & SN_DUMP_SNAPSHOT_PROPERTIES_FLAG))
	{			
		while (sniter)
		{
			int i=0;
			wulog(("%s",(lineno==0 ? padding1st : padding)));
			while (sniter && i<3)
			{
				wulog((" %016"I64FMT"x", sniter->timestamp));
				sniter = sniter->next;
				++i;
			}
			wulog(("\n")); ++lineno;
		}
	}
	else while (sniter)
	{
		wulog(("%s",(lineno==0 ? padding1st : padding)));
		wulog((" %016"I64FMT"x", sniter->timestamp));
		if (flags & SN_DUMP_SNAPSHOT_PROPERTIES_FLAG)
		{
			const char *typeStr = "unknown";
			switch (sniter->type)
			{
			case SN_REGULAR_SNAPSHOT:
				typeStr="regular"; break;
			case SN_PERSISTENT_SNAPSHOT:
				typeStr="persistent"; break;
			case SN_PREV_PERSISTENT_SNAPSHOT:
				typeStr="prev.pers."; break;
			case SN_FUTURE_SNAPSHOT:
				typeStr="future"; break;
			}
			wulog((" %11s %2d tr.%s", typeStr, sniter->occupancy, 
				(sniter->isDamaged ? " [damaged]" : "")));
		}
		wulog(("\n")); ++lineno;
		if (flags & SN_DUMP_SNAPSHOT_ATIMESTAMPS_FLAG &&
			sniter->tsBegin != sniter->tsEnd)
		{
			DbgDumpTimestamps(sniter->tsBegin, sniter->tsEnd, timestampsPadding, NULL);
		}
		sniter = sniter->next;
	}
}

void DbgDumpGcNodes(SnSnapshotsList *snapshots,
					int limit,
					int limit2,
					const char *padding,
					const char *padding1st)
{
		SnGcNode *hscan[SN_SNAPSHOTS_COUNT];
		std::list<SnVersionEntry>::iterator xscan[SN_SNAPSHOTS_COUNT];
		int perChainOutCountX2[SN_SNAPSHOTS_COUNT];
		SnSnapshot *sniter=NULL;
		int i=0, lineno = 0;

		assert(snapshots);
		if (!padding) padding = "";
		if (!padding1st) padding1st = padding;

		/* initialize hscan */ 
		sniter = snapshots->leadingSnapshot;
		for (i=0; i<SN_SNAPSHOTS_COUNT; ++i)
		{
			if (sniter)
			{
				hscan[i]=sniter->gcChain;
				sniter=sniter->next;
			}
			else hscan[i]=NULL;
		}
		/* process all chains simultaneously */ 
		while(1)
		{
			int j = 0;
			int rowDensity = 0;
			/* initialize xscan & perChainOutCountX2 */ 
			for (i=0; i<SN_SNAPSHOTS_COUNT; ++i)
			{
				perChainOutCountX2[i] = 0;
				if (hscan[i])
				{
					xscan[i] = hscan[i]->entries.begin();
					rowDensity ++;
				}
			}
			/* all chains exhausted? */ 
			if (rowDensity==0)
			{
				if (lineno==0)
                {
				    wulog(("\n"));
                }
				break;
			}
			/* output headers */ 
			wulog(("%s", (lineno==0 ? padding1st : padding)));
			for (i=0; i<SN_SNAPSHOTS_COUNT; ++i)
			{
				char buf[32] = "";
				if (hscan[i]) 
				{
				    if (hscan[i]->garbageCount) {
                                        sprintf(buf, "[%lu--%ub]",
						(unsigned long)(hscan[i]->entries.size() - hscan[i]->garbageCount),
						(unsigned)hscan[i]->garbageCount);
                                    } else {
                                        sprintf(buf, "[%ld]",
						(unsigned long)(hscan[i]->entries.size() - hscan[i]->garbageCount));
                                    }
				}
				wulog(("  %18s", buf));
			}
			wulog(("\n")); lineno++;
			/* output content */ 
			for (j=0; j<limit2; ++j)
			{
				rowDensity = 0;
				for (i=0; i<SN_SNAPSHOTS_COUNT && limit>0; ++i)
				{
					if (hscan[i] && xscan[i]!=hscan[i]->entries.end()) ++rowDensity;
				}
				if (rowDensity==0) break;
				wulog(("%s", padding));
				for (i=0; i<SN_SNAPSHOTS_COUNT; ++i)
				{
					char buf[32] = "";
					if (hscan[i] && xscan[i]!=hscan[i]->entries.end())
					{
						if (perChainOutCountX2[i]&1)
						{
							sprintf(buf,". %016"PRIx64, xscan[i]->xptr);
							xscan[i]++;
							perChainOutCountX2[i]++;
						}
						else
						{
							if (perChainOutCountX2[i]/2+1==limit ||
								j+1 == limit2 || (j+2==limit2 && xscan[i]->lxptr))
							{
								strcpy(buf,"................");
								xscan[i]=hscan[i]->entries.end();
							}
							else if (xscan[i]->lxptr==0)
							{
								sprintf(buf,"  %016"PRIx64, xscan[i]->xptr);
								xscan[i]++;
								perChainOutCountX2[i]+=2;
							}
							else
							{
								sprintf(buf,"L %016"PRIx64, xscan[i]->lxptr);
								perChainOutCountX2[i]++;
							}							
						}
					}
					wulog(("  %18s", buf));
				}
				wulog(("\n"));
			}
			/* descent down the chains */ 
			rowDensity=0;
			for (i=0; i<SN_SNAPSHOTS_COUNT; ++i)
			{
				if (hscan[i]) 
				{
					hscan[i]=hscan[i]->next;
					rowDensity++;
				}
			}
			/* output extra space */ 
			if (rowDensity&&j>0)
			{
			    wulog(("\n"));
			}
		}
}

/* global state */ 

static int isInitialized = 0;
static SnSetup setup = {};
static SnSnapshotsList snapshots = {};

/* Imps */ 

static int ImpFreeBlock (XPTR xptr)
{
	assert(setup.freeBlock);
	return setup.freeBlock(xptr);
}

static int ImpGetTimestamp (TIMESTAMP *timestamp)
{
	assert(setup.getTimestamp);
	return setup.getTimestamp(timestamp);
}

static int ImpOnDiscardSnapshot (TIMESTAMP snapshotTs)
{
	assert(setup.onDiscardSnapshot);
	return setup.onDiscardSnapshot(snapshotTs);
}

static int ImpOnBeforeDiscardSnapshot (TIMESTAMP snapshotTs, int *bDenyDiscard)
{
	assert(setup.onBeforeDiscardSnapshot);
	return setup.onBeforeDiscardSnapshot(snapshotTs, bDenyDiscard);
}

static int ImpGetCurrentStateBlock (SnClientState **ptr)
{
	return ClGetCurrentStateBlock((void**)ptr, setup.clientStateTicket);
}

/* public api functions */ 

int SnInitialize()
{	
	int success=0;
	if (isInitialized)
	{
		WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
	}
	else
	{
		DestroySnapshotsList(&snapshots);
		isInitialized = 1;
		success=1;
	}
	return 1;
}

void SnDeinitialize()
{
	isInitialized = 0;
}

void SnQueryResourceDemand(SnResourceDemand *demand)
{
	assert(demand);
	demand->clientStateSize = sizeof(SnClientState);
}

int SnStartup(const SnSetup *setupParam)
{
	int success = 0;
	
	assert(setupParam);
	setup = *setupParam;
	if (!InitSnapshotsList(&snapshots, setupParam->maxClientsCount, SN_SNAPSHOTS_COUNT)) {}
	else
	{
		if (setupParam->initialPersSnapshotTs == INVALID_TIMESTAMP)
		{
			success = 1;
		}
		else if (CreateSnapshot(&snapshots, setupParam->initialPersSnapshotTs, SN_PERSISTENT_SNAPSHOT))
		{
			success = 1;
		}	
		if (!success) DestroySnapshotsList(&snapshots);
	}
	return success;
}

int SnShutdown()
{
	int failure=0, success=0;
	SnSnapshot *persSn=NULL;
	if (GetSnapshotByType(&snapshots,&persSn,NULL,SN_PERSISTENT_SNAPSHOT))
	{
		SnInternalSnapshotStats stats = {};
		GatherInternalStats(&snapshots, &stats, persSn->timestamp);
		if (stats.nSnapshotVersions==0)
		{
			persSn->type=SN_REGULAR_SNAPSHOT;
		}
		else
		{
			WuSetLastErrorMacro(WUERR_SHUTDOWN_ERROR);
			failure=1;
		}
	}
	if (!failure)
	{
		if (!PurifySnapshots(&snapshots,0)) {}
		else if (GetCurrentSnapshot(&snapshots))
		{
			WuSetLastErrorMacro(WUERR_SHUTDOWN_ERROR);
		}
		else
		{
			success=1;
		}
	}
	return success;
}

int SnOnRegisterClient(int isUsingSnapshot)
{
	int success=0;
	SnClientState *state=NULL;
	SnSnapshot *current=NULL;

	current = GetCurrentSnapshot(&snapshots);
	ImpGetCurrentStateBlock(&state);
	if (state)
	{
		if (isUsingSnapshot) 
		{
			if (setup.flags & SN_SETUP_DISABLE_VERSIONS_FLAG)
			{
				WuSetLastErrorMacro(WUERR_VERSIONS_DISABLED);
			}
			else if (!current)
			{
				WuSetLastErrorMacro(WUERR_NO_SNAPSHOTS_EXIST);
			}
			else if (current->isDamaged)
			{
				WuSetLastErrorMacro(WUERR_UNABLE_TO_USE_DAMAGED_SNAPSHOT);
			}
			else
			{
				state->statusAndType = SN_READ_ONLY_TRANSACTON;
				state->snapshotTs = current->timestamp;
				current->occupancy++;
				success = 1;
			}
		}
		else if (ImpGetTimestamp(&state->clientTs))
		{
			AddActiveTimestamp(&snapshots, state->clientTs);
			state->statusAndType = SN_UPDATER_TRANSACTION;
			success = 1;
		}
	}
	return success;
}

int SnOnTransactionEnd(TIMESTAMP *currentSnapshotTs)
{
	int success=0;
	SnClientState *state=NULL;
	SnSnapshot *snapshot=NULL;

	ImpGetCurrentStateBlock(&state);
	if (state)
	{
		switch (state->statusAndType)
		{
			/*	*/ 
		case SN_UPDATER_TRANSACTION:
			RemoveActiveTimestamp(&snapshots, state->clientTs);
			success=1;
			break;
			/*	*/ 
		case SN_READ_ONLY_TRANSACTON:
			GetSnapshotByTimestamp(&snapshots,&snapshot,NULL,state->snapshotTs);
			assert(snapshot);
			snapshot->occupancy--;
			if (PurifySnapshots(&snapshots, 1)) 
			{
				success=1;
			}
			break;
			/*	*/ 
		case SN_COMPLETED_TRANSACTION:
			WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
			break;
		default:
			WuSetLastErrorMacro(WUERR_BAD_PARAMS);
			break;
		}
	}
	if (success)
	{
		state->statusAndType = SN_COMPLETED_TRANSACTION;
		state->timestamp = INVALID_TIMESTAMP;
	}
	if (currentSnapshotTs)
	{
		SnSnapshot *currentSn = GetCurrentSnapshot(&snapshots);
		*currentSnapshotTs = (currentSn ? currentSn->timestamp : INVALID_TIMESTAMP);
	}
	return success;
}

int SnOnUnregisterClient()
{
	int success=0;
	SnClientState *state=NULL;

	ImpGetCurrentStateBlock(&state);
	if (state)
	{
		if (state->statusAndType == 0)
		{
			/* SnOnRegisterClientFailed */ 
			success = 1;
		}
		else if (state->statusAndType == SN_COMPLETED_TRANSACTION)
		{
			success = 1;
		}
		else
		{
			success = SnOnTransactionEnd(NULL);
		}
	}
	return success;
}

int SnSubmitRequestForGc(TIMESTAMP currentSnapshotTs, const SnRequestForGc *buf, size_t count)
{
	static const SnSnapshot infinipast = {};
	int success = 0;
	const SnSnapshot *entrySn = NULL;
	const SnRequestForGc *ebuf = buf + count;

	assert(buf || count==0);
	if (!IsValidTimestamp(currentSnapshotTs) && currentSnapshotTs!=INVALID_TIMESTAMP)
	{
		WuSetLastErrorMacro(WUERR_BAD_TIMESTAMP);
	}
	else
	{
		entrySn = GetCurrentSnapshot(&snapshots);
		while (entrySn && entrySn->timestamp > currentSnapshotTs) entrySn = entrySn->next;
		/*	In normal mode SubmitRequestForGc will start from the current snapshot. 
			In versions-disabled mode SubmitRequestForGc will start from infinipast
			snapshot; that results in blocks submited with SN_REQUEST_ADD_NORMAL_VERSION
			and SN_REQUEST_ADD_BOGUS_VERSION to be discarded imediately. */ 
		if (!entrySn || (setup.flags & SN_SETUP_DISABLE_VERSIONS_FLAG)) entrySn = &infinipast;
		for (; buf!=ebuf; ++buf)
		{
			if (!SubmitRequestForGc(&snapshots, entrySn, buf[0])) break;
		}
		success = (buf==ebuf);
	}
	return success;
}

int SnPurifySnapshots()
{
	return PurifySnapshots(&snapshots, 1);
}

int SnTryAdvanceSnapshots(TIMESTAMP *snapshotTs)
{
	int success = 0;

	assert(snapshotTs);
	*snapshotTs = INVALID_TIMESTAMP;
	if (!PurifySnapshots(&snapshots, 0)) {}
	else if (!EnsureCanAdvanceSnapshots(&snapshots))
	{
		WuSetLastErrorMacro(WUERR_MAX_NUMBER_OF_SNAPSHOTS_EXCEEDED);
	}
	else if (ImpGetTimestamp(snapshotTs) && 
			 CreateSnapshot(&snapshots, *snapshotTs, SN_REGULAR_SNAPSHOT))
	{
	    wulog(("WULOG: Advanced snapshots: newSnTs =%"PRIx64, *snapshotTs));
		snapshots.runawayCount = 0;
		success = 1;
	}
	if (!success) *snapshotTs = INVALID_TIMESTAMP;
	return success;
}

int SnOnBeginCheckpoint(TIMESTAMP *persistentTs)
{
	int success=0;
	SnSnapshot *nextPersSnapshot=NULL, *persSnapshot=NULL, *dummy=NULL;

	assert(persistentTs);
	*persistentTs=INVALID_TIMESTAMP;
	GetSnapshotByType(&snapshots,&persSnapshot,NULL,SN_PERSISTENT_SNAPSHOT);
	
	if (GetSnapshotByType(&snapshots,&dummy,NULL,SN_PREV_PERSISTENT_SNAPSHOT))
	{
		/* someone forgot to call SnOnCompleteCheckpoint, SN_PREV_PERSISTENT_SNAPSHOT is on list */ 
		WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
	}
	else if (!(nextPersSnapshot = GetCurrentSnapshot(&snapshots))) 
	{
		WuSetLastErrorMacro(WUERR_NO_SNAPSHOTS_EXIST);
	}
	else if (nextPersSnapshot->type!=SN_REGULAR_SNAPSHOT)
	{
		WuSetLastErrorMacro(WUERR_SNAPSHOT_ALREADY_PERSISTENT);
	}
	else
	{
		if (persSnapshot) 
		{
			persSnapshot->type=SN_PREV_PERSISTENT_SNAPSHOT;
		}
		nextPersSnapshot->type=SN_PERSISTENT_SNAPSHOT;
		*persistentTs=nextPersSnapshot->timestamp;
		success=1;
	}
	return success;
}

int SnOnCompleteCheckpoint()
{
	int success=0;
	SnSnapshot *persSnapshot=NULL, *prevPersSnapshot=NULL;

	GetSnapshotByType(&snapshots,&prevPersSnapshot,NULL,SN_PREV_PERSISTENT_SNAPSHOT);
	if (!GetSnapshotByType(&snapshots,&persSnapshot,NULL,SN_PERSISTENT_SNAPSHOT))
	{
		/* we do not have persistent snapshot - wtf?*/ 
		WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
	}
	else
	{
		if (prevPersSnapshot)
		{
			prevPersSnapshot->type=SN_REGULAR_SNAPSHOT;			
		}		
		if (PurifySnapshots(&snapshots, 1)) success=1;
	}
	return success;
}

int SnEnumerateVersionsForCheckpoint(SnEnumerateVersionsParams *params,
									 SnEnumerateVersionsProc enumProc)
{
	int success = 0;
	TIMESTAMP persTs = INVALID_TIMESTAMP;
	SnInternalSnapshotStats stats = {};

	assert(params && enumProc);
	if (!SnGetSnapshotTimestamps(NULL, &persTs)) {}
	else if (!GatherInternalStats(&snapshots, &stats, persTs)) {}
	else
	{
		params->persSnapshotTs = persTs;
		params->persVersionsCount = stats.nSnapshotVersions - stats.nSnapshotGarbageVersions;
		params->garbageVersionsCount = stats.nVersions - params->persVersionsCount;
		params->persVersionsSent = 0;
		params->garbageVersionsSent = 0;
		if (stats.nVersions == 0)
		{
			/* if we have no versions at all, call enumProc once */ 
			success = enumProc(params, NULL, 0, 0);
		}
		else
		{
			SnVersionEntry buf[SN_BUFSZ];
			SnEnumerateVersionsBuf enumBuf = {};
			InitEnumerateVersionsBuf(&enumBuf, params, enumProc, buf, SN_BUFSZ);
			if (VisitNodes(&snapshots, persTs, EnumerateVersionsForCheckpointProc, &enumBuf))
			{
				success = FlushEnumerateVersionsBuf(&enumBuf, 1);
			}
		}
	}
	return success;
}

int SnGetSnapshotTimestamps(TIMESTAMP *curSnapshotTs,
							TIMESTAMP *persSnapshotTs)
{
	SnSnapshot *curSn = NULL, *persSn = NULL;
	curSn = GetCurrentSnapshot(&snapshots);
	GetSnapshotByType(&snapshots, &persSn, NULL, SN_PERSISTENT_SNAPSHOT);
	if (curSnapshotTs)
	{
		/*	In versions-disabled mode there is *logically* no current snapshot.
			However curSn may still be non-NULL since we keep 1 snapshot to track
			persSnapshotTs. */ 
		*curSnapshotTs = ((curSn && !(setup.flags & SN_SETUP_DISABLE_VERSIONS_FLAG)) ? 
			curSn->timestamp : INVALID_TIMESTAMP);
	}
	if (persSnapshotTs)
	{
		*persSnapshotTs = (persSn ? persSn->timestamp : INVALID_TIMESTAMP);
	}
	return 1;
}

int SnGatherSnapshotStats(SnSnapshotStats *stats)
{
	SnInternalSnapshotStats statsCur = {}, statsPers = {};
	int success = 0;

	assert(stats); 
	if (SnGetSnapshotTimestamps(&stats->curSnapshotTs, &stats->persSnapshotTs) &&
		GatherInternalStats(&snapshots, &statsCur, stats->curSnapshotTs) &&
		GatherInternalStats(&snapshots, &statsPers, stats->persSnapshotTs))
	{
		stats->versionsCount = statsCur.nVersions;
		stats->runawayVersionsCount = snapshots.runawayCount;
		stats->curSnapshotVersionsCount = statsCur.nSnapshotVersions;
		stats->curSnapshotSharedVersionsCount = statsCur.nSnapshotSharedVersions;
		stats->persSnapshotVersionsCount = statsPers.nSnapshotVersions;
		stats->persSnapshotSharedVersionsCount = statsPers.nSnapshotSharedVersions;
		success = 1;
	}
	if (setup.flags & SN_SETUP_DISABLE_VERSIONS_FLAG)
	{
		stats->isSnapshotSlotAvalible = 0;
	}
	else
	{
		stats->isSnapshotSlotAvalible = EnsureCanAdvanceSnapshots(&snapshots);
	}
	return success;
}

int SnGetTransactionStatusAndType(int *statusAndType)
{
	int success = 0;
	SnClientState *state=NULL;

	assert(statusAndType);
	if (ImpGetCurrentStateBlock(&state))
	{
		*statusAndType = state->statusAndType;
		success = 1;
	}
	return success;
}

int SnGetTransactionSnapshotTs(TIMESTAMP *timestamp)
{
	int success = 0;
	SnClientState *state=NULL;

	assert(timestamp);
	if (ImpGetCurrentStateBlock(&state))
	{
		*timestamp = 
			(state->statusAndType == SN_READ_ONLY_TRANSACTON ? state->timestamp : INVALID_TIMESTAMP);
		success = 1;
	}
	return success;
}

int SnGetDamagedTimestamps(TIMESTAMP *damagedSnapshots, int *tsDamCount)
{
	SnSnapshot *sniter = GetCurrentSnapshot(&snapshots);
	int i = 0;
	
	while (sniter)
	{
		if (sniter->isDamaged)
		{
			damagedSnapshots[i] = sniter->timestamp;
			i++;
		}
		sniter = sniter->next;
	}
	
	*tsDamCount = i;
	return 1;	
}

int SnGetTransactionTs(TIMESTAMP *timestamp)
{
	int success = 0;
	SnClientState *state=NULL;

	assert(timestamp);
	if (ImpGetCurrentStateBlock(&state))
	{
		*timestamp = 
			(state->statusAndType == SN_UPDATER_TRANSACTION ? state->timestamp : INVALID_TIMESTAMP);
		success = 1;
	}
	return success;
}

int SnFilterOutDamaged(TIMESTAMP tsOut[], int idOut[], size_t *szOut)
{
    int i = 0, k;
    int success = 0;
    SnSnapshot *sn = NULL;
    
    while (1)
    {
        if (i >= (int)*szOut)
        {
            success = 1;
            break;
        }
        
        if (tsOut[i] != SN_WORKING_VERSION_TIMESTAMP &&
            tsOut[i] != SN_LAST_COMMITED_VERSION_TIMESTAMP)
        {
            if (!GetSnapshotByTimestamp(&snapshots, &sn, NULL, tsOut[i]))
                break;
        
            if (sn->isDamaged)
            {
                int n = (int)*szOut;
                
                for (k = i; k < n - 1; k++)
                {
                    tsOut[k] = tsOut[k + 1];
                    idOut[k] = idOut[k + 1];
                }
            
                (*szOut)--;
            }
        }
        
        i++;
    }
            
    return success;
}

int SnExpandDfvHeader(const TIMESTAMP tsIn[],
					  size_t szIn,
					  TIMESTAMP tsOut[],
					  int idOut[],
					  size_t *szOut,
					  TIMESTAMP *anchorTsParam,
                      bool isTotalAnchor)
{
	int success = 0;
	SnSnapshot fakeHead = {}, *sniter = &fakeHead;
	const TIMESTAMP *tsInEnd = tsIn + szIn, *tsInCur = tsIn;
	TIMESTAMP *tsOutEnd = NULL, *tsOutCur = tsOut;
	TIMESTAMP anchorTs = INVALID_TIMESTAMP;
	int *idOutCur = idOut;

	assert(szOut && (tsIn || szIn==0) && ((tsOut && idOut) || *szOut==0));
	tsOutEnd = tsOut + *szOut;
	InitFakeHeadSnapshot(&snapshots, &fakeHead);

	if (szIn == 0 || *tsInCur==INVALID_TIMESTAMP) 
	{
		success=1;
	}
	else if(!IsValidTimestamp(*tsInCur))
	{
		WuSetLastErrorMacro(WUERR_BAD_TIMESTAMP);
	}
	else
	{
		if (IsVersionYoungerThanSnapshot(&fakeHead, *tsInCur))
		{
			/* if have room to store record */ 
			if (tsOutCur < tsOutEnd)
			{
				tsOutCur[0] = SN_WORKING_VERSION_TIMESTAMP;
				idOutCur[0] = 0;
			}
			tsOutCur++;
			idOutCur++;
		}
		while(1)
		{
			/* no more snapshots to match against */ 
			if (sniter==NULL) { success=1; break; }
			/* match snapshot version */ 
			while (tsInCur < tsInEnd && 
				   IsValidTimestamp(*tsInCur) && 
			       IsVersionYoungerThanSnapshot(sniter, *tsInCur)) ++tsInCur;
			/* list ended? */ 
			if (tsInCur == tsInEnd || *tsInCur==INVALID_TIMESTAMP) { success=1; break; }
			/* bad timestamp? */ 
			if (!IsValidTimestamp(*tsInCur)) { WuSetLastErrorMacro(WUERR_BAD_TIMESTAMP); break; }
			/* if have room to store record */ 
			if (tsOutCur < tsOutEnd)
			{
				tsOutCur [0] = sniter->timestamp;
				idOutCur [0] = (int)(tsInCur - tsIn);
			}
			/* switch to the next snapshot */ 
			sniter = sniter->next;
			/* offset output position */ 
			++tsOutCur;
			++idOutCur;
		}
	}
	if (success)
	{
		*szOut = tsOutCur - tsOut;
		if (tsOutCur>tsOutEnd)
		{
			WuSetLastErrorMacro(WUERR_GENERAL_ERROR); /* WUERR_INSUFFICIENT_BUFFER */ 
			success = 0;
		}
		else
		{
			int i=0, n=(int)*szOut;
			
			/* locate last commited version slot */ 
			for (i = 0; 
				 i < n && tsOut[i]!=SN_LAST_COMMITED_VERSION_TIMESTAMP; 
				 ++i) {}

			if (i < n)
			{
				/* found last commited version slot */ 
				int idLC = idOut[i];
				
				for (; i<n && idOut[i]==idLC; ++i) {}
				anchorTs = tsOut[i-1];

				if (anchorTs == SN_LAST_COMMITED_VERSION_TIMESTAMP)
				{
					/* currently no snapshots uses this object's versions */ 
					SnGetSnapshotTimestamps(&anchorTs, NULL);
					anchorTs++;
				}
			}			
            
            if (isTotalAnchor)
            {
                // here we must change anchorTs according to the oldest needed version
                if (tsOut[n - 1] != SN_LAST_COMMITED_VERSION_TIMESTAMP &&
                    tsOut[n - 1] != SN_WORKING_VERSION_TIMESTAMP)            
                    anchorTs = tsOut[n - 1];
            }
		}
	}
	else
	{
		*szOut = 0;
	}
    
	if (anchorTsParam) *anchorTsParam = anchorTs;
    
	return success;
}

int SnDamageSnapshots(TIMESTAMP timestampFrom)
{
	int success=0;

	if (!IsValidTimestamp(timestampFrom))
	{
		WuSetLastErrorMacro(WUERR_BAD_TIMESTAMP);
	}
	else if (setup.flags & SN_SETUP_DISABLE_VERSIONS_FLAG)
	{
		WuSetLastErrorMacro(WUERR_VERSIONS_DISABLED);
	}
	else
	{
		SnSnapshot *sniter = GetCurrentSnapshot(&snapshots);
		while (1)
		{
			if (sniter==NULL) { success=1; break; }
			if (sniter->timestamp <= timestampFrom)
			{
				if (sniter->occupancy>0) 
				{
					WuSetLastErrorMacro(WUERR_UNABLE_TO_DAMAGE_SNAPSHOT_IN_USE); 
					break;
				}
				sniter->isDamaged = 1;
			}
			sniter=sniter->next;
		}
	}
	return success;
}

void SnDbgDump(int flags)
{
	static const char *padding = "          ";
	if (flags & SN_DUMP_ATIMESTAMPS)
	{
		wulog(("active TS:"));
		DbgDumpTimestamps(snapshots.tsBegin, snapshots.tsEnd, padding, "");
	}

	if (flags & SN_DUMP_SNAPSHOTS)
	{		
		wulog(("snapshots:"));
		DbgDumpSnapshots(&snapshots, flags, padding, "");
	}

	if (flags & SN_DUMP_GC_NODES)
	{
		wulog(("GC nodes: "));
		DbgDumpGcNodes(
			&snapshots, INT_MAX, 
			(flags & SN_DUMP_UNLIMITED_FLAG ? INT_MAX : SN_DUMP_LIMIT), 
			padding, "");
	}
}
