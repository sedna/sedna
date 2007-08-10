#define __WUDANG_SOURCES__

#include <stdio.h>
#include <list>
#include <algorithm>
#include "wutypes.h"
#include "wuclients.h"
#include "wusnapshots.h"
#include "wuaux.h"
#include "wuerr.h"

#define SN_SNAPSHOTS_COUNT	3
#define SN_GC_NODES_COUNT	((SN_SNAPSHOTS_COUNT*SN_SNAPSHOTS_COUNT+SN_SNAPSHOTS_COUNT)/2)
#define SN_BUFSZ			1024
#define SN_DUMP_LIMIT		8

/* defs */ 

struct SnClientState
{
	TIMESTAMP snapshotTs;						/*	the snapshot timestamp (0 if not using sh) */ 
};

struct SnGcNode
{
	std::list<SnVersionEntry> entries;
	SnGcNode *next;
};

struct SnEnumerateVersionsBuf
{
	SnVersionEntry *buf, *ebuf, *ibuf;
	int isGarbageInside;
};

struct SnSnapshot
{
	TIMESTAMP timestamp;					/*	timestamp assigned to snapshot (future snapshots
												are timestamped with ~(TIMESTAMP)0, since TIMESTAMP is
												unsigned it is the maximum possible ts) */ 
	TIMESTAMP discardedTs;
	SnGcNode *gcChain;				/*	each snapshot has a single linked chain of gcList nodes
												attached; the first list contains pushed versions
												belonging to this snapshot ONLY; the second list
												contains pushed versions belonging to this snapshot
												AND the one in front of this snapshot, etc. */ 
	int type;								/*	snapshot type - see below */ 
	int occupancy;							/*	the number of clients using this snapshot */  
	SnSnapshot *next;				/*	snapshots are arranged in a single linked list
												sorted by their timestamps in decreasing order
												(future snapshots first, then other types of sh) */ 
};

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
	list. */ 
#define SN_FUTURE_SNAPSHOT				0x10

/* global state */ 

static int isInitialized = 0;
static TICKET ticket=NULL;
static SnSetup setup={};
static SnSnapshot snapshots[SN_SNAPSHOTS_COUNT], *leadingSnapshot=NULL;
static SnGcNode gcNodes[SN_GC_NODES_COUNT];
static size_t runawayVersionsCount=0;

/* helper functions */ 

static void ResetLists()
{
	int i=0, j=0, k=0;
	leadingSnapshot=snapshots+0;
	for (i=0;i<SN_SNAPSHOTS_COUNT;++i)
	{
		snapshots[i].timestamp = ~(TIMESTAMP)0;
		snapshots[i].discardedTs = 0;
		snapshots[i].next = snapshots+i+1;
		snapshots[i].gcChain = gcNodes+k;
		snapshots[i].type = SN_FUTURE_SNAPSHOT;
		snapshots[i].occupancy = 0;
		for (j=0;j<=i;++j)
		{
			gcNodes[k].entries.clear();
			gcNodes[k].next=gcNodes+k+1;
			++k;
		}
		gcNodes[k-1].next=NULL;
	}
	snapshots[SN_SNAPSHOTS_COUNT-1].next = NULL;
}

static
int PurgeVersions(std::list<SnVersionEntry> *versList)
{
	int failure=0;
	assert(versList);
	while (!versList->empty() && !failure)
	{
		if (!setup.freeBlock(versList->front().xptr)) failure=1;
		else versList->pop_front();
	}
	return failure==0;
}

static
int GetSnapshotByTimestamp(SnSnapshot *head,
						   SnSnapshot **result, 
						   SnSnapshot **prev, 
						   TIMESTAMP ts)
{
	int success=0, pos=0;
	SnSnapshot *dummy=NULL;

	assert(result);
	if (!prev) prev=&dummy;
	*result=NULL;
	*prev=NULL;

	if (!head) goto notfound;
	if (head->timestamp==ts)
	{
		*result=head;
		*prev=NULL;
		success=1;
	}
	else
	{
		++pos;
		while (head->next && head->next->timestamp>ts) 
		{ 
			head=head->next; pos++; 
		}
		if (head->next && head->next->timestamp == ts)
		{
			*result=head->next;
			*prev=head;
			success=1+pos;
		}
		else
		{
notfound:
			WuSetLastErrorMacro(WUERR_NO_SNAPSHOT_WITH_THIS_TIMESTAMP);
		}
	}
	return success;
}

static
int GetSnapshotByType(SnSnapshot *head,
					  SnSnapshot **result, 
					  SnSnapshot **prev, 
					  int typeIncl, int typeExcl)
{
	int success=0, pos=0;
	SnSnapshot *dummy=NULL;

	assert(result);
	if (!prev) prev=&dummy;
	*result=NULL;
	*prev=NULL;

	if (!head) goto notfound;
	if ((head->type & typeIncl) && !(head->type & typeExcl))
	{
		*result=head;
		*prev=NULL;
		success=1;
	}
	else
	{
		++pos;
		while(head->next && (!(head->next->type & typeIncl) || (head->next->type & typeExcl))) 
		{ 
			head=head->next; pos++; 
		}
		if (head->next && (head->next->type & typeIncl) && !(head->next->type & typeExcl))
		{
			*result=head->next;
			*prev=head;
			success=pos+1;
		}
		else
		{
notfound:
			WuSetLastErrorMacro(WUERR_NO_SNAPSHOT_WITH_THIS_TYPE);
		}
	}
	return success;
}

static 
int GetGcChainNode(SnGcNode *hd, SnGcNode **lst, int depth)
{
	assert(lst);
	*lst=NULL;
	while (hd && depth>0)
	{
		hd=hd->next;
		--depth;
	}
	return depth==0 && (*lst=hd);
}

static 
void RotateGcChain(SnGcNode *hd, int victimId, int depth)
{
	SnGcNode *i=NULL, *j=NULL;
	assert(hd);
	GetGcChainNode(hd,&i,depth-victimId-2);
	GetGcChainNode(i,&j,victimId+1);
	assert(i && j && i!=j && j->next==NULL);
	i->entries.splice(i->entries.end(),i->next->entries);
	j->next=i->next;
	i->next=j->next->next;
	j->next->next=NULL;
}

static
SnSnapshot *RotateSnapshots(SnSnapshot *hd, 
							SnSnapshot *beforeVictim,
							SnSnapshot **unaffected)
{
	SnSnapshot *victim=NULL, *iter=NULL;
	SnGcNode *listsHeap=NULL, *lists2MergeInto=NULL, *bot=NULL;
	int depth=0;

	assert(hd && unaffected);
	if (!beforeVictim) victim=hd;
	else victim=beforeVictim->next;
	assert(victim && victim->gcChain);

	victim->gcChain->entries.clear();
	victim->type=SN_FUTURE_SNAPSHOT;
	victim->timestamp=~(TIMESTAMP)0;

	if (victim==hd)
	{
		assert(victim->gcChain->next==NULL);
		*unaffected=victim->next;
	}
	else
	{
		listsHeap=victim->gcChain->next;
		lists2MergeInto=beforeVictim->gcChain;
		victim->gcChain->next=NULL;
		for (iter=hd,depth=1;iter!=victim;++depth,iter=iter->next)
		{
			assert (listsHeap && lists2MergeInto);
			lists2MergeInto->entries.splice(lists2MergeInto->entries.end(),listsHeap->entries);
			lists2MergeInto=lists2MergeInto->next;
			GetGcChainNode(iter->gcChain,&bot,depth-1);
			assert(bot && bot->next==NULL);
			bot->next=listsHeap;
			listsHeap=listsHeap->next;
			bot->next->next=NULL;
		}
		assert(listsHeap==NULL && lists2MergeInto==NULL);
		*unaffected=beforeVictim->next=victim->next;
		victim->next=hd;
		hd=victim;
	}
	return hd;
}

static
int DiscardSnapshot(TIMESTAMP ts, TIMESTAMP discardedTs)
{
	int success=0;
	SnSnapshot *victim=NULL, *beforeVictim=NULL, *tail=NULL;
	int victimId=0, depth=0;

	victimId = GetSnapshotByTimestamp(leadingSnapshot, &victim, &beforeVictim, ts)-1;
	if (victimId==-1)
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
	else if (PurgeVersions(&victim->gcChain->entries))
	{
		victim->discardedTs=discardedTs;
		leadingSnapshot=RotateSnapshots(leadingSnapshot, beforeVictim, &tail);
		for (depth=victimId+2; tail; tail=tail->next,++depth) 
		{
			RotateGcChain(tail->gcChain, victimId, depth);
		}
		assert(depth=SN_SNAPSHOTS_COUNT+1);
		success=1;
	}
	return success;
}

static
int CreateSnapshot(TIMESTAMP *ts)
{
	int success=0;
	SnSnapshot *current=NULL, *beforeCurrent=NULL;

	assert(ts);
	if (!setup.getTimestamp(ts))
	{
		; /* something wrong */ 
	}
	else 
	{
		GetSnapshotByType(leadingSnapshot,&current,&beforeCurrent,-1,SN_FUTURE_SNAPSHOT);
		if (!current)
		{
			beforeCurrent=leadingSnapshot;
			while (beforeCurrent->next) 
			{
				assert (beforeCurrent->type == SN_FUTURE_SNAPSHOT);
				beforeCurrent=beforeCurrent->next;
			}
		}
		if (beforeCurrent)
		{
			beforeCurrent->type=SN_REGULAR_SNAPSHOT;
			beforeCurrent->timestamp=*ts;
			success=1;
		}
		else
		{
			WuSetLastErrorMacro(WUERR_MAX_NUMBER_OF_SNAPSHOTS_EXCEEDED);
		}
	}
	if (!success)
	{
		*ts=0;
	}
	return success;
}

static 
SnSnapshot * GetCurrentSnapshot()
{
	SnSnapshot *result=NULL;
	GetSnapshotByType(leadingSnapshot,&result,NULL,-1,SN_FUTURE_SNAPSHOT);
	return result;
}

static
size_t GetGcChainVersionsCount(SnGcNode *head)
{
	size_t result=0;
	while (head)
	{
		result+=head->entries.size();
		head=head->next;
	}
	return result;
}

static
void GatherSnapshotsStats(SnSnapshot *head,
						  size_t *totalVersionsCount,
						  size_t *snapshotVersionsCount,
						  size_t *snapshotSharedVersionsCount,
						  TIMESTAMP snapshotTs)
{
	SnSnapshot *i=NULL;
	SnGcNode *top=NULL;
	int ofs=0;
	size_t cntT=0, cntX=0;

	if (totalVersionsCount)
	{
		*totalVersionsCount=0;
		for (i=head; i; i=i->next) *totalVersionsCount+=GetGcChainVersionsCount(i->gcChain);
	}
	if (snapshotVersionsCount || snapshotSharedVersionsCount)
	{
		if (snapshotTs!=0 && GetSnapshotByTimestamp(head,&head,NULL,snapshotTs))
		{
			assert(head->gcChain);
			cntT=0;
			cntX=head->gcChain->entries.size();
			for (i=head, ofs=0; i; i=i->next, ++ofs) 
			{
				GetGcChainNode(i->gcChain,&top,ofs);
				assert(top);
				cntT+=GetGcChainVersionsCount(top);
			}
		}
		if (snapshotVersionsCount) *snapshotVersionsCount=cntT;
		if (snapshotSharedVersionsCount) *snapshotSharedVersionsCount=cntT-cntX;
	}
}

static 
int FlushEnumerateVersionsBuf(SnEnumerateVersionsParams *params,
							  SnEnumerateVersionsProc enumProc,
							  SnEnumerateVersionsBuf *buf)
{
	size_t count=0;
	int failure=0;
	
	assert(params && enumProc && buf);
	count = buf->ibuf-buf->buf;
	if (count>0)
	{
		if(!enumProc(params,buf->buf,count,buf->isGarbageInside)) failure=1;
		if(buf->isGarbageInside) 
		{
			params->garbageVersionsSent+=count;
		}
		else 
		{
			params->persVersionsSent+=count;
		}
		buf->ibuf=buf->buf;
	}
	return failure==0;
}

static 
int EnumerateGcChainVersions(SnGcNode *head,
							 int length,
							 SnEnumerateVersionsParams *params,
							 SnEnumerateVersionsProc enumProc,
							 SnEnumerateVersionsBuf *buf,
							 SnEnumerateVersionsBuf *garbageBuf)
{
	SnEnumerateVersionsBuf *selectedBuf = NULL;
	std::list<SnVersionEntry>::iterator it;
	int failure=0;

	assert(head && params && enumProc && buf && garbageBuf);

	for (;head && length>0 && !failure; head=head->next, --length)
	{
		for (it=head->entries.begin(); it!=head->entries.end(); ++it)
		{
			selectedBuf = (it->lxptr == ~(LXPTR)0 ? garbageBuf : buf);
			if (selectedBuf->ibuf >= selectedBuf->ebuf && 
				!FlushEnumerateVersionsBuf(params,enumProc,selectedBuf))
			{
				failure=1;
				break;
			}
			*(selectedBuf->ibuf++)=*it;
		}
	}
	return (failure==0);
}

static
int PurifySnapshots(int isKeepingCurrentSnapshot)
{
	int failure=0;
	SnSnapshot *current=NULL;
	TIMESTAMP ts=0;

	current=GetCurrentSnapshot();
	if (current && isKeepingCurrentSnapshot) current=current->next;
	while (!failure && current)
	{
		ts=current->timestamp;
		if (current->occupancy!=0 || current->type!=SN_REGULAR_SNAPSHOT)
		{
			current=current->next;
		}
		else
		{
			if (!setup.onDiscardSnapshot(ts))
			{
				failure=1;
			}
			else if (current->timestamp!=ts)
			{
				/*	callback somehow managed to dispose snapshot
					pointed by current ptr, exiting */ 
				current=NULL;				
			}
			else
			{
				current=current->next;
				if (!DiscardSnapshot(ts,ts)) failure=1;
			}
		}
	}
	return failure==0;
}

/* public api functions */ 

int SnInitialize()
{	
	runawayVersionsCount=0;
	isInitialized = 1;
	return 1;
}

void SnQueryResourceDemand(SnResourceDemand *demand)
{
	assert(demand);
	demand->clientStateSize = sizeof(SnClientState);
}

int SnStartup(SnSetup *psetup)
{
	int success=0;
	SnSnapshot *snapshot=NULL;
	TIMESTAMP initialPersSnapshotTs=0, curTs=0;

	assert(psetup);
	setup=*psetup;
	ticket=setup.clientStateTicket;
	initialPersSnapshotTs=setup.initialPersSnapshotTs;
	ResetLists();
	if (initialPersSnapshotTs == 0)
	{
		success = 1;
	}
	else
	{
		if (!setup.getTimestamp(&curTs) || initialPersSnapshotTs >= curTs)
		{
			WuSetLastErrorMacro(WUERR_BAD_TIMESTAMP);
		}
		else
		{
			snapshot=leadingSnapshot;
			while (snapshot->next) snapshot=snapshot->next;
			snapshot->type=SN_PERSISTENT_SNAPSHOT;
			snapshot->timestamp=initialPersSnapshotTs;
			success = 1;
		}
	}	
	return success;
}

int SnShutdown()
{
	int failure=0, i=0;
	size_t versCount=0;
	SnSnapshot *curr=NULL, *pers=NULL;
	if (GetSnapshotByType(leadingSnapshot,&pers,0,SN_PERSISTENT_SNAPSHOT,0))
	{
		GatherSnapshotsStats(leadingSnapshot,NULL,&versCount,NULL,pers->timestamp);
		if (versCount==0)
		{
			pers->type=SN_REGULAR_SNAPSHOT;
		}
		else
		{
			WuSetLastErrorMacro(WUERR_SHUTDOWN_ERROR);
			failure=1;
		}
	}
	if (failure)
	{
		; /* something wrong */ 
	}
	else if (!PurifySnapshots(0)) failure=1;
	else if (GetCurrentSnapshot())
	{
		failure=1;
		WuSetLastErrorMacro(WUERR_SHUTDOWN_ERROR);
	}
	if (!failure) ResetLists();
	return failure==0;
}

void SnDeinitialize()
{
	isInitialized = 0;
}

int SnOnRegisterClient(int isUsingSnapshot, TIMESTAMP *snapshotTs)
{
	int success=0;
	SnClientState *state=NULL;
	SnSnapshot *current=NULL;

	ClGetCurrentStateBlock((void**)&state,ticket);
	current=GetCurrentSnapshot();
	if (state)
	{
		if (isUsingSnapshot) 
		{
			if (!current)
			{
				WuSetLastErrorMacro(WUERR_NO_SNAPSHOTS_EXIST);
			}
			else
			{
				state->snapshotTs=current->timestamp;
				current->occupancy++;
				success=1;
			}
		}
		else
		{
			state->snapshotTs=~(TIMESTAMP)0;
			success=1;
		}
		if (snapshotTs) *snapshotTs=state->snapshotTs;
	}
	return success;
}

int SnOnUnregisterClient()
{
	int success=0;
	SnClientState *state=NULL;
	SnSnapshot *snapshot=NULL;

	ClGetCurrentStateBlock((void**)&state,ticket);
	if (state)
	{
		if (state->snapshotTs==~(TIMESTAMP)0)
		{
			success=1;
		}
		else
		{
			GetSnapshotByTimestamp(leadingSnapshot,&snapshot,NULL,state->snapshotTs);
			assert(snapshot);
			snapshot->occupancy--;
			if (PurifySnapshots(1)) success=1;
		}
	}
	return success;
}

int SnAcceptRequestForGc(TIMESTAMP operationTs, SnRequestForGc *buf, size_t count)
{
	int failure=0, ofs=0;
	SnRequestForGc *ebuf=NULL;
	SnSnapshot *top=NULL, *i=NULL;
	SnGcNode *gcNode=NULL;
	SnVersionEntry entry;
	
	assert(buf || count==0);
	ebuf=buf+count;
	for (top=leadingSnapshot; top && top->timestamp>operationTs; top=top->next);
	GetSnapshotByType(top,&top,NULL,-1,SN_FUTURE_SNAPSHOT);

	if (!top) 
	{
		for (;buf<ebuf && !failure; ++buf)
		{
			++runawayVersionsCount;
			if (!setup.freeBlock(buf->xptr)) failure=1;
		}
	}
	else 
	{
		for (;buf<ebuf && !failure; ++buf)
		{
			if (top->timestamp <= buf->anchorTs)
			{
				++runawayVersionsCount;
				if (!setup.freeBlock(buf->xptr)) failure=1;
			}
			else
			{
				for (ofs=0, i=top; i->next && i->next->timestamp > buf->anchorTs; ++ofs, i=i->next);
				GetGcChainNode(i->gcChain,&gcNode,ofs);
				assert(gcNode);
				entry.lxptr=buf->lxptr;
				entry.xptr=buf->xptr;
				gcNode->entries.push_back(entry);
			}
		}
	}

	return (failure==0);
}

int SnAdvanceSnapshots(TIMESTAMP *snapshotTs, TIMESTAMP *discardedTs)
{
	static int AdvanceSnapshotsRecursion=0;
	int success=0;
	SnSnapshot *current=NULL;
	TIMESTAMP dummyTs=0;

	assert(snapshotTs);
	if (!discardedTs) discardedTs=&dummyTs;
	*snapshotTs=0;
	*discardedTs=0;

	if (AdvanceSnapshotsRecursion>0)
	{
		WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
	}
	else
	{
		AdvanceSnapshotsRecursion++;
		if(!SnCheckIfCanAdvanceSnapshots(NULL))
		{
			WuSetLastErrorMacro(WUERR_UNABLE_TO_ADVANCE_SNAPSHOTS);
		}
		else if (!PurifySnapshots(0))
		{
			; /* something wrong */ 
		}
		else if (CreateSnapshot(snapshotTs))
		{
			GetSnapshotByTimestamp(leadingSnapshot,&current,NULL,*snapshotTs);
			assert(current);
			*discardedTs=current->discardedTs;			
			success=1;
		}
		AdvanceSnapshotsRecursion--;
	}
	if (success)
	{
		runawayVersionsCount=0;
	}
	return success;
}

int SnOnBeginCheckpoint(TIMESTAMP *persistentTs)
{
	int success=0;
	SnSnapshot *snapshot=NULL, *persSnapshot=NULL, *dummy=NULL;

	assert(persistentTs);
	*persistentTs=0;
	snapshot = GetCurrentSnapshot();
	GetSnapshotByType(leadingSnapshot,&persSnapshot,NULL,SN_PERSISTENT_SNAPSHOT,0);
	
	if (GetSnapshotByType(leadingSnapshot,&dummy,NULL,SN_PREV_PERSISTENT_SNAPSHOT,0))
	{
		WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
	}
	else if (!snapshot) {}
	else if (snapshot->type!=SN_REGULAR_SNAPSHOT)
	{
		WuSetLastErrorMacro(WUERR_SNAPSHOT_ALREADY_PERSISTENT);
	}
	else
	{
		if (persSnapshot) 
		{
			persSnapshot->type=SN_PREV_PERSISTENT_SNAPSHOT;
		}
		snapshot->type=SN_PERSISTENT_SNAPSHOT;
		*persistentTs=snapshot->timestamp;
		success=1;
	}
	return success;
}

int SnEnumerateVersionsForCheckpoint(SnEnumerateVersionsParams *params,
									 SnEnumerateVersionsProc enumProc)
{
	int success=0, failure=0, persPos=0, i=0;
	size_t total=0, persistent=0;
	SnSnapshot *pers=NULL, *dummy=NULL, *iter=NULL;
	SnGcNode *top=NULL;
	SnVersionEntry bufStorg[SN_BUFSZ], garbageBufStorg[SN_BUFSZ];
	SnEnumerateVersionsBuf buf = {bufStorg, bufStorg, bufStorg+SN_BUFSZ, 0};
	SnEnumerateVersionsBuf garbageBuf = 
	{
		garbageBufStorg, garbageBufStorg, garbageBufStorg+SN_BUFSZ, 0
	};

	assert(params && enumProc);
	persPos = GetSnapshotByType(leadingSnapshot,&pers,NULL,SN_PERSISTENT_SNAPSHOT,0)-1;
	params->persSnapshotTs = (pers?pers->timestamp:0);
	GatherSnapshotsStats(leadingSnapshot,&total,&persistent,NULL,params->persSnapshotTs);
	params->persVersionsCount=persistent;
	params->garbageVersionsCount=total-persistent;
	params->persVersionsSent=0;
	params->garbageVersionsSent=0;

	if (1)
	{
		/* if neither garbage nor persistent versions exist call saveListsProc once */ 
		if (params->persVersionsCount==0 && params->garbageVersionsCount==0)
		{
			if(!enumProc(params,NULL,0,0)) failure=1;
		}
		else
		{
			/* transmit persistent parallelogram */ 
			for(i=0, iter=pers; !failure && iter; iter=iter->next, ++i)
			{
				GetGcChainNode(iter->gcChain,&top,i);
				assert(top);
				if (!EnumerateGcChainVersions(top,persPos+1,params,enumProc,&buf,&garbageBuf)) failure=1;
			}
			/* flush buffer */ 
			if (!failure)
			{
				if (!FlushEnumerateVersionsBuf(params,enumProc,&buf)) failure=1;
			}

			/* transmit first garbage triangle */ 
			for(i=1, iter=leadingSnapshot; !failure && iter!=pers; iter=iter->next, ++i)
			{
				if (!EnumerateGcChainVersions(iter->gcChain,i,params,enumProc,&garbageBuf,&garbageBuf)) failure=1;
			}
			/* transmit second garbage triangle */ 
			for(i=1, iter=pers->next; !failure && iter; iter=iter->next, ++i)
			{
				if (!EnumerateGcChainVersions(iter->gcChain,i,params,enumProc,&garbageBuf,&garbageBuf)) failure=1;
			}
			/* flush garbage buffer */ 
			if (!failure)
			{
				if (!FlushEnumerateVersionsBuf(params,enumProc,&garbageBuf)) failure=1;
			}
		}		
		success = (failure==0);
	}
	return success;
}

int SnOnCompleteCheckpoint()
{
	int success=0;
	SnSnapshot *pers=NULL, *prevPers=NULL, *dummy=NULL;

	GetSnapshotByType(leadingSnapshot,&prevPers,NULL,SN_PREV_PERSISTENT_SNAPSHOT,0);
	if (!GetSnapshotByType(leadingSnapshot,&pers,NULL,SN_PERSISTENT_SNAPSHOT,0))
	{
		WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
	}
	else
	{
		if (prevPers)
		{
			prevPers->type=SN_REGULAR_SNAPSHOT;			
		}		
		if (PurifySnapshots(1)) success=1;
	}
	return success;
}

int SnGatherStats(SnStats *stats)
{
	TIMESTAMP ts=0;
	SnSnapshot *snapshot=NULL;

	assert(stats); 
	stats->runawayVersionsCount=runawayVersionsCount;
	snapshot=GetCurrentSnapshot();
	ts = (snapshot? snapshot->timestamp: 0);
	GatherSnapshotsStats(leadingSnapshot,
						 &stats->versionsCount,
						 &stats->curSnapshotVersionsCount,
						 &stats->curSnapshotSharedVersionsCount,
						 ts);

	GetSnapshotByType(leadingSnapshot,&snapshot,NULL,SN_PERSISTENT_SNAPSHOT,0);
	ts = (snapshot? snapshot->timestamp: 0);
	GatherSnapshotsStats(leadingSnapshot,
						 NULL,
						 &stats->persSnapshotVersionsCount,
						 &stats->persSnapshotSharedVersionsCount,
						 ts);
	return 1;
}

int SnCheckIfCanAdvanceSnapshots(int *canMakeCurrentSnapshotPersistent)
{
	SnSnapshot *current=NULL, *last=NULL, *criterion=NULL;
	int dummy=0, canAdvance=0;

	last=leadingSnapshot;
	assert(last);
	while (last->next) last=last->next;
	criterion=(last->occupancy==0?leadingSnapshot:leadingSnapshot->next);

	canAdvance=(criterion->type==SN_FUTURE_SNAPSHOT || 
				criterion->type==SN_REGULAR_SNAPSHOT && criterion->occupancy==0);
	current=GetCurrentSnapshot();
	dummy=(current && current->type==SN_REGULAR_SNAPSHOT);
	if (canMakeCurrentSnapshotPersistent) *canMakeCurrentSnapshotPersistent=dummy;
	return canAdvance;
}

int SnGetCurrentSnapshotTs(TIMESTAMP *timestamp)
{
	int success=0;
	SnClientState *state=NULL;
	assert(timestamp);
	if (ClGetCurrentStateBlock((void**)&state,ticket))
	{
		*timestamp=state->snapshotTs;
		success=1;
	}
	return success;
}

void SnDbgDump(int reserved)
{
	SnSnapshot *it=NULL;
	SnGcNode *hscan[SN_SNAPSHOTS_COUNT];
	std::list<SnVersionEntry>::iterator xscan[SN_SNAPSHOTS_COUNT];
	int i=0, j=0, con=1;
	const char *typeStr=NULL;
	char buf[32];
	SnStats stats;

	SnGatherStats(&stats);
	fprintf(stderr,"SnDbgDump total %d, cur %d, cur-s %d, pers %d, pers-s %d\n", 
		stats.versionsCount,
		stats.curSnapshotVersionsCount,
		stats.curSnapshotSharedVersionsCount,
		stats.persSnapshotVersionsCount,
		stats.persSnapshotSharedVersionsCount);

	fprintf(stderr,"   %-16s   %-10s   %-16s   %-6s\n","ts","type","disc.-ts","occup.");
	for (i=0,it=leadingSnapshot; it; ++i, it=it->next)
	{
		switch (it->type)
		{
		case SN_REGULAR_SNAPSHOT:
			typeStr="REGULAR"; break;
		case SN_PERSISTENT_SNAPSHOT:
			typeStr="PERSISTENT"; break;
		case SN_PREV_PERSISTENT_SNAPSHOT:
			typeStr="PREV-PERS."; break;
		case SN_FUTURE_SNAPSHOT:
			typeStr="FUTURE"; break;
		default:
			typeStr="UNKNOWN";
		}
		fprintf(stderr,"   %0.16I64X   %-10s   %0.16I64X   %6d\n",
			it->timestamp,
			typeStr,
			it->discardedTs,			
			it->occupancy);
		hscan[i]=it->gcChain;
	}
	fputs("   ---------------------------------------------------------\n",stderr);
	/* for each depth level... (serving all gcChains simultaneously) */ 
	while(con)
	{		
		con=0;
		/*	for each gcChain output headers (num of elements)
			unless any node has non-zero elements con remains zero*/ 
		for (i=0;i<SN_SNAPSHOTS_COUNT;++i)
		{
			*buf=0;
			if (hscan[i]) 
			{
				xscan[i]=hscan[i]->entries.begin();
				sprintf(buf,"[%d]",hscan[i]->entries.size());
				if (hscan[i]->entries.size()>0) ++con;
			}
			fprintf(stderr,"   %17s",buf);
		}
		fputs("\n",stderr);
		/*	for each gcChain dump elements (either until all elements are dumped or SN_DUMP_LIMIT is hit) */ 
		for (j=0; j<SN_DUMP_LIMIT && con; ++j)
		{
			con=0;
			for (i=0;i<SN_SNAPSHOTS_COUNT;++i)
			{
				*buf=0;
				if (hscan[i] && xscan[i]!=hscan[i]->entries.end())
				{
					sprintf(buf,"%0.8X:%0.8X",(uint32_t)xscan[i]->lxptr,(uint32_t)xscan[i]->xptr);
					++xscan[i];
					if (xscan[i]!=hscan[i]->entries.end()) 
					{
						++con;
						if (j==SN_DUMP_LIMIT-1) strcpy(buf,"........");
					}
				}
				fprintf(stderr,"   %17s",buf);
			}
			fputs("\n",stderr);
		}
		/*	descent one level down gcChains, con remains zero unless any chain has a node at next level */ 
		con=0;
		for (i=0;i<SN_SNAPSHOTS_COUNT;++i)
		{
			if (hscan[i])
			{
				++con;
				hscan[i]=hscan[i]->next;
			}
		}
	}
}

