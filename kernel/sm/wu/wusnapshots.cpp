#include <stdio.h>
#include <list>
#include <algorithm>
#include "wutypes.h"
#include "wuclients.h"
#include "wusnapshots.h"
#include "wuaux.h"

#define SH_SNAPSHOTS_COUNT	3
#define SH_GC_NODES_COUNT	((SH_SNAPSHOTS_COUNT*SH_SNAPSHOTS_COUNT+SH_SNAPSHOTS_COUNT)/2)
#define SH_BUFSZ			1024
#define SH_DUMP_LIMIT		8

/* defs */ 

struct SnapshotsClientState
{
	TIMESTAMP snapshotTs;						/*	the snapshot timestamp (0 if not using sh) */ 
};

struct SnapshotsGcEntry
{
	LXPTR lxptr;	/*	logical XPTR of the version */ 
	XPTR xptr;		/*	physical XPTR of the version */ 
};

struct SnapshotsGcNode
{
	std::list<SnapshotsGcEntry> entries;
	SnapshotsGcNode *next;
};

struct SnapshotsSnapshot
{
	TIMESTAMP timestamp;					/*	timestamp assigned to snapshot (future snapshots
												are timestamped with ~(TIMESTAMP)0, since TIMESTAMP is
												unsigned it is the maximum possible ts) */ 
	TIMESTAMP discardedTs;
	SnapshotsGcNode *gcChain;				/*	each snapshot has a single linked chain of gcList nodes
												attached; the first list contains pushed versions
												belonging to this snapshot ONLY; the second list
												contains pushed versions belonging to this snapshot
												AND the one in front of this snapshot, etc. */ 
	int type;								/*	snapshot type - see below */ 
	int occupancy;							/*	the number of clients using this snapshot */  
	SnapshotsSnapshot *next;				/*	snapshots are arranged in a single linked list
												sorted by their timestamps in decreasing order
												(future snapshots first, then other types of sh) */ 
};

/*	Just the normal snapshot - has a timestamp assigned and 0+ clients using it; regular 
	snapshot is	automaticly discarded as soon as it's occupancy counter reaches zero AND 
	a more recent snapshot different from SH_FUTURE_SNAPSHOT exists. Can be turned into
	SH_NEXT_PERSISTENT_SNAPSHOT during checkpoint or SH_FUTURE_SNAPSHOT when discarded. */ 
#define SH_REGULAR_SNAPSHOT				0x01

/*	Persistent snapshot is never discarded. Clients are allowed to use it as if it was 
	SH_REGULAR_SNAPSHOT. It is turned into SH_PREV_PERSISTENT_SNAPSHOT during checkpoint. */ 
#define SH_PERSISTENT_SNAPSHOT			0x02

/*	The most recent SH_REGULAR_SNAPSHOT is turned into SH_NEXT_PERSISTENT_SNAPSHOT as the
	result of ShOnBeginCheckpoint call. It is later turned into SH_PERSISTENT_SNAPSHOT when
	ShOnCheckpoint is called. */ 
#define SH_NEXT_PERSISTENT_SNAPSHOT		0x04

/*	ShOnCheckpoint finds SH_PERSISTENT_SNAPSHOT and marks it as SH_PREV_PERSISTENT_SNAPSHOT. 
	It then turns SH_NEXT_PERSISTENT_SNAPSHOT and turns it into SH_PERSISTENT_SNAPSHOT. Finally
	when ShOnCompleteCheckpoint is called SH_PREV_PERSISTENT_SNAPSHOT is turned into 
	SH_REGULAR_SNAPSHOT and rules for auto discarding SH_REGULAR_SNAPSHOT apply. */ 
#define SH_PREV_PERSISTENT_SNAPSHOT		0x08

/*	There is a fixed number of snapshots allowed - SH_SNAPSHOTS_COUNT. When particular snapshot
	is currently unused it is marked SH_FUTURE_SNAPSHOT and placed in the head of snapshots
	list. */ 
#define SH_FUTURE_SNAPSHOT				0x10

/* global state */ 

static SnapshotsSetup setup;
static TICKET ticket=NULL;
static SnapshotsSnapshot snapshots[SH_SNAPSHOTS_COUNT], *leadingSnapshot=NULL;
static SnapshotsGcNode gcNodes[SH_GC_NODES_COUNT];

/* helper functions */ 

static void ResetLists()
{
	int i=0, j=0, k=0;
	leadingSnapshot=snapshots+0;
	for (i=0;i<SH_SNAPSHOTS_COUNT;++i)
	{
		snapshots[i].timestamp = ~(TIMESTAMP)0;
		snapshots[i].discardedTs = 0;
		snapshots[i].next = snapshots+i+1;
		snapshots[i].gcChain = gcNodes+k;
		snapshots[i].type = SH_FUTURE_SNAPSHOT;
		snapshots[i].occupancy = 0;
		for (j=0;j<=i;++j)
		{
			gcNodes[k].entries.clear();
			gcNodes[k].next=gcNodes+k+1;
			++k;
		}
		gcNodes[k-1].next=NULL;
	}
	snapshots[SH_SNAPSHOTS_COUNT-1].next = NULL;
}

static
int PurgeVersions(std::list<SnapshotsGcEntry> *versList)
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
int GetSnapshotByTimestamp(SnapshotsSnapshot *head,
						   SnapshotsSnapshot **result, 
						   SnapshotsSnapshot **prev, 
						   TIMESTAMP ts)
{
	int success=0, pos=0;
	SnapshotsSnapshot *dummy=NULL;

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
			ERROR(WUERR_NO_SNAPSHOT_WITH_THIS_TIMESTAMP);
		}
	}
	return success;
}

static
int GetSnapshotByType(SnapshotsSnapshot *head,
					  SnapshotsSnapshot **result, 
					  SnapshotsSnapshot **prev, 
					  int typeIncl, int typeExcl)
{
	int success=0, pos=0;
	SnapshotsSnapshot *dummy=NULL;

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
			ERROR(WUERR_NO_SNAPSHOT_WITH_THIS_TYPE);
		}
	}
	return success;
}

static 
int GetGcChainNode(SnapshotsGcNode *hd, SnapshotsGcNode **lst, int depth)
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
void RotateGcChain(SnapshotsGcNode *hd, int victimId, int depth)
{
	SnapshotsGcNode *i=NULL, *j=NULL;
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
SnapshotsSnapshot *RotateSnapshots(SnapshotsSnapshot *hd, 
								   SnapshotsSnapshot *beforeVictim,
								   SnapshotsSnapshot **unaffected)
{
	SnapshotsSnapshot *victim=NULL, *iter=NULL;
	SnapshotsGcNode *listsHeap=NULL, *lists2MergeInto=NULL, *bot=NULL;
	int depth=0;

	assert(hd && unaffected);
	if (!beforeVictim) victim=hd;
	else victim=beforeVictim->next;
	assert(victim && victim->gcChain);

	victim->gcChain->entries.clear();
	victim->type=SH_FUTURE_SNAPSHOT;
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
	SnapshotsSnapshot *victim=NULL, *beforeVictim=NULL, *tail=NULL;
	int victimId=0, depth=0;

	victimId = GetSnapshotByTimestamp(leadingSnapshot, &victim, &beforeVictim, ts)-1;
	if (victimId==-1)
	{
		; /* bad timestamp or something */ 
	}
	else if (victim->type != SH_REGULAR_SNAPSHOT)
	{
		ERROR(WUERR_UNABLE_TO_DISCARD_SPECIAL_SNAPSHOT);
	}
	else if (victim->occupancy > 0)
	{
		ERROR(WUERR_UNABLE_TO_DISCARD_SNAPSHOT_IN_USE);
	}
	else if (PurgeVersions(&victim->gcChain->entries))
	{
		victim->discardedTs=discardedTs;
		leadingSnapshot=RotateSnapshots(leadingSnapshot, beforeVictim, &tail);
		for (depth=victimId+2; tail; tail=tail->next,++depth) 
		{
			RotateGcChain(tail->gcChain, victimId, depth);
		}
		assert(depth=SH_SNAPSHOTS_COUNT+1);
		success=1;
	}
	return success;
}

static
int CreateSnapshot(TIMESTAMP *ts)
{
	int success=0;
	SnapshotsSnapshot *current=NULL, *beforeCurrent=NULL;

	assert(ts);
	if (!setup.getTimestamp(ts))
	{
		; /* something wrong */ 
	}
	else 
	{
		GetSnapshotByType(leadingSnapshot,&current,&beforeCurrent,-1,SH_FUTURE_SNAPSHOT);
		if (!current)
		{
			beforeCurrent=leadingSnapshot;
			while (beforeCurrent->next) 
			{
				assert (beforeCurrent->type == SH_FUTURE_SNAPSHOT);
				beforeCurrent=beforeCurrent->next;
			}
		}
		if (beforeCurrent)
		{
			beforeCurrent->type=SH_REGULAR_SNAPSHOT;
			beforeCurrent->timestamp=*ts;
			success=1;
		}
		else
		{
			ERROR(WUERR_MAX_NUMBER_OF_SNAPSHOTS_EXCEEDED);
		}
	}
	if (!success)
	{
		*ts=0;
	}
	return success;
}

static 
SnapshotsSnapshot * GetCurrentSnapshot()
{
	SnapshotsSnapshot *result=NULL;
	GetSnapshotByType(leadingSnapshot,&result,NULL,-1,SH_FUTURE_SNAPSHOT);
	return result;
}

static
size_t GetGcChainVersionsCount(SnapshotsGcNode *head)
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
void GatherSnapshotsStats(SnapshotsSnapshot *head,
						  size_t *totalVersionsCount,
						  size_t *snapshotVersionsCount,
						  size_t *snapshotSharedVersionsCount,
						  TIMESTAMP snapshotTs)
{
	SnapshotsSnapshot *i=NULL;
	SnapshotsGcNode *top=NULL;
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
int FlushTransmitBuf(SnapshotsOnCheckpointParams *params,
					 int(*saveListsProc)(SnapshotsOnCheckpointParams*,SnapshotsVersion*,size_t,int),
					 SnapshotsVersion *buf,
					 SnapshotsVersion *ebuf,
					 SnapshotsVersion **ibuf,
					 int isGarbage)
{
	size_t count=0;
	int failure=0;
	
	assert(params && saveListsProc && buf && ebuf && ibuf && *ibuf && *ibuf>=buf && *ibuf<=ebuf);
	count=*ibuf-buf;
	if (count>0)
	{
		if(!saveListsProc(params,buf,count,isGarbage)) failure=1;
		if(isGarbage) 
		{
			params->garbageVersionsSent+=count;
		}
		else 
		{
			params->persistentVersionsSent+=count;
		}
		*ibuf=buf;
	}
	return failure==0;
}

static 
int TransmitGcChain(SnapshotsGcNode *head,
					int length,
					SnapshotsOnCheckpointParams *params,
					int(*saveListsProc)(SnapshotsOnCheckpointParams*,SnapshotsVersion*,size_t,int),
					SnapshotsVersion *buf,
					SnapshotsVersion *ebuf,
					SnapshotsVersion **ibuf,
					int isGarbage)
{
	std::list<SnapshotsGcEntry>::iterator it;
	SnapshotsVersion *i=NULL;
	int failure=0;

	assert(head && params && saveListsProc && buf && ebuf && ibuf && *ibuf);
	i=*ibuf;

	if (head) it=head->entries.begin();
	while (!failure && head && length>0)
	{
		for(; i<ebuf && it!=head->entries.end(); ++i, ++it)
		{
			i->xptr=it->xptr;
			i->lxptr=it->lxptr;
		}
		if(i==ebuf)
		{
			if (!FlushTransmitBuf(params,saveListsProc,buf,ebuf,&i,isGarbage)) failure=1;
		}
		else if (it==head->entries.end())
		{
			head=head->next;
			if (head) it=head->entries.begin();
			--length;
		}
	}

	*ibuf=i;
	assert(length==0 || failure);
	return (failure==0);
}

static
int PurifySnapshots(int isKeepingCurrentSnapshot)
{
	int failure=0;
	SnapshotsSnapshot *current=NULL;
	TIMESTAMP ts=0;

	current=GetCurrentSnapshot();
	if (current && isKeepingCurrentSnapshot) current=current->next;
	while (!failure && current)
	{
		ts=current->timestamp;
		if (current->occupancy!=0 || current->type!=SH_REGULAR_SNAPSHOT)
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

int ShInitialise()
{	
	return 1;
}

void ShQueryResourceDemand(SnapshotsResourceDemand *demand)
{
	assert(demand);
	demand->clientStateSize = sizeof(SnapshotsClientState);
}

int ShStartup(SnapshotsSetup *psetup)
{
	int success=1;

	assert(psetup);
	setup=*psetup;
	ticket=setup.clientStateTicket;
	ResetLists();

	return success;
}

int ShShutdown()
{
	int failure=0, i=0;
	size_t versCount=0;
	SnapshotsSnapshot *curr=NULL, *pers=NULL;
	if (GetSnapshotByType(leadingSnapshot,&pers,0,SH_PERSISTENT_SNAPSHOT,0))
	{
		GatherSnapshotsStats(leadingSnapshot,NULL,&versCount,NULL,pers->timestamp);
		if (versCount==0)
		{
			pers->type=SH_REGULAR_SNAPSHOT;
		}
		else
		{
			ERROR(WUERR_SHUTDOWN_ERROR);
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
		ERROR(WUERR_SHUTDOWN_ERROR);
	}
	if (!failure) ResetLists();
	return failure==0;
}

void ShDeinitialise()
{
}

int ShOnRegisterClient(int isUsingSnapshot, TIMESTAMP *snapshotTs)
{
	int success=0;
	SnapshotsClientState *state=NULL;
	SnapshotsSnapshot *current=NULL;

	ClGetCurrentStateBlock((void**)&state,ticket);
	current=GetCurrentSnapshot();
	if (state)
	{
		if (isUsingSnapshot) 
		{
			if (!current)
			{
				ERROR(WUERR_NO_SNAPSHOTS_EXIST);
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

int ShOnUnregisterClient()
{
	int success=0;
	SnapshotsClientState *state=NULL;
	SnapshotsSnapshot *snapshot=NULL;

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

int ShAcceptRequestForGc(TIMESTAMP operationTs, SnapshotsRequestForGc *buf, size_t count)
{
	int failure=0, ofs=0;
	SnapshotsRequestForGc *ebuf=NULL;
	SnapshotsSnapshot *top=NULL, *i=NULL;
	SnapshotsGcNode *gcNode=NULL;
	SnapshotsGcEntry entry;
	
	assert(buf || count==0);
	ebuf=buf+count;
	for (top=leadingSnapshot; top && top->timestamp>operationTs; top=top->next);
	GetSnapshotByType(top,&top,NULL,-1,SH_FUTURE_SNAPSHOT);

	if (!top) 
	{
		for (;buf<ebuf && !failure; ++buf)
		{
			if (!setup.freeBlock(buf->snapshotVersionXptr)) failure=1;
		}
	}
	else 
	{
		for (;buf<ebuf && !failure; ++buf)
		{
			if (top->timestamp <= buf->anchorTs)
			{
				if (!setup.freeBlock(buf->snapshotVersionXptr)) failure=1;
			}
			else
			{
				for (ofs=0, i=top; i->next && i->next->timestamp > buf->anchorTs; ++ofs, i=i->next);
				GetGcChainNode(i->gcChain,&gcNode,ofs);
				assert(gcNode);
				entry.lxptr=buf->lxptr;
				entry.xptr=buf->snapshotVersionXptr;
				gcNode->entries.push_back(entry);
			}
		}
	}

	return (failure==0);
}

int ShAdvanceSnapshots(TIMESTAMP *snapshotTs, TIMESTAMP *discardedTs)
{
	static int AdvanceSnapshotsRecursion=0;
	int success=0, canAdvance=0;
	SnapshotsSnapshot *current=NULL;
	TIMESTAMP dummyTs=0;

	assert(snapshotTs);
	if (!discardedTs) discardedTs=&dummyTs;
	*snapshotTs=0;
	*discardedTs=0;

	if (AdvanceSnapshotsRecursion>0)
	{
		ERROR(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
	}
	else
	{
		AdvanceSnapshotsRecursion++;
		if (!ShCheckIfCanAdvanceSnapshots(&canAdvance,NULL))
		{
			; /* something wrong */ 
		}
		else if (canAdvance==0)
		{
			ERROR(WUERR_UNABLE_TO_ADVANCE_SNAPSHOTS);
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
	return success;
}

int ShOnBeginCheckpoint(TIMESTAMP *persistentTs)
{
	int success=0;
	SnapshotsSnapshot *s=NULL, *dummy=NULL;

	assert(persistentTs);
	*persistentTs=0;
	s=GetCurrentSnapshot();
	if (!s)
	{
		; /* something wrong */ 
	}
	else if (GetSnapshotByType(leadingSnapshot,&dummy,NULL,SH_NEXT_PERSISTENT_SNAPSHOT,0))
	{
		ERROR(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
	}
	else if (s->type!=SH_REGULAR_SNAPSHOT)
	{
		ERROR(WUERR_SNAPSHOT_ALREADY_PERSISTENT);
	}
	else
	{
		s->type=SH_NEXT_PERSISTENT_SNAPSHOT;
		*persistentTs=s->timestamp;
		success=1;
	}
	return success;
}

int ShOnCheckpoint(SnapshotsOnCheckpointParams *params,
				   int(*saveListsProc)(SnapshotsOnCheckpointParams*,SnapshotsVersion*,size_t,int))
{
	int success=0, failure=0, persPos=0, i=0;
	size_t total=0, persistent=0;
	SnapshotsSnapshot *nextPers=NULL, *pers=NULL, *dummy=NULL, *iter=NULL;
	SnapshotsGcNode *top=NULL;
	SnapshotsVersion buf[SH_BUFSZ], *ibuf=buf, *ebuf=buf+SH_BUFSZ;

	assert(params && saveListsProc);
	persPos = GetSnapshotByType(leadingSnapshot,&nextPers,NULL,SH_NEXT_PERSISTENT_SNAPSHOT,0)-1;
	GetSnapshotByType(leadingSnapshot,&pers,NULL,SH_PERSISTENT_SNAPSHOT,0);
	if (!nextPers || GetSnapshotByType(leadingSnapshot,&dummy,NULL,SH_PREV_PERSISTENT_SNAPSHOT,0))
	{
		ERROR(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
	}
	else
	{
		if (pers) 
		{
			assert(!GetSnapshotByType(pers->next,&dummy,NULL,SH_PERSISTENT_SNAPSHOT,0));
			pers->type=SH_PREV_PERSISTENT_SNAPSHOT;
		}
		pers=nextPers;
		pers->type=SH_PERSISTENT_SNAPSHOT;

		GatherSnapshotsStats(leadingSnapshot,&total,&persistent,NULL,pers->timestamp);
		params->persistentVersionsCount=persistent;
		params->garbageVersionsCount=total-persistent;
		params->persistentVersionsSent=0;
		params->garbageVersionsSent=0;
		
		/* if neither garbage nor persistent versions exist call saveListsProc once */ 
		if (params->persistentVersionsCount==0 && params->garbageVersionsCount==0)
		{
			if(!saveListsProc(params,NULL,0,0)) failure=1;
		}
		else
		{
			/* transmit persistent parallelogram */ 
			for(i=0, iter=pers; !failure && iter; iter=iter->next, ++i)
			{
				GetGcChainNode(iter->gcChain,&top,i);
				assert(top);
				if (!TransmitGcChain(top,persPos+1,params,saveListsProc,buf,ebuf,&ibuf,0)) failure=1;
			}
			/* flush buffer */ 
			if (!failure)
			{
				if (!FlushTransmitBuf(params,saveListsProc,buf,ebuf,&ibuf,0)) failure=1;
			}

			/* transmit first garbage triangle */ 
			for(i=1, iter=leadingSnapshot; !failure && iter!=pers; iter=iter->next, ++i)
			{
				if (!TransmitGcChain(iter->gcChain,i,params,saveListsProc,buf,ebuf,&ibuf,1)) failure=1;
			}
			/* transmit second garbage triangle */ 
			for(i=1, iter=pers->next; !failure && iter; iter=iter->next, ++i)
			{
				if (!TransmitGcChain(iter->gcChain,i,params,saveListsProc,buf,ebuf,&ibuf,1)) failure=1;
			}
			/* flush buffer */ 
			if (!failure)
			{
				if (!FlushTransmitBuf(params,saveListsProc,buf,ebuf,&ibuf,1)) failure=1;
			}
		}		
		success=(failure==0);
	}
	return success;
}

int ShOnCompleteCheckpoint()
{
	int success=0;
	SnapshotsSnapshot *prevPers=NULL, *dummy=NULL;

	if (GetSnapshotByType(leadingSnapshot,&dummy,NULL,SH_NEXT_PERSISTENT_SNAPSHOT,0))
	{
		ERROR(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
	}
	if (!GetSnapshotByType(leadingSnapshot,&prevPers,NULL,SH_PREV_PERSISTENT_SNAPSHOT,0))
	{
		success=1;
	}
	else
	{
		assert(!GetSnapshotByType(prevPers->next,&dummy,NULL,SH_PREV_PERSISTENT_SNAPSHOT,0));
		prevPers->type=SH_REGULAR_SNAPSHOT;
		if (PurifySnapshots(1)) success=1;
	}
	return success;
}

int ShGatherStats(SnapshotsStats *stats)
{
	TIMESTAMP ts=0;
	SnapshotsSnapshot *sh=NULL;

	assert(stats); 
	sh=GetCurrentSnapshot();
	ts = (sh? sh->timestamp: 0);
	GatherSnapshotsStats(leadingSnapshot,
						 &stats->versionsCount,
						 &stats->curSnapshotVersionsCount,
						 &stats->curSnapshotSharedVersionsCount,
						 ts);

	GetSnapshotByType(leadingSnapshot,&sh,NULL,SH_PERSISTENT_SNAPSHOT,0);
	ts = (sh? sh->timestamp: 0);
	GatherSnapshotsStats(leadingSnapshot,
						 NULL,
						 &stats->persSnapshotVersionsCount,
						 &stats->persSnapshotSharedVersionsCount,
						 ts);
	return 1;
}

int ShCheckIfCanAdvanceSnapshots(int *canAdvance, int *canMakeCurrentSnapshotPersistent)
{
	SnapshotsSnapshot *current=NULL, *last=NULL, *criterion=NULL;
	int dummy=0;

	assert(canAdvance);	
	last=leadingSnapshot;
	assert(last);
	while (last->next) last=last->next;
	criterion=(last->occupancy==0?leadingSnapshot:leadingSnapshot->next);

	*canAdvance=(criterion->type==SH_FUTURE_SNAPSHOT || 
				 criterion->type==SH_REGULAR_SNAPSHOT && criterion->occupancy==0);
	current=GetCurrentSnapshot();
	dummy=(current && current->type==SH_REGULAR_SNAPSHOT);
	if (canMakeCurrentSnapshotPersistent) *canMakeCurrentSnapshotPersistent=dummy;
	return 1;
}

int ShGetCurrentClientSnapshotTs(TIMESTAMP *timestamp)
{
	int success=0;
	SnapshotsClientState *state=NULL;
	assert(timestamp);
	if (ClGetCurrentStateBlock((void**)&state,ticket))
	{
		*timestamp=state->snapshotTs;
		success=1;
	}
	return success;
}

void ShDbgDump(int reserved)
{
	SnapshotsSnapshot *it=NULL;
	SnapshotsGcNode *hscan[SH_SNAPSHOTS_COUNT];
	std::list<SnapshotsGcEntry>::iterator xscan[SH_SNAPSHOTS_COUNT];
	int i=0, j=0, con=1;
	const char *typeStr=NULL;
	char buf[32];
	SnapshotsStats stats;

	ShGatherStats(&stats);
	fprintf(stderr,"ShDbgDump total %d, cur %d, cur-s %d, pers %d, pers-s %d\n", 
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
		case SH_REGULAR_SNAPSHOT:
			typeStr="REGULAR"; break;
		case SH_PERSISTENT_SNAPSHOT:
			typeStr="PERSISTENT"; break;
		case SH_NEXT_PERSISTENT_SNAPSHOT:
			typeStr="NEXT-PERS."; break;
		case SH_PREV_PERSISTENT_SNAPSHOT:
			typeStr="PREV-PERS."; break;
		case SH_FUTURE_SNAPSHOT:
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
		for (i=0;i<SH_SNAPSHOTS_COUNT;++i)
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
		/*	for each gcChain dump elements (either until all elements are dumped or SH_DUMP_LIMIT is hit) */ 
		for (j=0; j<SH_DUMP_LIMIT && con; ++j)
		{
			con=0;
			for (i=0;i<SH_SNAPSHOTS_COUNT;++i)
			{
				*buf=0;
				if (hscan[i] && xscan[i]!=hscan[i]->entries.end())
				{
					sprintf(buf,"%0.8X:%0.8X",(uint32_t)xscan[i]->lxptr,(uint32_t)xscan[i]->xptr);
					++xscan[i];
					if (xscan[i]!=hscan[i]->entries.end()) 
					{
						++con;
						if (j==SH_DUMP_LIMIT-1) strcpy(buf,"........");
					}
				}
				fprintf(stderr,"   %17s",buf);
			}
			fputs("\n",stderr);
		}
		/*	descent one level down gcChains, con remains zero unless any chain has a node at next level */ 
		con=0;
		for (i=0;i<SH_SNAPSHOTS_COUNT;++i)
		{
			if (hscan[i])
			{
				++con;
				hscan[i]=hscan[i]->next;
			}
		}
	}
}
