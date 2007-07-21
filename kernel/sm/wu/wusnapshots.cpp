#include <list>
#include <algorithm>
#include "wutypes.h"
#include "wuversions.h"
#include "wusnapshots.h"
#include "wuaux.h"

#define SH_SNAPSHOTS_COUNT	3
#define SH_GC_LISTS_COUNT	((SH_SNAPSHOTS_COUNT*SH_SNAPSHOTS_COUNT+SH_SNAPSHOTS_COUNT)/2)

/* defs */ 

struct SnapshotsClientState
{
	TIMESTAMP snapshotTs;						/*	the snapshot timestamp (0 if not using sh) */ 
	std::list<VersionsCreateVersionParams>
			*pushedVersions;					/*	list of versions becoming subject to gc due
													to transaction activity (undefined if using sh) */ 
};

struct SnapshotsGcEntry
{
	LXPTR lxptr;	/*	logical XPTR of the version */ 
	XPTR xptr;		/*	physical XPTR of the version */ 
};

struct SnapshotsGcList
{
	std::list<SnapshotsGcEntry> entries;
	SnapshotsGcNode *next;
};

struct SnapshotsSnapshot
{
	TIMESTAMP timestamp;					/*	timestamp assigned to snapshot (future snapshots
												are timestamped with ~(TIMESTAMP)0, since TIMESTAMP is
												unsigned it is the maximum possible ts) */ 
	TIMESTEMP discardedTs;
	SnapshotsSnapshot *next;				/*	snapshots are arranged in a single linked list
												sorted by their timestamps in decreasing order
												(future snapshots first, then other types of sh) */ 
	SnapshotsGcList *gcChain;				/*	each snapshot has a single linked chain of gcList nodes
												attached; the first list contains pushed versions
												belonging to this snapshot ONLY; the second list
												contains pushed versions belonging to this snapshot
												AND the one in front of this snapshot, etc. */ 
	int type;								/*	snapshot type - see below */ 
	int occupancy;							/*	the number of clients using this snapshot */  
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
static SnapshotsGcList gcLists[SH_GC_LISTS_COUNT];

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
		snapshots[i].gcLists = gcLists+k;
		snapshots[i].type = SH_FUTURE_SNAPSHOT;
		snapshots[i].occupancy = 0;
		for (j=0;j<=i;++j)
		{
			gcLists[k].entries.clear();
			gcLists[k].next=gcLists+k+1;
			++k;
		}
		gcLists[k-1].next=NULL;
	}
	snapshots[SH_SNAPSHOTS_COUNT-1]->next = NULL;
}

static
int GetSnapshotByTimestamp(SnapshotsSnapshot *head,
						   SnapshotsSnapshot **result, 
						   SnapshotsSnapshot **prev, 
						   TIMESTAMP ts)
{
	int success=0, pos=0;
	SnapshotsSnapshot *dummy=NULL;

	assert(head && result);
	if (!prev) prev=&dummy;
	*result=NULL;
	*prev=NULL;

	if (head->timestamp==ts)
	{
		*result=head;
		*prev=NULL;
		*success=1;
	}
	else
	{
		while (head->next && head->next->timestamp>ts) 
		{ 
			head=head->next; pos++; 
		}
		if (head->next && head->next->timestamp == ts)
		{
			*result=head->next;
			*prev=head;
			success=1+pos
		}
		else
		{
			/* ERROR: "no snapshot with this timestamp" */ 
			ERROR("no snapshot with this timestamp");
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

	assert(head && result);
	if (!prev) prev=&dummy;
	*result=NULL;
	*prev=NULL;

	if ((head->type & typeIncl) && !(head->type & typeExcl))
	{
		*result=head;
		*prev=NULL;
		success=1;
	}
	else
	{
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
			/* ERROR: "no snapshot with this type" */ 
			ERROR("no snapshot with this type");
		}
	}
	return success;
}

static 
int GetGcChainNode(SnapshotsGcList *hd, SnapshotsGcList **lst, int depth)
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
void RotateGcChain(SnapshotsGcList *hd, int victimId, int depth)
{
	SnapshotsGcList *i=NULL, *j=NULL;
	assert(sh);
	GetGcChainNode(hd,&i,depth-victimId-2);
	GetGcChainNode(i,&j,victimId+1);
	assert(i && j && i!=j && j->next==NULL);
	i->entries.splice(i->entries.begin(),i->next->entries);
	j->next=i->next;
	i->next=j->next;
	j->next=NULL;
}

static
SnapshotsSnapshot *RotateSnapshots(SnapshotsSnapshot *hd, 
								   SnapshotsSnapshot *beforeVictim,
								   SnapshotsSnapshot **unaffected)
{
	SnapshotsSnapshot *victim=NULL, *iter=NULL;
	SnapshotsGcList *listsHeap=NULL, lists2MergeInto=NULL, *bot=NULL;
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
			lists2MergeInto->entries.splice(lists2MergeInto->entries.begin(),listsHeap->entries);
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
		/* ERROR: "attempting to discard non-regular snapshot" */ 
		ERROR("attempting to discard non-regular snapshot");
	}
	else if (victim->occupancy > 0)
	{
		/* ERROR: "attempting to discard currently used snapshot" */ 
		ERROR("attempting to discard currently used snapshot");
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
	if (!setup->getTimestamp(ts))
	{
		; /* something wrong */ 
	}
	else 
	{
		GetSnapshotByType(leadingSnapshot,&current,&beforeCurrent,-1,SH_FUTURE_SNAPSHOT)
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
			beforeCurrent.type=SH_REGULAR_SNAPSHOT;
			beforeCurrent.timestamp=*ts;
			success=1;
		}
		else
		{
			/* ERROR: "maximim number of snapshots exceeded" */ 
			ERROR("maximim number of snapshots exceeded");
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
	GetSnapshotByType(leadingSnapshot,&resut,NULL,-1,SH_FUTURE_SNAPSHOT);
	return result;
}

static
size_t GetGcChainVersionsCount(SnapshotsGcList *head)
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
	SnapshotsGcList *top=NULL;
	int ofs=0;
	size_t cntT=0, cntX=0;

	if (totalVersionsCount)
	{
		*totalVersionsCount=0;
		for (i=head; i; i=i->next) *totalVersionsCount+=GetGcChainVersionsCount(i->gcChain);
	}
	if (snapshotVersionsCount || snapshotSharedVersionsCount)
	{
		if (GetSnapshotByTimestamp(head,&head,NULL,snapshotTs))
		{
			assert(head->gcChain);
			cntT=0;
			cntX=head->gcChain->entries.size();
			for (i=head, ofs=0; i; u=i->next, ++ofs) 
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
int TransmitGcChain(SnapshotsGcList *head,
					int length,
					SnapshotsOnCheckpointInfo *onCheckpointInfo,
					int(*saveListsProc)(SnapshotsOnCheckpointInfo*,SnapshotsVersionInfo*,size_t),
					int isGarbage)
{
	success=0;
	return success;
}

static
int PurifySnapshots(int isKeepingCurrentSnapshot)
{
	int failure=0, isAllowed=0;
	SnapshotsSnapshot *current=NULL;
	TIMESTAMP ts=0, victimTs=0;

	current=GetCurrentSnapshot();
	if (current && isKeepingCurrentSnapshot) current=current->next;
	if (current) ts=current->timestamp;
	current=0;
	/*	callbacks can call functions affecting snapshots list so
		we never save pointer to snapshots across callback call */ 
	while (!failure && GetSnapshotByTimestamp(leadingSnapshot,&current,0,ts))
	{
		victimTs=ts;
		ts=(current->next?current->next->timestamp:0);
		if (current->occupancy==0 && current->type==SH_REGULAR_SNAPSHOT)
		{
			current=NULL;
			if (!setup->onDiscardSnapshot(victimTs,&isAllowed))
			{
				failure=1;
			}
			else if (isAllowed)
			{
				if (!DiscardSnapshot(victimTs,victimTs)) failure=1;
			}
		}
	}
	if (!failure && leadingSnapshot->type==SH_FUTURE_SNAPSHOT)
	{
		if (!setup->onCanAdvanceSnapshots()) failure=1;
	}
	return failure==0;
}

static
int PurgeVersions(std::list<SnapshotsGcEntry> *versList)
{
	int failure=0;
	assert(versList);
	while (!versList->empty() && !failure)
	{
		if (!setup->freeBlock(versList->front().xptr)) failure=1;
		else versLists->pop_front();
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
	int failure=0; i=0;
	for (i=0; i<SH_GC_LISTS_COUNT && !failure; ++i) failure=!PurgeVersions(&gcLists[i]->entries);
	if (!failure) ResetLists();
	return failure==0;
}

void ShDeinitialise()
{
}

int ShOnRegisterClient(SnapshotsClientInfo *clientInfo)
{
	int success=0;
	SnapshotsClientState *state=NULL;
	SnapshotsSnapshot *current;

	assert(clientInfo);
	clientInfo->snapshotTs=0;
	ClGetCurrentStateBlock((void**)&state,ticket);
	if (state)
	{
		if (clientInfo->isUsingSnapshot) 
		{
			current=GetCurrentSnapshot();
			if (!current)
			{
				/* ERROR: "no snapshots avail" */ 
				ERROR("no snapshots avail");
			}
			else
			{
				state->snapshotTs=current->timestamp;
				current->occupancy++;
			}
		}
		else
		{
			state->snapshotTs=0;
			state->prevId=state->nextId=-1;
			state->pushedVersions=new std::list<VersionsCreateVersionParams>;
			success=1;
		}			
	}
	if (success)
	{
		clientInfo->snapshotTs=state->snapshotTs;
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
		if (state->snapshotTs==0)
		{
			delete state->pushedVersions;
			state->pushedVersions=NULL;
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

int ShOnCreateVersion(VersionsCreateVersionParams *params)
{
	int success=0;
	SnapshotsClientState *state=NULL;
	if (!ClGetCurrentClientStateBlock((void**)state,ticket))
	{
		; /* something wrong */ 
	}
	else if (state->timestamp!=0)
	{
		/* ERROR: "snapshot user created a version" */ 
		ERROR("snapshot user created a version");
	}
	else
	{
		assert(state->pushedVersions);
		state->pushedVersions.push_back(params);
	}
	return success;
}

int ShOnRollback()
{
	/*	TODO: call revertBlock for each block in versions list and kill blocks
		and reset list */ 
	return 0;
}

int ShOnCommit()
{
	/*	TODO: process pushedVersions list (posting entries to other lists & probably
		killing blocks) and reset list */ 
	return 0;
}

int ShAdvanceSnapshots(TIMESTAMP *snapshotTs, TIMESTAMP *discardedTs);
{
	static int AdvanceSnapshotsRecursion=0;
	int success=0, failure=0, isDiscardingOk=0;
	SnapshotsSnapshot *current=NULL;
	TIMESTAMP dummyTs=0;

	assert(snapshotTs);
	if (!discardedTs) discardedTs=&dummyTs;
	*snapshotTs=0;
	*discardedTs=0;

	if (AdvanceSnapshotsRecursion>0)
	{
		/* ERROR: "snapshots advance already in progress" */ 
		ERROR("snapshots advance already in progress");
	}
	else
	{
		AdvanceSnapshotsRecursion++;

		current=GetCurrentSnapshot();
		if (IsSnapshotDiscardable(current))
		{
			if (!setup->onDiscardSnapshot(current->timestamp,&isDiscardingOk))
			{
				failure=1; /* failure in callback */ 
			}
			else if (isDiscardingOk) failure=!DiscardSnapshot(current->timestamp,current->timestamp);
			current=NULL;
		}
		if (!failure && CreateSnapshot(snapshotTs))
		{
			GetSnapshotByTimestamp(leadingSnapshot,&current,NULL,*snapshotTs);
			assert(current);
			*discardedTs=current->discardedTs;

			if (!PurifySnapshots(1)) 
			{
				failure=1;
			}
			else if (!setup->onCurrentSnapshotGrowing(0,0)) 
			{
				failure=1;
			}
			
			if (failure) DiscardSnapshot(*snapshotTs,*discardedTs));

			success=!failure;		
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
		/* ERROR: "snapshots onCheckpoint protocol violation" */ 
		ERROR("snapshots onCheckpoint protocol violation");
	}
	else if (s->type!=SH_REGULAR_SNAPSHOT)
	{
		/* ERROR: "current snapshot already persistent" */ 
		ERROR("current snapshot already persistent");
	}
	else
	{
		s->type=SH_NEXT_PERSISTENT_SNAPSHOT;
		*persistentTs=s->timestamp;
		success=1;
	}
	return success;
}

int ShOnCheckpoint(SnapshotsOnCheckpointInfo *onCheckpointInfo,
				   int(*saveListsProc)(SnapshotsOnCheckpointInfo*,SnapshotsVersionInfo*,size_t))
{
	int success=0;
	SnapshotsSnapshot *nextPers=NULL, *pers=NULL, *dummy=NULL;

	assert(onCheckpointInfo && saveListsProc);
	GetSnapshotByType(leadingSnapshot,&nextPers,NULL,SH_NEXT_PERSISTENT_SNAPSHOT,0);
	GetSnapshotByType(leadingSnapshot,&pers,NULL,SH_PERSISTENT_SNAPSHOT,0);
	if (!nextPers || GetSnapshotByType(leadingSnapshot,&dummy,NULL,SH_PREV_PERSISTENT_SNAPSHOT,0))
	{
		/* ERROR: "snapshots onCheckpoint protocol violation" */ 
		ERROR("snapshots onCheckpoint protocol violation");
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
	}
	return success;
}

int ShOnCompleteCheckpoint()
{
	int success=0;
	SnapshotsSnapshot *prevPers=NULL, *dummy=NULL;

	if (GetSnapshotByType(leadingSnapshot,&dummy,NULL,SH_NEXT_PERSISTENT_SNAPSHOT,0))
	{
		/* ERROR: "snapshots onCheckpoint protocol violation" */ 
		ERROR("snapshots onCheckpoint protocol violation");
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
