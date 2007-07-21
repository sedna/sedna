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
int GetGcList(SnapshotsGcList *hd, SnapshotsGcList **lst, int depth)
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
	GetGcList(hd,&i,depth-victimId-2);
	GetGcList(i,&j,victimId+1);
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
	victim->discardedTs = victim->timestamp;
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
			GetGcList(iter->gcChain,&bot,depth-1);
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
int DiscardSnapshot(TIMESTAMP ts)
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
	else
	{
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
SnapshotsSnapshot * GetCurrentSnapshot()
{
	SnapshotsSnapshot *result=NULL;
	GetSnapshotByType(leadingSnapshot,&resut,NULL,-1,SH_FUTURE_SNAPSHOT);
	return result;
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

	/* TODO: initialise lists & snapshot ctrl blocks and stuff */ 

	return success;
}

int ShShutdown()
{
	/*	TODO: kill every block from global lists. */ 
	return 0;
}

void ShDeinitialise()
{
}

int ShOnRegisterClient(SnapshotsClientInfo *clientInfo)
{
	int success=0;
	SnapshotsClientState *state=NULL;

	assert(clientInfo);
	clientInfo->snapshotTs=0;
	ClGetCurrentStateBlock((void**)&state,ticket);
	if (state)
	{
		if (clientInfo->isUsingSnapshot) 
		{
			/* TODO: assign proper value to snapshotTs and update counters */ 
			
		}
		else
		{
			state->snapshotTs=0;
			state->prevId=state->nextId=-1;
			state->pushedVersions=new std::list<VersionsCreateVersionParams>;
		}	
		success=1;
	}
	if (success)
	{
		clientInfo->snapshotTs=state->snapshotTs;
	}
	return success;
}

int ShOnUnregisterClient()
{
	int success=0, isUsingSnapshot=0;
	SnapshotsClientState *state=NULL;
	ClGetCurrentStateBlock((void**)&state,ticket);
	if (state)
	{
		isUsingSnapshot = (state->snapshotTs!=0);
		if (!isUsingSnapshot)
		{
			delete state->pushedVersions;
			state->pushedVersions=NULL;
		}
		/* TODO: update counters and call onCanAdvanceSnapshots if apropriate */ 
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

int ShAdvanceSnapshots()
{
	/*	TODO: check if we can really advance the snapshot and do the work */ 
	return 0;
}

int ShOnBeginCheckpoint(TIMESTAMP *persistentTs)
{
	/*	TODO: mark latest snapshot "becoming persistent", if we already have 
		one snapshot marked "becoming persistent" report failure; ShOnCheckpoint will 
		later change the mark to "persistent". If one try to eject snapshot marked as
		"becoming persistent" he will fail. Snapshots marked as "becoming
		persistent" are never reported by onCanAdvanceSnapshots.  */ 
	return 0;
}

int ShOnCheckpoint(SnapshotsOnCheckpointInfo *onCheckpointInfo,
				   int(*saveListsProc)(SnapshotsOnCheckpointInfo*,SnapshotsVersionInfo*,size_t))
{
	/*	TODO: find snapshot marked "persistent" and mark it "disintegrating"; find 
		snapshot marked as "becoming persistent" and mark it as "persistent".
		If no such snapshot fail.  */ 
	return 0;
}

int ShOnCompleteCheckpoint()
{
	return 1;
}
