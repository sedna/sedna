#include <assert.h>

#define	VE_VERSIONS_COUNT	4
#define VE_SNAPSHOTS_COUNT	3

struct VersionsEncodedHeader
{
	TIMESTAMP creatorTs[VE_VERSIONS_COUNT];
	XPTR blocks[VE_VERSIONS_COUNT-1];
	int creators[VE_VERSIONS_COUNT];
	int isZomby;
};

struct VersionsRecord
{
	XPTR xptr;
	TIMESTAMP creatorTs;
	int creator;
};

struct VersionsHeader
{
	VersionsRecord records[VE_SNAPSHOTS_COUNT+2];
	int validDataBegin, validDataEnd;
	int isZomby;
};

struct VersionsSnapshot
{
	TIMESTAMP timestamp;	
	VersionsSnapshot *next;
	size_t clientsCount;
	TIMESTAMP clientsTs[1];
};

/* global state */ 

static TIMESTAMP persSnapshotTs=0;
static VersionsSnapshot *snapshots=0, *leadingSnapshot=0; 

/* utility functions */ 

static
int ValidateSnapshots(VersionsSnapshot *head)
{
	return 1;
}

static
int ValidateHeader(VersionsHeader *hdr)
{
	return 1;
}

inline
static
int IsAbove(VersionsRecord *rec, VersionsSnapshot *sh)
{
	assert(rec && sh);
	assert(rec->creator>=0 && rec->creator<sh->clientsCount);
	return rec->creatorTs>sh->timestamp || rec->creatorTs==sh->clientsTs[rec->creator];
}

static
void Decode(VersionsHeader *hdr, VersionsEncodedHeader *ehdr, XPTR xptr, VersionsSnapshot *head)
{
	static const VersionsRecord initRec00 = {0,0,-1}, initRecFF = {0,~(TIMESTAMP)0,-1};
	VersionsRecord tempRec[VE_VERSIONS_COUNT+1], *i=NULL, *ie=NULL, *o=NULL, *oe=NULL;
	VersionsSnapshot *it=NULL, *shCol[VE_SNAPSHOTS_COUNT+1];
	int j=0;

	assert(hdr && ehdr && head);
	assert(ValidateSnapshots(head));

	/* unpack ehdr */ 
	tempRec[0]=initRecFF;
	tempRec[1].xptr=xptr;
	tempRec[1].creatorTs=ehdr->creatorTs[VE_VERSIONS_COUNT-1];
	tempRec[1].creator=ehdr->creators[VE_VERSIONS_COUNT-1];
	for(j=2;i<VE_VERSIONS_COUNT+1;++j)
	{
		tempRec[j].xptr=ehdr->blocks[j-2];
		tempRec[j].creatorTs=ehdr->creatorTs[j-2];
		tempRec[j].creator=ehdr->creators[j-2];
		if (tempRec[j].creatorTs==~(TIMESTAMP)0) break;
	}
	i=tempRec;
	ie=tempRec+j;
	assert(i<ie);
	/* now store snapshots pointers in a single array */ 
	for (j=0,it=head;j<VE_SNAPSHOTS_COUNT+1;++j,it=it->next)
	{
		assert(it);
		shCol[j]=it;
	}
	assert(it==NULL);
	/* initialise records in hdr*/ 
	for (j=0;j<VE_SNAPSHOTS_COUNT+2;++j)
	{
		hdr->records[j].xptr=0;
		hdr->records[j].creatorTs=0;
		hdr->records[j].creator=-1;
	}
	/* find bottom */ 
	for (j=VE_VERSIONS_COUNT;j>0;--j)
	{
		if (IsBelow
	}
	
	o=hdr->records; oe=hdr->records+VE_SNAPSHOTS_COUNT+2;
	it=head;
	hdr->validDataBegin = (IsAbove(i,head)?0:1);
	while(i<ie && o<oe && it)
	{
		if (IsAbove(i,it))
		{
			/* we're above the snapshot by timeline */ 
			o[0]=o[1]=*i;
			do ++i; while (i<ie && IsAbove(i,it));
		}
		++o;
		it=it->next;
	}
	
	hdr->validDataEnd = (o-hdr->records);
	hdr->isZomby = ehdr->isZomby;
	assert (ValidateHeader(hdr));
}

static
void Encode(VersionsEncodedHeader *hdr, XPTR *xptr, VersionsHeader *hdr, VersionsSnapshot *head)
{
	;
}
