#include <stdlib.h>
#include <assert.h>
#include "wustatetable.h"
#include "wuaux.h"

#ifdef DEBUG
#define ST_DEBUGI					1
#else
#define ST_DEBUGI					0
#endif

#define ST_ALIGNMENT				sizeof(void*)
#define ST_PTR_FROM_TICKET(PTR)		PTR
#define ST_TICKET_FROM_PTR(PTR)		PTR
#define ST_GUARD_FILL				UINT32_C(0x1DECADE1)


static
int GetStateTableColumnInfo(StateTable *t, TICKET ticket, 
							size_t *size, size_t *preceedingUnused, size_t *followingUnused)
{
	size_t dummy;
	int status=0;

	if (!size) size = &dummy;
	if (!preceedingUnused) preceedingUnused = &dummy;
	if (!followingUnused) followingUnused = &dummy;

	if (!ST_DEBUGI)
	{
		; /* ERROR: "feature present in debug build only" */ 
	}
	else
	{
		status=1;
	}
	return status;
}

static 
int ValidateStateTableTicket(StateTable *t, TICKET ticket)
{
	int status=0;
	void *begin=NULL, *end=NULL, *tickptr=NULL;
	ptrdiff_t tickofs=0;
	
	assert(t);
	begin = ST_PTR_FROM_TICKET(t->firstTicket);
	end = ST_PTR_FROM_TICKET(t->nextTicket);
	tickptr = ST_PTR_FROM_TICKET(ticket);
	tickofs = CalcPtrDistance(begin,tickptr);

	if (tickofs<0 || tickptr>=end || AlignPtr(tickptr)!=tickptr)
	{
		; /* ERROR: "bad ticket" */ 
	}
	else
	{
		status=(0==ST_DEBUGI | GetStateTableColumnInfo(t, ticket, NULL, NULL, NULL));
	}
	return status;
}

static
void InitStateTableGuardMemory(StateTable *t)
{
	assert(t);
	if (ST_DEBUGI)
	{
		;
	}
}

#define ST_VALIDATE_TABLE			0
#define ST_VALIDATE_ROW				1
#define ST_VALIDATE_CELL			2

static
void ValidateStateTableGuardMemory(StateTable *t, int rowId, TICKET ticket, int validateMethod)
{
	assert(t);
	if (ST_DEBUGI)
	{
		;
	}
}

void InitStateTable(StateTable *t, int rowsCount, size_t maxRowSize)
{
	int i=0;
	assert(t && rowsCount>0);

	t->rowsCount = rowsCount;
	t->columnsCount = 0;
	t->maxRowSize = maxRowSize;
	t->mem = NULL;
	t->base = NULL;
	t->stride = NULL;
	t->firstTicket = ST_TICKET_FROM_PTR(NULL);
	t->nextTicket = ST_TICKET_FROM_PTR(NULL);
	for (i=0;i<MAX_CHECKED_COLUMNS;++i) t->columnSize[i] = 0;
}

int CreateStateTableRows(StateTable *t)
{
	size_t bitmapSize=0, headerSize=0, rowSize=0, totalSize=0;
	int status=0;
	assert(t);

	bitmapSize = (sizeof(uint32_t)*RoundSizeUp(t->rowsCount,32))>>5;
	rowSize = CalcPtrDistance(ST_PTR_FROM_TICKET(t->firstTicket),ST_PTR_FROM_TICKET(t->nextTicket));
	headerSize = RoundSizeUp(bitmapSize,ST_ALIGNMENT)+ST_ALIGNMENT;
	totalSize = headerSize+rowSize*t->rowsCount+ST_ALIGNMENT;

	if(rowSize>t->maxRowSize)
	{
		; /* ERROR: "user size limit hit" */ 
	}
	else
	{
		t->mem=malloc(totalSize);
		if (NULL==t->mem)
		{
			; /* ERROR: "out of memory" */ 
		}
		else
		{
			t->base=AlignPtr(OffsetPtr(t->mem,headerSize),ST_ALIGNMENT);
			t->stride=rowSize;
			memset(t->mem,0,totalSize);
			InitStateTableGuardMemory(t);
			status=1;
		}		
	}
	return status;
}

void ResetStateTable(StateTable *t)
{
	int rowsCount=0;
	size_t maxRowSize=0;

	assert(t);
	rowsCount=t->rowsCount();
	maxRowSize=t->maxRowSize();
	if (t->mem) ValidateStateTableGuardMemory(t,0,NULL,ST_VALIDATE_TABLE);
	free(t->mem);
	InitStateTable(t,rowsCount,maxRowSize);
}

void DeinitStateTable(StateTable *t)
{
	ResetStateTable(t);
}

