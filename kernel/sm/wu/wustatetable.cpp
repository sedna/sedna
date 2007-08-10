#define __WUDANG_SOURCES__

#include <stdlib.h>
#include <stddef.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include "wustatetable.h"
#include "wuaux.h"
#include "wuerr.h"

#define ST_ALIGNMENT				sizeof(void*)
#define ST_TICKET_PTR_BITS			20
#define	ST_TICKET_ORD_BITS			8
#define ST_GUARD_SIZE				sizeof(uint32_t)*2
#define ST_GUARD_FILL				UINT32_C(0xDEADBEAF)

#define ST_MAX_ROW_SIZE				(1U<<(ST_TICKET_PTR_BITS-1))
#define ST_MAX_COLUMNS				(1U<<(ST_TICKET_ORD_BITS-1))

/*	the mask (a) must be non zero (b) must have 2 high bit set so that resulting pointer
	is invalid since it points to kernel address space both on Windows and Linux */ 
#define ST_TICKET_XOR_MASK			UINT32_C(0xFFAFE0CC)

static inline 
TICKET MakeTicket(void *ptr, int ordinal)
{
	uintptr_t iv=0;
	iv=(uintptr_t)ptr & (ST_MAX_ROW_SIZE-1);
	iv|=((unsigned)ordinal & (ST_MAX_COLUMNS-1)) << ST_TICKET_PTR_BITS;
	return (TICKET)(iv^ST_TICKET_XOR_MASK);
}

static inline
void *PtrFromTicket(TICKET ticket)
{
	uintptr_t iv=0;
	iv=((uintptr_t)ticket^ST_TICKET_XOR_MASK) & (ST_MAX_ROW_SIZE-1);
	return (void*)iv;
}

static inline 
int OrdinalFromTicket(TICKET ticket)
{
	uintptr_t iv=0;
	iv=((uintptr_t)ticket^ST_TICKET_XOR_MASK)>>ST_TICKET_PTR_BITS;
	return (int)(iv&(ST_MAX_COLUMNS-1));
}

static inline
int CheckTicket(TICKET ticket)
{
	return 1;
}

static
int GetColumnInfo(StateTable *t, TICKET ticket, 
							size_t *size, size_t *preceedingUnused, size_t *followingUnused)
{
	size_t dummy;
	void *begin=NULL, *end=NULL, *colbase=NULL;
	int success=0, isBogus=0, colnum=0;

	assert(t);
	end = OffsetPtr(NULL,t->stride);
	colbase = PtrFromTicket(ticket);
	colnum = OrdinalFromTicket(ticket);

	isBogus = (size==NULL && preceedingUnused==NULL && followingUnused==NULL);
	if (!size) size = &dummy;
	if (!preceedingUnused) preceedingUnused = &dummy;
	if (!followingUnused) followingUnused = &dummy;

	if (colbase<begin || colbase>=end || !CheckTicket(ticket) || colnum<0 || colnum>=t->columnsCount ||
		DEBUGI && t->columnBase[colnum]!=colbase)
	{
		WuSetLastErrorMacro(WUERR_BAD_TICKET);
	}
	else if (isBogus)
	{
		success=1;		
	}
	else if (!DEBUGI)
	{
		WuSetLastErrorMacro(WUERR_DEBUG_FUNCTION_UNAVAILABLE);
	}
	else
	{
		*size=t->columnSize[colnum];
		*preceedingUnused=*followingUnused=ST_GUARD_SIZE;
		if (colnum>0)
		{
			*preceedingUnused = (size_t)CalcPtrDistance(t->columnBase[colnum-1],colbase)-
				t->columnSize[colnum-1];
		}
		if (colnum<t->columnsCount-1)
		{
			*followingUnused = (size_t)CalcPtrDistance(colbase,t->columnBase[colnum+1])-*size;
		}
		success=1;
	}
	return success;
}

static 
int ValidateTicket(StateTable *t, TICKET ticket)
{
	return GetColumnInfo(t,ticket,NULL,NULL,NULL);
}

#define ST_TABLE_GUARD			0
#define ST_ROW_GUARD			1
#define ST_CELL_GUARD			2

static
int WalkGuardMemory(StateTable *t, 
					int rowId, TICKET ticket, 
					int guardKind, int(*walkProc)(StateTable*,int,int,void*,ptrdiff_t))
{
	void *ptr=NULL; int acc=1;
	size_t size=0, preceedingUnused=0, followingUnused=0, ofs=0;
	int i=0;
	assert(t);
	if (DEBUGI)
	{
		switch(guardKind)
		{
		case ST_TABLE_GUARD:
			for(rowId=0; rowId<t->rowsCount && guardKind!=ST_ROW_GUARD; ++rowId)
			{
			case ST_ROW_GUARD:
				assert(rowId>=0 && rowId<t->rowsCount);
				preceedingUnused=ST_GUARD_SIZE;
				ptr=OffsetPtr(t->base,t->stride*rowId);
				for (i=0;i<t->columnsCount-1;++i)
				{					
					acc&=walkProc(t,rowId,i,ptr,-(ptrdiff_t)preceedingUnused);
					ofs=(size_t)CalcPtrDistance(t->columnBase[i],t->columnBase[i+1]);
					preceedingUnused=ofs-t->columnSize[i];
					ptr=OffsetPtr(ptr,ofs);
				}
				acc&=walkProc(t,rowId,i,ptr,-(ptrdiff_t)preceedingUnused);
				acc&=walkProc(t,rowId,i+1,OffsetPtr(ptr,t->columnSize[i]),ST_GUARD_SIZE);
			}
			break;
		case ST_CELL_GUARD:
			assert(rowId>=0 && rowId<t->rowsCount);
			ptr=OffsetPtr(t->base,t->stride*rowId+CalcPtrDistance(NULL,PtrFromTicket(ticket)));
			i=OrdinalFromTicket(ticket);
			GetColumnInfo(t,ticket,&size,&preceedingUnused,&followingUnused);
			acc&=walkProc(t,rowId,i,ptr,-(ptrdiff_t)preceedingUnused);
			acc&=walkProc(t,rowId,i+1,OffsetPtr(ptr,size),followingUnused);
			break;
		default:
			assert(0);
		}
	}
	return acc;
}

static
int InitGuardMemoryProc(StateTable* t, int rowId, int colId, void *ptr, ptrdiff_t dist)
{
	DbgInitGuardMemory(ptr,dist,ST_GUARD_FILL);
	return 1;
}

static
void InitGuardMemory(StateTable *t, int rowId, TICKET ticket, int guardKind)
{
	WalkGuardMemory(t,rowId,ticket,guardKind,InitGuardMemoryProc);
}

static
int ValidateGuardMemoryProc(StateTable* t, int rowId, int colId, void *ptr, ptrdiff_t dist)
{
	assert(t);
	int isPassed; char buf[128]="", buf1[16]="?", buf2[16]="?"; 
	isPassed=DbgCheckGuardMemory(ptr,dist,ST_GUARD_FILL,0,NULL);
	if (!isPassed)
	{
		if (colId>0) sprintf(buf1,"%d",colId-1);
		if (colId<t->columnsCount) sprintf(buf2,"%d",colId);
		sprintf(buf,"StateTable %08p row %d, columns %s-%s", t, rowId, buf1, buf2);
		DbgCheckGuardMemory(ptr,dist,ST_GUARD_FILL,1,buf);
	}
	return isPassed;
}

static
void ValidateGuardMemory(StateTable *t, int rowId, TICKET ticket, int guardKind)
{
	int isGuardMemoryValidationPassed;
	isGuardMemoryValidationPassed=WalkGuardMemory(t,rowId,ticket,guardKind,ValidateGuardMemoryProc);
	assert(isGuardMemoryValidationPassed); /* verbose assert message */ 
}

void InitializeStateTable(StateTable *t)
{
	assert(t);
	t->mem=NULL;
	ResetStateTable(t);
}

int CreateStateTableRows(StateTable *t, int rowsCount, size_t maxRowSize)
{
	size_t bitmapSize=0, headerSize=0, rowSize=0, totalSize=0;
	int success=0;
	assert(t && rowsCount>0);

	bitmapSize = sizeof(uint32_t)*RoundSizeUp(rowsCount,32)/32;
	rowSize = RoundSizeUp(t->rowSize,ST_ALIGNMENT);
	headerSize = RoundSizeUp(bitmapSize,ST_ALIGNMENT);
	if (DEBUGI) rowSize+=ST_GUARD_SIZE;
	totalSize = headerSize+rowSize*rowsCount+ST_ALIGNMENT;

	if(rowSize>maxRowSize)
	{
		WuSetLastErrorMacro(WUERR_STATE_TABLE_MAX_ROW_SIZE_EXCEEDED);
	}
	else
	{
		t->mem=malloc(totalSize);
		if (NULL==t->mem)
		{
			WuSetLastErrorMacro(WUERR_NO_MEMORY);
		}
		else
		{
			t->rowsCount=rowsCount;
			t->stride=(ptrdiff_t)rowSize;
			t->base=AlignPtr(OffsetPtr(t->mem,headerSize+(DEBUGI!=0)*ST_GUARD_SIZE),ST_ALIGNMENT);
			memset(t->mem,0,totalSize);
			memset(t->mem,0xFF,headerSize);
			InitGuardMemory(t,0,NULL,ST_TABLE_GUARD);
			success=1;
		}		
	}
	return success;
}

void ResetStateTable(StateTable *t)
{
	int i=0;

	assert(t);
	if (t->mem) ValidateGuardMemory(t,0,NULL,ST_TABLE_GUARD);
	free(t->mem);

	t->rowsCount=0;
	t->columnsCount=0;
	t->rowSize=0;
	t->mem=NULL;
	t->base=NULL;
	t->stride=0;
	for (i=0;i<ST_MAX_COLUMNS_WITH_INFO;++i) 
	{
		t->columnSize[i]=0;
		t->columnBase[i]=NULL;
	}
}

void DeinitializeStateTable(StateTable *t)
{
	ResetStateTable(t);
}

int  ReserveStateTableColumn(StateTable *t, TICKET *ticket, size_t size, int alignment)
{
	void *p=NULL; 
	int colnum=t->columnsCount; 
	size_t nsize=0;
	int success=0;

	assert(t && ticket && alignment>=0 && Is2Power(alignment));
	if (alignment==0 || alignment>(int)ST_ALIGNMENT) alignment=ST_ALIGNMENT;
	*ticket=NULL;

	p=AlignPtr(OffsetPtr(NULL,t->rowSize),alignment);
	nsize=(size_t)CalcPtrDistance(NULL,p)+size;
	if (DEBUGI) nsize+=ST_GUARD_SIZE;

	if (colnum>=(int)ST_MAX_COLUMNS)
	{
		WuSetLastErrorMacro(WUERR_STATE_TABLE_MAX_NUMBER_OF_COLUMNS_EXCEEDED);
	}
	else if (nsize>=ST_MAX_ROW_SIZE)
	{
		WuSetLastErrorMacro(WUERR_STATE_TABLE_MAX_ROW_SIZE_EXCEEDED);
	}
	else if (DEBUGI && colnum>=ST_MAX_COLUMNS_WITH_INFO)
	{
		WuSetLastErrorMacro(WUERR_STATE_TABLE_MAX_NUMBER_OF_COLUMNS_WITH_DEBUG_INFO_EXCEEDED);
	}
	else
	{
		++t->columnsCount;
		t->rowSize=nsize;
		if (DEBUGI)
		{
			t->columnBase[colnum]=p;
			t->columnSize[colnum]=size;	
		}
		*ticket=MakeTicket(p,colnum);
		success=1;
	}	
	return success;
}

int  GetStateTableCell(StateTable *t, void **dest, TICKET ticket, int rowId)
{
	int success=0;
	assert(t && dest);
	*dest=NULL;

	if (!ValidateTicket(t,ticket))
	{
	}
	else if (!IsValidStateTableRowId(t,rowId))
	{
	}
	else
	{
		ValidateGuardMemory(t,rowId,ticket,ST_CELL_GUARD);
		*dest=OffsetPtr(t->base,t->stride*rowId+CalcPtrDistance(NULL,PtrFromTicket(ticket)));
		success=1;
	}
	return success;
}

int  GetStateTableMasterCell(StateTable *t, void **dest, int rowId)
{
	return GetStateTableCell(t,dest,MakeTicket(NULL,0),rowId);
}

int  OccupyStateTableFirstVacantRow(StateTable *t, int *rowId, int rngBegin, int rngEnd)
{
	int success=0, bitId=0;
	uint32_t *i=NULL, *e=NULL, m0=~UINT32_C(0), m1=~UINT32_C(0), v=0;

	assert(t && rowId);
	*rowId=-2;
	if (rngBegin<0) rngBegin=0;
	if (rngEnd>t->rowsCount) rngEnd=t->rowsCount;
	m0<<=rngBegin&31;
	m1>>=(32-rngEnd&31);
	i=(uint32_t*)t->mem+rngBegin/32;
	e=(uint32_t*)t->mem+(rngEnd+31)/32;

	if (rngBegin>rngEnd)
	{
		;
	}
	if (i+1==e)
	{
		v=i[0]&m0&m1;
	}
	else if (i+1<e)
	{
		v=i[0]&m0;
		while (v==0 && i+1<e)
		{
			++i;
			v=i[0];
		}
		if (v==0) v=(++i)[0]&m1;
	}

	if (v==0)
	{
		WuSetLastErrorMacro(WUERR_STATE_TABLE_FULL);
	}
	else
	{
		bitId=FindLowestBitSet(v);
		*i&=~(v&-v);
		*rowId=bitId+(i-(uint32_t*)t->mem)*32;
		ValidateGuardMemory(t,*rowId,NULL,ST_ROW_GUARD);
		success=1;
	}
	return success;
}

int  SetStateTableIsVacantRowFlag(StateTable *t, int rowId, int isVacant)
{
	uint32_t *p=NULL, m=1;
	int success=0;

	assert(t);
	if (rowId<0 || rowId>=t->rowsCount)
	{
		WuSetLastErrorMacro(WUERR_STATE_TABLE_BAD_ROW_ID);
	}
	else
	{
		ValidateGuardMemory(t,rowId,NULL,ST_ROW_GUARD);
		m<<=rowId&31;
		p=(uint32_t*)t->mem+rowId/32;
		if (isVacant)
		{
			memset(OffsetPtr(t->base,t->stride*rowId),0,t->rowSize);
			InitGuardMemory(t,rowId,NULL,ST_ROW_GUARD);
			*p|=m;
		}
		else
		{
			*p&=~m;
		}
		success=1;
	}
	return success;
}

int  IsStateTableRowVacant(StateTable *t, int *isVacant, int rowId)
{
	uint32_t *p=NULL, m=1;
	int success=0;

	assert(t&&isVacant);
	*isVacant=2;

	if (rowId<0 || rowId>=t->rowsCount)
	{
		WuSetLastErrorMacro(WUERR_STATE_TABLE_BAD_ROW_ID);
	}
	else
	{
		ValidateGuardMemory(t,rowId,NULL,ST_ROW_GUARD);
		success=1;
		m<<=rowId&31;
		p=(uint32_t*)t->mem+rowId/32;
		*isVacant=((p[0]&m)!=0);
	}
	return success;
}

void DbgDumpStateTableParams(StateTable *t, DbgDumpMemoryParams *params)
{
	assert(t && params);
	memset(params,0,sizeof *params);
	params->base = t->base;
	params->size = (size_t)(t->stride*t->rowsCount);
	params->stride = t->stride;
	params->skipBits = (uint32_t*)t->mem;
	params->sectionsBase = NULL;
	params->sectionsCount = t->columnsCount;
	if (DEBUGI)
	{
		params->sections = t->columnBase;
		params->sectionsSize = t->columnSize;
	}
}

void DbgDumpStateTable(StateTable *t)
{
	DbgDumpMemoryParams params;
	assert(t);
	DbgDumpStateTableParams(t,&params);
	DbgDumpMemory(&params);
	ValidateGuardMemory(t,0,NULL,ST_TABLE_GUARD);
}

int  IsValidStateTableRowId(StateTable *t, int rowId)
{
	int success=0;
	assert(t);
	if (rowId<0 || rowId>=t->rowsCount || ((uint32_t*)t->mem)[rowId/32]&(UINT32_C(1)<<rowId&31))
	{
		WuSetLastErrorMacro(WUERR_STATE_TABLE_BAD_ROW_ID);
	}
	else
	{
		success=1;
	}
	return success;
}
