#ifdef _MSC_VER
#pragma once
#endif

#ifndef WUAUX_INCLUDED
#define WUAUX_INCLUDED

#define __STDC_CONSTANT_MACROS
#include <stdint.h>
#include <stddef.h>
#include <assert.h>
#include "wuerr.h"

static
inline
int Is2Power(int val)
{
	return (val&val-1)==0;
}

static
inline
void *OffsetPtr(void *ptr, ptrdiff_t distance)
{
	return (char*)ptr+distance;
}

static
inline
ptrdiff_t CalcPtrDistance(void *ptr1, void *ptr2)
{
	return (char*)ptr2-(char*)ptr1;
}

static 
inline 
size_t RoundSizeUp(size_t size, int roundFactor)
{
	assert(roundFactor>0 && Is2Power(roundFactor));
	return ((size-1)|(size_t)(roundFactor-1))+1;
}

static
inline
void *AlignPtr(void *ptr, int alignment)
{
	assert(alignment>0 && Is2Power(alignment));
	return (void*)((uintptr_t)ptr+alignment-1&~(uintptr_t)(alignment-1));
}

void DbgInitGuardMemory(void *ptr, ptrdiff_t dist, uint32_t fill);

int DbgCheckGuardMemory(void *ptr, ptrdiff_t dist, uint32_t fill, 
						int isReportingEnabled, 
						const char *errorMessageHeader);

struct DbgDumpMemoryMarks
{
	char mark;
	uint32_t *markBits, xorMask;	
	DbgDumpMemoryMarks *next;
};

struct DbgDumpMemoryParams
{
	int flags;
	void *base;
	size_t size;
	ptrdiff_t stride;
	uint32_t *skipBits;
	void *sectionsBase;
	int sectionsCount;
	void **sections;
	size_t *sectionsSize;
	DbgDumpMemoryMarks *marks;
};

void DbgDumpMemory(DbgDumpMemoryParams *dumpMemoryParams);

/*	http://graphics.stanford.edu/~seander/bithacks.html#ZerosOnRightMultLookup	
	if val==0 returns 0 incorrectly	*/ 
static
inline
int FindLowestBitSet(uint32_t val)
{
	static const int tab[32] = 
	{
		0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8, 
		31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
	};
	int bitId=0;

	assert(val);
	bitId=tab[(val&-val)*UINT32_C(0x077CB531)>>27];

	return bitId;
}

static
inline
int ResetLowestBitSet(uint32_t *val)
{
	int bitId=FindLowestBitSet(*val);
	*val^=*val&-*val;
	return bitId;
}

#define ERROR(CODE) ERRORfn(__FILE__,__LINE__,__FUNCTION__,(int)CODE);
#define ISERROR(CODE) ISERRORfn(CODE)

void ERRORfn(const char*,int,const char*,int);
int  ISERRORfn(int);

#endif
