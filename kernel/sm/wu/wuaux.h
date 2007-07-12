#ifdef _MSC_VER
#pragma once
#endif

#ifndef WUAUX_INCLUDED
#define WUAUX_INCLUDED

#include <assert.h>

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
	assert(roundFactor>0);
	assert(roundFactor-1&roundFactor==0); /* roundFactor MUST be 2 power */ 
	return (size+roundFactor-1)&(roundFactor-1);
}

static
inline
void *AlignPtr(void *ptr, int alignment)
{
	assert(alignment>0);
	assert(alignment-1&alignment==0); /* alignment MUST be 2 power */ 
	return (void*)((uintptr_t)ptr+alignment-1&alignment-1);
}

void DbgInitGuardMemory(void *ptr, ptrdiff_t otherSide, uint32_t fill);
void DbgValidateGuardMemory(void *ptr, ptrdiff_t otherSide, uint32_t fill);

#endif
