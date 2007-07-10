#ifdef _MSC_VER
#pragma once
#endif

#ifndef WUBUFFERS_INCLUDED
#define WUBUFFERS_INCLUDED

#include "wutypes.h"

struct BufferInfo
{
	XPTR xptr;		/* xptr of the buffer begin */ 
	void *ptr;		/* pointer to buffer begin in current address space */ 
	void *base;		/* the base address of filemapping, to calculate offsets */ 
	size_t size;	/* size of buffer */ 
	int  protection;/* */ 

	int  isDirty:1;	/* indicates that buffer is out of sync with disc */ 
	int  isFlushed:1; /* indicates that buffer was ever stored on disk or loaded from it */ 
};

#endif
