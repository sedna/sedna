#include <stdint.h>

#ifdef _MSC_VER
#pragma once
#endif

#ifndef WUTYPES_INCLUDED
#define WUTYPES_INCLUDED

typedef uint64_t XPTR, LXPTR;
typedef uint64_t TIMESTAMP;
typedef void *TICKET;


/* [TIMESTAMP_MIN, TIMESTAMP_MAX] is the range of valid timestamps; values outside this
   range have the special interpretation. One of the special timestamp values is
   INVALID_TIMESTAMP. Other "special" values may have a module-specific meaning
   or may be used internally for some purposes. */ 
#define TIMESTAMP_MIN		UINT64_C(0x0000000000010000)
#define TIMESTAMP_MAX		UINT64_C(0xFFFFFFFFFFFF0000)

inline
static
bool IsValidTimestamp(TIMESTAMP ts)
{
	return ts>=TIMESTAMP_MIN && ts<=TIMESTAMP_MAX;
}

/* INVALID_TIMESTAMP is the greatest possible value for the variable of TIMESTAMP type */ 
#define INVALID_TIMESTAMP	UINT64_C(0xFFFFFFFFFFFFFFFF)

#define INVALID_TICKET		NULL

#endif
