#if (_MSC_VER > 1000)
#pragma once
#endif

#ifndef WUSNAPSHOTSCS_H_INCLUDED
#define WUSNAPSHOTSCS_H_INCLUDED

#include "wutypes.h"

/* garbage entries have lxptr==0 */ 
struct SnVersionEntry
{
	LXPTR lxptr;
	XPTR xptr;
};

struct SnEnumerateVersionsParams
{
	TIMESTAMP persSnapshotTs;
	size_t persVersionsCount;
	size_t garbageVersionsCount;
	size_t persVersionsSent;
	size_t garbageVersionsSent;
	void *userData;
};

struct SnSnapshotStats
{
	TIMESTAMP curSnapshotTs;
	TIMESTAMP persSnapshotTs;
	size_t versionsCount;
	size_t runawayVersionsCount;
	size_t curSnapshotVersionsCount;
	size_t curSnapshotSharedVersionsCount;
	size_t persSnapshotVersionsCount;
	size_t persSnapshotSharedVersionsCount;
};

typedef int (*SnEnumerateVersionsProc)(SnEnumerateVersionsParams *params, 
									   SnVersionEntry *buf, size_t count, int isGarbage);

#endif
