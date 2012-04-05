#if (_MSC_VER > 1000)
#pragma once
#endif

#ifndef WUSTRUCTURES_INCLUDED
#define WUSTRUCTURES_INCLUDED

#include "wutypes.h"

#define	VE_VERSIONS_COUNT		4

struct VersionsHeader
{
	XPTR xptr[VE_VERSIONS_COUNT];
	TIMESTAMP creatorTs[VE_VERSIONS_COUNT];
};

#endif
