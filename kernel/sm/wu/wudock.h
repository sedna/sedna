#if (_MSC_VER > 1000)
#pragma once
#endif

#ifndef WUDOCK_INCLUDED
#define WUDOCK_INCLUDED

#include "wutypes.h"
#include "common/xptr.h" 

XPTR WuInternaliseXptr(const xptr& v);
xptr WuExternaliseXptr(XPTR v);

#endif
