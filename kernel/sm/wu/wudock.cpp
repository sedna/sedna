#define __WUDANG_SOURCES__

#define __STDC_LIMIT_MACROS
#include <stdint.h>
#include "wudock.h"

XPTR WuInternaliseXptr(const xptr& v)
{
	return ((XPTR)UINTPTR_MAX+1)*v.layer + (uintptr_t)v.addr;
}

xptr WuExternaliseXptr(XPTR v)
{
	return xptr((t_layer)(v/((XPTR)UINTPTR_MAX+1)),(void*)(uintptr_t)(v));
}
