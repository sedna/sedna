#if (_MSC_VER > 1000)
#pragma once
#endif

#ifndef WUDOCK_INCLUDED
#define WUDOCK_INCLUDED

//#define WU_DEBUG_LOGGING // turns on extensive logging on wu in custom file

#include "wutypes.h"
#include "wuerr.h"
#include "common/xptr.h"
#include "common/errdbg/exceptions.h"

#include <assert.h>

XPTR WuInternaliseXptr(const xptr& v);
xptr WuExternaliseXptr(XPTR v);
void WuSetLastExceptionObject(const SednaException &e);
void WuThrowException();

void WuLogHeader(int bufferId);

#ifdef WU_DEBUG_LOGGING
#define wulog(message) elog(EL_DBG, message)
#define wulogheader(bufferId) WuLogHeader(bufferId)
#else
#define wulog(message)
#define wulogheader(bufferId)
#endif /* WU_DEBUG_LOGGING */

#define PRI_XPTR PRIx64

inline int BufferIdFromRamoffs(ramoffs ofs)
{
    assert(ofs % PAGE_SIZE == 0);
    return ofs / PAGE_SIZE;
}

inline ramoffs RamoffsFromBufferId(int id)
{
    return id * PAGE_SIZE;
}

#define WU_CATCH_EXCEPTIONS() \
	catch (SednaException &e) \
	{ \
		WuSetLastExceptionObject(e); \
	} \
	catch (ANY_SE_EXCEPTION) \
	{ \
		WuSetLastErrorMacro(WUERR_UNKNOWN_EXCEPTION); \
	}

#endif
