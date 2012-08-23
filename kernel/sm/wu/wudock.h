#if (_MSC_VER > 1000)
#pragma once
#endif

#ifndef WUDOCK_INCLUDED
#define WUDOCK_INCLUDED

//#define WU_DEBUG_LOGGING // turns on extensive logging on wu in custom file

#include "wutypes.h"
#include "wuerr.h"
#include "common/xptr/xptr.h"
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
    /* explicit cast is safe here since bufs number is int everywhere on se_sm */
    return ofs == RAMOFFS_OUT_OFF_BOUNDS ? -1 : (int)(ofs / PAGE_SIZE);
}

inline ramoffs RamoffsFromBufferId(int id)
{
    return id == -1 ? RAMOFFS_OUT_OFF_BOUNDS : (ramoffs)id * PAGE_SIZE;
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
