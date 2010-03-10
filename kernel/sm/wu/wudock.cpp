#define __WUDANG_SOURCES__

#include <stdint.h>
#include "common/sm_vmm_data.h"
#include "wudock.h"
#include "wuaux.h"
#include "sm/bufmgr/bm_core.h"

XPTR WuInternaliseXptr(const xptr& v)
{
	return ((XPTR)UINTPTR_MAX+1)*v.layer + (uintptr_t)v.addr;
}

xptr WuExternaliseXptr(XPTR v)
{
	return cxptr((t_layer)(v/((XPTR)UINTPTR_MAX+1)),(void*)(uintptr_t)(v));
}

static
int LocateBlockHeader(int bufferId, vmm_sm_blk_hdr **header)
{
    int success = 0;

    assert(header); *header=NULL;
    if (bufferId<0)
    {
        WuSetLastErrorMacro(WUERR_BAD_PARAMS);
    }
    else
    {
        *header = (vmm_sm_blk_hdr*)OffsetPtr(buf_mem_addr, RamoffsFromBufferId(bufferId));
        success = 1;
    }
    return success;
}

static
int LocateVersionsHeader(int bufferId, VersionsHeader **veHeader)
{
    int success = 0;
    vmm_sm_blk_hdr *blockHeader = NULL;

    assert(veHeader); *veHeader=NULL;
    if (LocateBlockHeader(bufferId, &blockHeader))
    {
        *veHeader = &blockHeader->versionsHeader;
        success = 1;
    }
    return success;
}

void WuLogHeader(int bufferId)
{
    VersionsHeader *header;
    ramoffs rof = -1;
    
    if (!LocateVersionsHeader(bufferId, &header))
    {
        return;
    }

    wulog(("Version header for version: lxptr = %"PRI_XPTR", physXptr = %"PRI_XPTR", bufferId = %d", header->xptr[0], 
            WuInternaliseXptr((*phys_xptrs)[bufferId]), bufferId));
    
    buffer_table.find((*phys_xptrs)[bufferId], rof);
    wulog(("Buffer table for physXptr = %"PRI_XPTR" says bufferId = %d", WuInternaliseXptr((*phys_xptrs)[bufferId]), 
            BufferIdFromRamoffs(rof))); 
    
    for (unsigned int i = 0; i < VE_VERSIONS_COUNT; i++)
        wulog(("\tRecord number %u: xptr = %"PRI_XPTR", timestamp = %"PRIx64, i, header->xptr[i], header->creatorTs[i]));
}
