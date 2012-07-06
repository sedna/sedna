#ifndef _OPT_SPACE_H_
#define _OPT_SPACE_H_

#include "common/sedna.h"
#include "tr/cat/catmem.h"

#include <set>
#include <stdlib.h>

#define MEMORY_BLOCK_SIZE (0x100000)

struct MemoryBlock {
    size_t size;
    ::ptrdiff_t freePtr;
    char data[];
};

typedef std::set<MemoryBlock *> MemoryRegionMap;

class MemoryPool {
    MemoryRegionMap regions;
    MemoryBlock * freeRegion;
    size_t allocated;

    void createNewRegion()
    {
        MemoryBlock * region = (MemoryBlock *) malloc(MEMORY_BLOCK_SIZE);
        region->freePtr = 0;
        region->size = MEMORY_BLOCK_SIZE - (region->data - (char *) region);
        regions.insert(region);
        freeRegion = region;
    };

    void clearOnly()
    {
        allocated = 0;

        for (MemoryRegionMap::const_iterator it = regions.begin(); it != regions.end(); ++it) {
            free(*it);
        };

        regions.clear();
    };
public:
    MemoryPool();
    ~MemoryPool();

    void * alloc(size_t n)
    {
        U_ASSERT(MEMORY_BLOCK_SIZE > (n + sizeof(MemoryBlock::freePtr) + sizeof(MemoryBlock::size)));

        if (freeRegion->size - freeRegion->freePtr < n) {
            createNewRegion();
        }

        void * result = freeRegion->data + freeRegion->freePtr;

        freeRegion->freePtr += n;
        allocated += n;

        return result;
    };

    void clear()
    {
        clearOnly();
        createNewRegion();
    };

    size_t total() const ;
    size_t totalAllocated() const { return allocated; };

};

#endif /* _OPT_SPACE_H_ */
