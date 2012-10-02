#ifndef _MEMORY_POOL_H_
#define _MEMORY_POOL_H_

#include "common/sedna.h"

#include <vector>
#include <stdlib.h>

struct MemoryBlock {
    size_t size;
    ::ptrdiff_t freePtr;
    char data[];
};

typedef std::vector<MemoryBlock *> MemoryRegions;

/**
 * @brief Memory pool is a "mark and release" memory allocator
 * It's mission is to allocate memory as fast as possible,
 * providing no deallocation functionality.
 *
 * Description can be found here:
 * http://www.flounder.com/memory_allocation.htm
 *
 * The idea is slightly different. "Mark" is actually
 * constructing a new allocator. And release is clear method
 *
 * @warning There is max space, that can be allocated by this pool
 */

class MemoryPool {
    MemoryRegions regions;
    std::vector<size_t> marks;

    MemoryBlock * freeRegion;
    size_t allocated;
    const size_t blockSize;

    void createNewRegion()
    {
        MemoryBlock * region = (MemoryBlock *) malloc(blockSize);
        region->freePtr = 0;
        region->size = blockSize - (region->data - (char *) region);
        regions.push_back(region);
        freeRegion = region;
    };

    void clearOnly()
    {
        allocated = 0;

        for (MemoryRegions::const_iterator it = regions.begin(); it != regions.end(); ++it) {
            free(*it);
        };

        regions.clear();
    };
public:
    /**
     * @brief Creates a MnR memory manager
     * @param n number of bytes.
     */
    explicit MemoryPool(size_t _blockSize)
      : freeRegion(NULL), allocated(0), blockSize(_blockSize)
    {
        createNewRegion();
    };

    ~MemoryPool()
    {
        clearOnly();
    };

    /**
     * @returns maximum allocatable number of bytes.
     */
    size_t maxSize() const { return blockSize; }
    
    /**
     * @brief Allocates n bytes very fast
     * @param n number of bytes.
     * @returns pointer to allocated bytes
     * @warning NO alignment is garanteed!
     */
    inline
    void * alloc(size_t n)
    {
//      NOTE: commented out due it's breaks the C++ standard. It works in some compilers, but it's wrong.
//            More details there: http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2253.html
//      U_ASSERT(blockSize > (n + sizeof(MemoryBlock::freePtr) + sizeof(MemoryBlock::size)));
        U_ASSERT(blockSize > (n + sizeof(freeRegion->freePtr) + sizeof(freeRegion->size)));

        if (freeRegion->size - freeRegion->freePtr < n) {
            createNewRegion();
        }

        void * result = freeRegion->data + freeRegion->freePtr;

        freeRegion->freePtr += n;
        allocated += n;

        return result;
    };

    /**
     * @brief Sets the mark for memory allocation
     */
    void mark()
    {
        marks.push_back(regions.size());
        createNewRegion();
    };
    
    /**
     * @brief Releases allocated memory to the last mark
     */
    void release()
    {
        while (regions.size() > marks.back())
        {
            free(regions.back());
            regions.pop_back();
        };

        marks.pop_back();
    };
    
    /**
     * @brief Releases all allocated memory
     */
    void clear()
    {
        clearOnly();
        createNewRegion();
    };

    /**
     * @returns total bytes occupied
     */
    size_t total() const { return regions.size() * blockSize; };

    /**
     * @returns total bytes allocated
     */
    size_t totalAllocated() const { return allocated; };

};


#endif /* _MEMORY_POOL_H_ */
