#include "OptSpace.h"
#include "OptTypes.h"

MemoryPool::MemoryPool()
  : allocated(0)
{
    createNewRegion();
}

MemoryPool::~MemoryPool()
{
    clearOnly();
}

size_t MemoryPool::total() const
{
    return regions.size() * MEMORY_BLOCK_SIZE;
}
