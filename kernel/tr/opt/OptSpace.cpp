#include "OptSpace.h"

OptimizationSpace::OptimizationSpace()
: allocated(0)
{
    createNewRegion();
}

OptimizationSpace::~OptimizationSpace()
{
    clearOnly();
}

size_t OptimizationSpace::total() const
{
    return regions.size() * MEMORY_BLOCK_SIZE;
}

