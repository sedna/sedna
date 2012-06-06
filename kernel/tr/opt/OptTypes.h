#ifndef OPTTYPES_H
#define OPTTYPES_H

#include <vector>
#include <set>
#include <map>

#include "common/sedna.h"

#include <cstddef>

#include "tr/opt/types/Range.h"
#include "tr/opt/types/IntBitIterator.h"

namespace pe {
    class Path;
};

#define MAX_GRAPH_SIZE 63

namespace opt {

typedef uint64_t PlanDesc;
typedef std::set<PlanDesc> PlanDescSet;
typedef int TupleId;

typedef ::IntBitIterator<PlanDesc> PlanDescIterator;

#define CDGQNAME(N) xsd::QName::getConstantQName(NULL_XMLNS, N)

struct Predicate;
struct DataNode;
struct Variable;
struct DataGraph;

class PhysicalModel;

typedef std::vector<Predicate *> PredicateList;
typedef std::vector<DataNode *> DataNodeList;
typedef std::vector<DataGraph *> DataGraphList;

typedef std::multimap<TupleId, DataNode *> VariableMap;
typedef std::map<std::string, DataNode *> VariableNameMap;

typedef std::set<Predicate *> PredicateSet;
typedef std::set<TupleId> TupleScheme;
typedef std::map<TupleId, TupleId> TupleMapping;


static inline
TupleScheme singleTupleScheme(opt::TupleId tid)
{
    TupleScheme a;
    a.insert(tid);
    return a;
}

#define MEMORY_BLOCK_SIZE (0x100000)

struct MemoryBlock {
    size_t size;
    ::ptrdiff_t freePtr;
    char data[];
};

typedef std::set<MemoryBlock *> MemoryRegionMap;

class OptimizationSpace {
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
    OptimizationSpace();
    ~OptimizationSpace();
  
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

extern OptimizationSpace * currentOptimizationSpace;

class IPlanDisposable {
public:
    virtual ~IPlanDisposable() {};
    void * operator new(size_t n) { return currentOptimizationSpace->alloc(n); };
    void operator delete(void *) { return; };
};

inline static
PlanDesc singlePlanDesc(uint8_t item) { return 1ULL << item; };

}

#endif /* OPTTYPES_H */
