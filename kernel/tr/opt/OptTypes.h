#ifndef OPTTYPES_H
#define OPTTYPES_H

#include <vector>
#include <set>
#include <map>

#include "common/sedna.h"

#include <cstddef>

#include "tr/opt/OptSpace.h"
#include "tr/opt/types/Range.h"
#include "tr/opt/types/Comparison.h"
#include "tr/opt/types/IntBitIterator.h"
#include "tr/executor/base/tuple.h"

class DataRoot;
class XmlConstructor;

namespace pe {
class Step;
class Path;
};

namespace phop {
class ITupleOperator;
struct FunctionSignature;
}

#define MAX_GRAPH_SIZE 63

namespace opt {

typedef uint64_t PlanDesc;
typedef std::set<PlanDesc> PlanDescSet;
typedef int TupleId;

static const opt::TupleId nullTuple = 0;
static const opt::TupleId invalidTupleId = -1;
static const opt::TupleId worldDataTupleId = 1;

typedef ::IntBitIterator<PlanDesc> PlanDescIterator;

#define CDGQNAME(N) xsd::QName::getConstantQName(NULL_XMLNS, N)

struct Predicate;
struct DataNode;
struct Variable;
struct DataGraph;
struct DataGraphMaster;

class PhysicalModel;

typedef std::vector<Predicate *> PredicateList;
typedef std::vector<DataNode *> DataNodeList;
typedef std::vector<DataGraph *> DataGraphList;
typedef std::set<DataGraph *> DataGraphSet;

typedef std::multimap<TupleId, DataNode *> VariableMap;
typedef std::map<std::string, DataNode *> VariableNameMap;

typedef std::set<Predicate *> PredicateSet;
typedef std::set<TupleId> TupleScheme;
typedef std::map<TupleId, TupleId> TupleMapping;

typedef std::vector<tuple_cell> MemoryTupleSequence;
typedef counted_ptr< std::vector<tuple_cell> > MemoryTupleSequencePtr;

static inline
TupleScheme singleTupleScheme(opt::TupleId tid)
{
    TupleScheme a;
    a.insert(tid);
    return a;
}

extern ::OptimizationSpace * currentOptimizationSpace;
extern ::OptimizationSpace * optObjects;

class IPlanDisposable {
public:
    virtual ~IPlanDisposable() {};
    void * operator new(size_t n)
    {
        return currentOptimizationSpace->createObject(n);
    };
    
    void operator delete(void *) { return; };
};

inline static
PlanDesc singlePlanDesc(uint8_t item) { return 1ULL << item; };

}

#endif /* OPTTYPES_H */
