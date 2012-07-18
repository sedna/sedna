#ifndef OPTTYPES_H
#define OPTTYPES_H

#include <vector>
#include <set>
#include <map>

#include "common/sedna.h"

#include <cstddef>

#include "tr/opt/OptimizingExecutor.h"
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
struct FunctionInfo;
}

#define MAX_GRAPH_SIZE 63

template <typename T>
class object_vector : public std::vector<T *>
{
typedef std::vector<T *> base_t;
public:
    ~object_vector() {
        for (base_t::const_iterator it = base_t::begin(); it != base_t::end(); ++it) {
            delete *it;
        };
    };
};

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

template <typename T>
struct opt_allocator
{
    typedef T value_type;
    typedef value_type* pointer;
    typedef const value_type* const_pointer;
    typedef value_type& reference;
    typedef const value_type& const_reference;
    typedef size_t size_type;

    template<typename U>
    struct rebind {
        typedef opt_allocator<U> other;
    };

    inline explicit opt_allocator() {}
    inline explicit opt_allocator(opt_allocator const&) {}

    template<typename U>
    inline opt_allocator(opt_allocator<U> const&) {}

    inline ~opt_allocator() {}

    inline pointer address(reference r) { return &r; }
    inline const_pointer address(const_reference r) { return &r; }

    inline pointer allocate(size_type cnt, const_pointer hint = 0)
    {
        return reinterpret_cast<pointer>(optimizer->alloc(cnt * sizeof (T)));
    }
    
    inline void deallocate(pointer p, size_type) { }

    inline size_t max_size() const {
        return MEMORY_BLOCK_SIZE / sizeof(T);
    }

    //    construction/destruction
    inline void construct(pointer p, const T& t) { new(p) T(t); }
    inline void destroy(pointer p) { p->~T(); }

    inline bool operator==(opt_allocator const&) { return true; }
    inline bool operator!=(opt_allocator const& a) { return !operator==(a); }
};

typedef std::vector<Predicate *, opt_allocator<Predicate *> > PredicateList;
typedef std::vector<DataNode *, opt_allocator<DataNode *> > DataNodeList;
typedef std::set<DataNode *, std::less<DataNode *>, opt_allocator<DataNode *> > DataNodeSet;
typedef std::vector<DataGraph *, opt_allocator<DataGraph *> > DataGraphList;
typedef std::set<DataGraph *, std::less<DataGraph *>, opt_allocator<DataGraph *> > DataGraphSet;

typedef std::multimap<TupleId, DataNode *> VariableMap;
typedef std::map<std::string, DataNode *> VariableNameMap;

typedef std::set<TupleId, std::less<TupleId>, opt_allocator<TupleId> > TupleScheme;

typedef std::vector<tuple_cell> MemoryTupleSequence;
typedef counted_ptr< std::vector<tuple_cell> > MemoryTupleSequencePtr;

static inline
TupleScheme singleTupleScheme(opt::TupleId tid)
{
    TupleScheme a;
    a.insert(tid);
    return a;
}

class IPlanDisposable {
public:
    virtual ~IPlanDisposable() {};
    void * operator new(size_t n)
    {
        return optimizer->createObject(n);
    };
    
    void operator delete(void *) { return; };
};

inline static
PlanDesc singlePlanDesc(uint8_t item) { return 1ULL << item; };

}

#endif /* OPTTYPES_H */
