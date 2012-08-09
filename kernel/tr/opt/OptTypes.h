#ifndef OPTTYPES_H
#define OPTTYPES_H

#include "common/sedna.h"

#include "tr/executor/base/tuple.h"

#include "tr/opt/types/Range.h"
#include "tr/opt/types/Comparison.h"
#include "tr/opt/types/IntBitIterator.h"
#include "tr/opt/OptForewards.h"

#include "tr/opt/OptimizingExecutor.h"

#include <vector>
#include <set>
#include <map>
#include <cstddef>

template <typename T>
class object_vector : public std::vector<T *>
{
typedef std::vector<T *> base_t;
public:
    ~object_vector() {
        for (typename base_t::const_iterator it = base_t::begin(); it != base_t::end(); ++it) {
            delete *it;
        };
    };
};

namespace rqp {
    typedef std::vector< rqp::RPBase * > OperationList;
};

namespace opt {

typedef uint64_t PlanDesc;
typedef std::set<PlanDesc> PlanDescSet;
typedef int TupleId;

static const opt::TupleId invalidTupleId = 0;

typedef ::IntBitIterator<PlanDesc> PlanDescIterator;

#define SE_EL_NAME(N) xsd::QName::getConstantQName(NULL_XMLNS, N)

struct tuple_info_t
{
    enum sequence_flags_t {
      sf_noflags = 0x00,
      sf_notNull = 0x01,
      sf_alwaysTrue = 0x02,
      sf_alwaysFalse = 0x04,

      /* Implies notNull */
      sf_singleton = (0x08 | 0x01),
    };

    uint32_t flags;

    inline bool notNull() { return (flags & sf_notNull) > 0; }
    inline bool singleton() { return (flags & sf_singleton) > 0; }
    inline bool alwaysTrue() { return (flags & sf_alwaysTrue) > 0; }
    inline bool alwaysFalse() { return (flags & sf_alwaysFalse) > 0; }

    inline tuple_info_t() : flags(sf_noflags) {};
};

typedef std::vector<Predicate *> PredicateList;
typedef std::vector<DataNode *> DataNodeList;

typedef std::set<DataNode *, std::less<DataNode *> > DataNodeSet;
typedef std::vector<DataGraph *> DataGraphList;
typedef std::set<DataGraph *, std::less<DataGraph *> > DataGraphSet;

typedef std::set<TupleId, std::less<TupleId> > TupleScheme;

typedef std::vector<tuple_cell> MemoryTupleSequence;
typedef counted_ptr< std::vector<tuple_cell> > MemoryTupleSequencePtr;

typedef std::set< rqp::RPBase * > OperationSet;

static inline
TupleScheme singleTupleScheme(opt::TupleId tid)
{
    TupleScheme a;
    a.insert(tid);
    return a;
}

struct ICostModelDisposable
{
public:
    virtual ~ICostModelDisposable() {};
    void * operator new(size_t n)
    {
        return optimizer->costModelPool.alloc(n);
    };

    void operator delete(void *) { return; };
};

class IPlanDisposable {
public:
    virtual ~IPlanDisposable() {};
    void * operator new(size_t n)
    {
        return optimizer->planGenerationPool.alloc(n);
    };
    
    void operator delete(void *) { return; };
};

inline static
PlanDesc singlePlanDesc(uint8_t item) { return 1ULL << item; };

}

enum {
    plan_operation_base,

    plan_operation_ConstantOperation,
    plan_operation_ListOperation,
    plan_operation_NestedOperation,
    plan_operation_ManyChildren,

    plan_operation_Const,
    plan_operation_VarIn,
    plan_operation_Exists, // Important: differs from effective boolean value
    plan_operation_FalseIfNull,
    plan_operation_Sequence,

    plan_operation_XPathStep,
    plan_operation_FunCallParams,
    plan_operation_Construct,

    plan_operation_MapGraph,

    plan_operation_MapConcat,
    plan_operation_SequenceConcat,

    plan_operation_If
};


enum {
    physical_model_POProt,
    physical_model_BinaryOpPrototype,
    physical_model_MergeJoinPrototype,
    physical_model_FilterTuplePrototype,
    physical_model_AbsPathScanPrototype,
    physical_model_PathEvaluationPrototype,
    physical_model_ValueScanPrototype,
    physical_model_EvaluatePrototype,
    physical_model_ExternalVarPrototype,
    physical_model_ValidatePathPrototype,
};

enum {
    sequence_operator_IOperator,
    sequence_operator_IValueOperator,
    sequence_operator_ITupleOperator,

    sequence_operator_ReduceToItemOperator,
    sequence_operator_BinaryTupleOperator,
    sequence_operator_UnaryTupleOperator,
    sequence_operator_ItemOperator,

    sequence_operator_DocOrderMerge,
    sequence_operator_TupleSort,

    sequence_operator_TupleJoinFilter,
    sequence_operator_TuplePredicateFilter,
    sequence_operator_SchemaScan,
    sequence_operator_SchemaValueScan,
    sequence_operator_VariableIn,
    sequence_operator_NestedEvaluation,
    sequence_operator_BogusConstSequence,
    sequence_operator_CachedNestedLoop,

    sequence_operator_PathSchemaCheck,
    sequence_operator_PathEvaluateTraverse,
    sequence_operator_PathSchemaResolve,
};


#endif /* OPTTYPES_H */
