#include "Statistics.h"

#include "tr/opt/phm/Operations.h"

CostModel * publicCostModel = NULL;

const double C_CPU_Cost = 0.1;
const double C_IO_Cost = 10.0;

#define AXIS_DESC_COST (50.0)
#define AXIS_CHILD_COST (10.0)
#define AXIS_SINGLE_COST (1.0)

#define AXIS_DESC_COST_SINGLE (10.0)
#define AXIS_CHILD_COST_SINGLE (1.0)

PathCostModel* CostModel::getAbsPathCost(const DataRoot& root, const pe::Path& path, TupleStatistics * result)
{
    return getPathCost(TupleRef(NULL, 1), path, result);
}

PathCostModel* CostModel::getPathCost(const TupleRef& base, const pe::Path& path, TupleStatistics * _result)
{
    PathCostModel * result = new PathCostModel();

    pe::PathVectorPtr p = path.getBody();

    result->schemaTraverseCost = 0;
    result->iterationCost = 0;
    result->nidSize = 0;

    if (base.tupleDesc != NULL && base->statistics != NULL) {
        result->card = base->statistics->distinctValues;
        U_ASSERT(base->statistics->pathInfo != NULL);
        result->blockCount = base->statistics->pathInfo->blockCount;
    } else {
        result->card = 1;
        result->blockCount = 1;
    };

    for (pe::PathVector::const_iterator i = p->begin(); i != p->end(); ++i) {
        switch (i->getAxis()) {
            case pe::axis_descendant :
                result->schemaTraverseCost += AXIS_DESC_COST;
                result->iterationCost += AXIS_DESC_COST_SINGLE;
                result->card *= AXIS_DESC_COST;
                result->blockCount *= (1 + AXIS_DESC_COST * 0.01);
                break;
            case pe::axis_parent :
                result->schemaTraverseCost += AXIS_SINGLE_COST;
                result->iterationCost += AXIS_SINGLE_COST;
                result->blockCount *= 2;
                break;
            default:
                result->schemaTraverseCost += AXIS_CHILD_COST;
                result->iterationCost += AXIS_DESC_COST_SINGLE;
                result->card *= AXIS_CHILD_COST;
                result->blockCount *= (1 + AXIS_CHILD_COST * 0.01);
                break;
        };
    };

    result->schemaTraverseCost *= getCPUCost();
    result->iterationCost *= getIOCost();

    if (_result != NULL) {
        _result->pathInfo = result;
        _result->distinctValues = result->card;
    };

    return result;
}

ValueCostModel* CostModel::getValueCost(PathCostModel* m, TupleStatistics * _result)
{
    ValueCostModel* result = new ValueCostModel();

    result->atomizationCost = getIOCost() + getCPUCost();
    result->size = (double) (sizeof(tuple_cell));

    if (_result != NULL) {
        _result->valueInfo = result;
        _result->valueSize = result->size;
    };

    return result;
}

ComparisonInfo* CostModel::getCmpInfo(TupleStatistics* m1, TupleStatistics* m2, const Comparison& cmp)
{
    ComparisonInfo* result = new ComparisonInfo;
  
    switch (cmp.op) {
      case Comparison::g_eq :
        result->selectivity = Range(0.1, 0.5);
        result->opCost = getCPUCost();
        break;
      case Comparison::do_after :
      case Comparison::do_before :
        result->selectivity = Range(0.5);
        result->opCost = getCPUCost();
        break;
      default:
        U_ASSERT(false);
        return NULL;
    };

    return result;
}

TupleStatistics* CostModel::getConstInfo(const MemoryTupleSequence* cnst)
{
    TupleStatistics* result = new TupleStatistics;

    result->pathInfo = NULL;
    result->valueInfo = new ValueCostModel();
    result->valueInfo->atomizationCost = 0;
    result->valueInfo->size = (double) (sizeof(tuple_cell));

    return result;
}

ComparisonInfo* CostModel::getDocOrderInfo(PathCostModel* m1, PathCostModel* m2, const pe::Path& path)
{
    ComparisonInfo* result = new ComparisonInfo;

    if (path.forall(pe::StepPredicate(pe::nidComparableAxisTest))) {
        result->opCost = path.getBody().get()->size() * getCPUCost();
        result->selectivity = 1.0;
    } else {
        U_ASSERT(false);
        return NULL;
    };

    return result;
}

struct XLogXOp { double operator() (double x) { return x*log(x); } };

SequenceInfo* CostModel::getDocOrderSequenceCost(const TupleRef& tuple)
{
    SequenceInfo* result = new SequenceInfo;

    U_ASSERT(tuple->statistics->pathInfo != NULL);

    result->blockCount = tuple.tupleDesc->rowCount * tuple.tupleDesc->rowSize / PAGE_SIZE;
    result->card = tuple.tupleDesc->rowCount;
    result->sortCost =
      result->blockCount.map<XLogXOp>() * getIOCost() + 
      (result->card / result->blockCount).map<XLogXOp>() * getCPUCost()
        * tuple->statistics->pathInfo->nidSize
        * result->blockCount;
    
    return result;
}

SequenceInfo* CostModel::getValueSequenceCost(const TupleRef& tuple)
{
    SequenceInfo* result = new SequenceInfo;

    U_ASSERT(tuple->statistics->valueInfo != NULL);

    result->blockCount = tuple.tupleDesc->rowCount * tuple.tupleDesc->rowSize / PAGE_SIZE;
    result->card = tuple.tupleDesc->rowCount;
    result->sortCost =
      result->blockCount.map<XLogXOp>() * getIOCost() +
      (result->card / result->blockCount).map<XLogXOp>() * getCPUCost()
        * tuple->statistics->valueInfo->atomizationCost * result->blockCount;
    
    return result;
}

