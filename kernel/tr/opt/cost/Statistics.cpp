#include "Statistics.h"

#include "tr/opt/phm/Operations.h"

CostModel * publicCostModel = NULL;

const double C_CPU_Cost = 1.0;
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

    if (base.tupleDesc != NULL && base->statistics != NULL) {
        result->card = base->statistics->distinctValues;
    } else {
        result->card = 1;
    };

    for (pe::PathVector::const_iterator i = p->begin(); i != p->end(); ++i) {
        switch (i->getAxis()) {
            case pe::axis_descendant :
                result->schemaTraverseCost += AXIS_DESC_COST;
                result->iterationCost += AXIS_DESC_COST_SINGLE;
                result->card *= AXIS_DESC_COST;
                break;
            case pe::axis_parent :
                result->schemaTraverseCost += AXIS_SINGLE_COST;
                result->iterationCost += AXIS_SINGLE_COST;
                break;
            default:
                result->schemaTraverseCost += AXIS_CHILD_COST;
                result->iterationCost += AXIS_DESC_COST_SINGLE;
                result->card *= AXIS_CHILD_COST;
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
}

ComparisonInfo* CostModel::getCmpInfo(TupleStatistics* m1, TupleStatistics* m2, const Comparison& cmp)
{

}

TupleStatistics* CostModel::getConstInfo(const MemoryTupleSequence* cnst)
{
    TupleStatistics* result = new TupleStatistics;
    result->pathInfo = NULL;
    result->valueInfo = new ValueCostModel();
}

ComparisonInfo* CostModel::getDocOrderInfo(PathCostModel* m1, PathCostModel* m2, const pe::Path& path)
{

}

SequenceInfo* CostModel::getDocOrderSequenceCost(const TupleRef& tuple)
{

}

SequenceInfo* CostModel::getValueSequenceCost(const TupleRef& tuple)
{

}

