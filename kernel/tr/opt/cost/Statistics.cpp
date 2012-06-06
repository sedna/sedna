#include "Statistics.h"

#include "tr/opt/phm/Operations.h"
#include "tr/opt/path/XPathLookup.h"

using namespace opt;

CostModel * opt::publicCostModel = NULL;
const double opt::C_CPU_Cost = 0.1;
const double opt::C_IO_Cost = 10.0;

#define AXIS_DESC_COST (50.0)
#define AXIS_CHILD_COST (10.0)
#define AXIS_SINGLE_COST (1.0)

#define AXIS_DESC_COST_SINGLE (10.0)
#define AXIS_CHILD_COST_SINGLE (1.0)

// TODO : MAKE CACHE AT MUST !!!

struct PathCostModelData : public IPlanDisposable {
    std::vector<schema_node_xptr> snodes;
    scoped_ptr<pe::SchemaLookup> lookup;
};

PathCostModel* CostModel::evaluatePathCost(const DataRoot& root, const pe::Path& path, TupleStatistics * copyStats, PathCostModel * result, TupleStatistics * baseStats)
{
    PathCostModelData * modelData = new PathCostModelData;
    pe::SchemaLookup * scmLookup = new pe::SchemaLookup(path);

    result->data = modelData;
    modelData->lookup = scmLookup;

    scmLookup->compile();

    if (path.getBody()->rbegin()->getAxis() == pe::axis_parent) {
        if (baseStats != NULL) {
            result->card = baseStats->distinctValues;
            result->blockCount = baseStats->pathInfo->blockCount;
            result->nidSize = baseStats->pathInfo->nidSize;
            result->schemaTraverseCost = path.getBody()->size() * getCPUCost();
            result->iterationCost = path.getBody()->size() * getIOCost();
            scmLookup->executeAll(&((PathCostModelData*) baseStats->pathInfo->data)->snodes, &modelData->snodes);

            if (copyStats != NULL) {
                copyStats->pathInfo = result;
                copyStats->distinctValues = result->card;
            };
        }

        return result;
    }
    
    result->nidSize = 0;

    result->iterationCost = scmLookup->atomizedPath.cost();
    result->schemaTraverseCost = scmLookup->atomizedPath.cost();

    if (baseStats == NULL) {
        scmLookup->execute(root.getSchemaNode(), &modelData->snodes);
    } else if (baseStats->pathInfo->data == NULL) {
        if (path.getBody()->rbegin()->getAxis() != pe::axis_parent) {
            double error = scmLookup->findSomething(root, &modelData->snodes, 0) * 100;
        }
    } else {
        scmLookup->executeAll(&((PathCostModelData*) baseStats->pathInfo->data)->snodes, &modelData->snodes);
    }

    if (!modelData->snodes.empty()) {
        std::vector<schema_node_xptr>::const_iterator it = modelData->snodes.begin();
        schema_node_cptr sn = *it;

        result->blockCount = sn->blockcnt;
        result->card = sn->nodecnt;
        result->nidSize = sn->extnids;

        for (++it; it != modelData->snodes.end(); ++it) {
            schema_node_cptr sn = *it;

            result->blockCount.lower = std::min(result->blockCount.lower, (double) sn->blockcnt);
            result->blockCount.upper += (double) sn->blockcnt;

            result->card.lower = std::min(result->card.lower, (double) sn->nodecnt);
            result->card.upper += (double) sn->nodecnt;

            result->nidSize.lower = std::min(result->card.lower, (double) sn->extnids);
            result->nidSize.upper += (double) sn->extnids;
        };
    };

    result->schemaTraverseCost *= getCPUCost();
    result->iterationCost *= getIOCost();

    if (copyStats != NULL) {
        copyStats->pathInfo = result;
        copyStats->distinctValues = result->card;
    };

    return result;
};

PathCostModel* CostModel::getAbsPathCost(const DataRoot& root, const pe::Path& path, TupleStatistics * result)
{
    PathCostModel * cm = new PathCostModel();

    cm->card = 1;
    cm->blockCount = 1;

    return evaluatePathCost(root, path, result, cm, NULL);
}

PathCostModel* CostModel::getPathCost(const TupleRef& base, const pe::Path& path, TupleStatistics * _result)
{
    PathCostModel * cm = new PathCostModel();

    U_ASSERT(base.tupleDesc != NULL);

    if (base->statistics != NULL) {
        cm->card = base->statistics->distinctValues;
        U_ASSERT(base->statistics->pathInfo != NULL);
        cm->blockCount = base->statistics->pathInfo->blockCount;
        cm->nidSize = base->statistics->pathInfo->nidSize;
    } else {
        cm->card = 1;
        cm->blockCount = 1;
    };

    return evaluatePathCost(base->node->root, path, _result, cm, base->statistics);
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

EvaluationInfo* CostModel::getCmpInfo(TupleStatistics* m1, TupleStatistics* m2, const Comparison& cmp)
{
    EvaluationInfo* result = new EvaluationInfo;

    Range values1 = std::min(m1->distinctValues, m2->distinctValues);
    Range values2 = std::max(m1->distinctValues, m2->distinctValues);

    switch (cmp.op) {
      case Comparison::g_eq :
        result->selectivity = values1 / values2;
        result->selectivity.upper *= 10;
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

TupleStatistics* CostModel::getConstInfo(MemoryTupleSequencePtr cnst)
{
    TupleStatistics* result = new TupleStatistics;

    result->pathInfo = NULL;
    result->distinctValues = cnst->size();
    result->valueInfo = new ValueCostModel();
    result->valueInfo->atomizationCost = 0;
    result->valueInfo->size = (double) (sizeof(tuple_cell));

    return result;
}

EvaluationInfo* CostModel::getDocOrderInfo(PathCostModel* m1, PathCostModel* m2, const pe::Path& path)
{
    EvaluationInfo* result = new EvaluationInfo;

    if (path.horizontal()) {
        if (path.forall(pe::StepPredicate::axis(pe::axis_following) | pe::StepPredicate::axis(pe::axis_preceding))) {
            result->selectivity = 10.0;
        } else {
            result->selectivity = 2.0;
        };
    } else {
        result->opCost = path.getBody().get()->size() * getCPUCost();
        result->selectivity = 0.5;
    }

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

