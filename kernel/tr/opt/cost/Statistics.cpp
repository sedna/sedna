#include "Statistics.h"

#include "tr/opt/phm/Operations.h"

CostModel * publicCostModel;

void CostModel::evaluateBaseStatistics(SchemeElement* stats)
{
    switch (stats->node->type) {
        case DataNode::dnDatabase : {
            stats->statistics = new Statistics();
            stats->statistics->self = new TupleStatistics();
            stats->statistics->self->schemaNodeCount = Range(1, 1);
            stats->statistics->self->nodeCount = Range(10, 100);
        }
        case DataNode::dnExternal : {
            stats->statistics = new Statistics();
            stats->statistics->self = new TupleStatistics();
            stats->statistics->self->schemaNodeCount = Range(1, 1);
            stats->statistics->self->nodeCount = Range(10, 100);
        }
        default : break;
    };
}

Range CostModel::getBaseSortCost(Statistics* stats)
{
    if (stats->self->baseOperationCosts != NULL) {
    };

    return stats->self->nodeCount;
}

Range CostModel::getPathSelectivity(SchemeElement* base, const pe::Path& path)
{
//    Statistics * x = new Statistics();
//    x->self = base->statistics->self;
    return Range(1.0, 1.0);
}


