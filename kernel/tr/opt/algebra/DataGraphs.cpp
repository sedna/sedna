#include "DataGraphs.h"

using namespace opt;

DataGraphWrapper::DataGraphWrapper(DataGraph* _dg) : dg(_dg)
{
    nodes.reserve(MAX_GRAPH_SIZE / 2);
    in.reserve(MAX_GRAPH_SIZE / 2);
    out.reserve(MAX_GRAPH_SIZE / 2);
    predicates.reserve(MAX_GRAPH_SIZE / 2);

    FOR_ALL_GRAPH_ELEMENTS(dg->dataNodes, i) {
        nodes.insert(dg->dataNodes[i]);

        if ((dg->dataNodes[i]->indexBit & dg->outputNodes) > 0) {
            out.insert(dg->dataNodes[i]);
        };

        if ((dg->dataNodes[i]->indexBit & dg->inputNodes) > 0) {
            in.insert(dg->dataNodes[i]);
        };
    };

    FOR_ALL_GRAPH_ELEMENTS(dg->predicates, i) {
        predicates.insert(dg->predicates[i]);
    };
}

DataGraph* DataGraphBuilder::build(DataGraphMaster* master)
{

}


