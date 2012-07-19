#ifndef _GRAPH_COMPILER_H_
#define _GRAPH_COMPILER_H_

#include "tr/opt/graphs/DataGraphs.h"

namespace phop {
    class GraphExecutionBlock;
}

namespace opt {

class GraphCompiler
{
//    typedef std::pair<DataGraph *, phop::ITupleOperator*> CompiledGraph;
//    typedef std::vector<CompiledGraph> GraphList;
//    typedef std::vector<GraphList> GraphHash;
    typedef std::map<DataGraph *, phop::GraphExecutionBlock*> GraphMap;

    GraphMap graphCache;

    phop::GraphExecutionBlock* getGraph(DataGraph * dg)
    {
        GraphMap::iterator it = graphCache.find(dg);
        return it == graphCache.end() ? NULL : it->second;
    };

public:
//    GraphCompiler(DataGraphMaster * dgm);
    phop::GraphExecutionBlock* compile(DataGraphIndex & graph);
};

};

#endif /* _GRAPH_COMPILER_H_ */
