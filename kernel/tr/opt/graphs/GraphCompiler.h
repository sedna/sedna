#ifndef _GRAPH_COMPILER_H_
#define _GRAPH_COMPILER_H_

#include "tr/opt/graphs/DataGraphs.h"

namespace opt {

class GraphCompiler
{
//    typedef std::pair<DataGraph *, phop::ITupleOperator*> CompiledGraph;
//    typedef std::vector<CompiledGraph> GraphList;
//    typedef std::vector<GraphList> GraphHash;
    typedef std::map<DataGraph *, phop::ITupleOperator*> GraphMap;

    GraphMap graphCache;

    phop::ITupleOperator* getGraph(DataGraph * dg)
    {
        GraphMap::iterator it = graphCache.find(dg);
        return it == graphCache.end() ? NULL : *it;
    };

public:
//    GraphCompiler(DataGraphMaster * dgm);
    phop::ITupleOperator* compile(DataGraphIndex & graph);
};

};

#endif /* _GRAPH_COMPILER_H_ */
