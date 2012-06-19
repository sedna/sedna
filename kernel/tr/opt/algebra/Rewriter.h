#ifndef _REWRITER_H_
#define _REWRITER_H_

#include "DataGraphs.h"

namespace opt {
  
struct DataGraphRewriter {
    DataGraphWrapper graph;
    DataGraphIndex index;

    DataGraphRewriter(DataGraph * dg) : graph(dg), index(dg) {};

    void mergeNodes(const DataNodeIndex& master, const DataNodeIndex& alias);

    void selfReferenceResolution();
    void aliasResolution();
    void structuralComparison();
    void doPathExpansion();
};

};

#endif /* _REWRITER_H_ */
