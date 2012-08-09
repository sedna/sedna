#ifndef _REWRITER_H_
#define _REWRITER_H_

#include "DataGraphs.h"

namespace opt {
  
struct DataGraphRewriter {
    DataGraphIndex & graph;

    DataGraphRewriter(DataGraphIndex & _graph) : graph(_graph) {};

    void mergeNodes(const DataNodeIndex& master, const DataNodeIndex& alias);

    void deleteRedundantConsts();
    void selfReferenceResolution();
    void aliasResolution();
    void structuralComparison();
    void doPathExpansion();

    void constResolution();
    
    void expandAbsolutePath();
};

};

#endif /* _REWRITER_H_ */
