/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _DATAGRAPH_COLLECTION_H
#define _DATAGRAPH_COLLECTION_H

#include <map>
#include <vector>
#include <string>

#include "tr/opt/OptTypes.h"
#include "tr/executor/por2qep/scheme_tree.h"

namespace opt {

class DataGraphMaster {
  friend class DataGraph;
  public:
    DataGraphMaster();
    ~DataGraphMaster();
  private:
    TupleId lastIndex;

    PredicateList allPredicates;
    DataNodeList allNodes;
    DataGraphList allGraphs;

    DataNode * createNodeFromLR(DataGraph * dg, const scheme_list * vf, VariableNameMap * vmap);
    Predicate * createPredicateFromLR(DataGraph * dg, const scheme_list * vf, VariableNameMap * vmap);
  public:
    VariableMap variableMap;

    /* Factory functions */

//    DataNode * createNode(DataGraph * dg);
//    Predicate * createPredicate(DataGraph * dg, Predicate * predicate);

//    DataGraph * createGraph();
    DataGraph * createGraphFromLR(const scheme_list * vf);

//    DataNode * getVarNode(TupleId var) const { return allNodes.at(var); };

    DataGraph * join(DataGraph * left, DataGraph * right);
    DataGraph * leftOuterJoin(DataGraph * left, DataGraph * right);

    phop::ITupleOperator* compile(DataGraph* dg);
};

}

#endif /* _DATAGRAPH_COLLECTION_H */
