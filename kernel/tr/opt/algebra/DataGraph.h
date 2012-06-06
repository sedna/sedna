/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef DATAGRAPH_H
#define DATAGRAPH_H

#include <map>
#include <vector>
#include <string>

#include "tr/opt/OptTypes.h"
#include "tr/executor/por2qep/scheme_tree.h"

namespace phop {
    class ITupleOperator;
}

class tuple_cell;
class DataRoot;

namespace pe {
  class Path;
}

namespace opt {

struct Comparison;

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

    DataNode * createNode(DataGraph * dg);
    Predicate * createPredicate(DataGraph * dg, Predicate * predicate);
    
    DataGraph * createGraph();
    DataGraph * createGraphFromLR(const scheme_list * vf);

    Predicate * replacePredicate(DataGraph* dg, Predicate* predicate, Predicate* withPredicate);
    
    DataNode * getVarNode(TupleId var) const { return allNodes.at(var); };

    phop::ITupleOperator* compile(DataGraph* dg);

    DataGraph * join(DataGraph * left, DataGraph * right);
    DataGraph * leftOuterJoin(DataGraph * left, DataGraph * right);
};

}

#endif /* DATAGRAPH_H */
