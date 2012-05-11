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

struct Comparison;

class PPIterator;
class DataRoot;
class tuple_cell;

namespace pe {
  class Path;
}

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
    VariableMap variableMap;

    DataNode * createNode(DataGraph * dg);
    Predicate * createPredicate(DataGraph * dg, Predicate * predicate);

    DataNode * createNodeFromLR(DataGraph * dg, const scheme_list * vf, VariableNameMap * vmap);
    Predicate * createPredicateFromLR(DataGraph * dg, const scheme_list * vf, VariableNameMap * vmap);
  public:
    /* Factory functions */

    DataGraph * createGraph();
    DataGraph * createGraphFromLR(const scheme_list * vf);

    DataNode * createFreeNode(DataGraph * dg);
    DataNode * createConstNode(DataGraph * dg, const tuple_cell & tc);
    DataNode * createRootNode(DataGraph * dg, const DataRoot & root, const pe::Path & _path);

    DataGraph * createPath(DataGraph* dg, TupleId left, TupleId right, const pe::Path& _path, bool outer = false);
    DataGraph * createComparison(DataGraph * dg, TupleId left, TupleId right, const Comparison & cmp);

    Predicate * replacePredicate(DataGraph* dg, Predicate* predicate, Predicate* withPredicate);
    
    DataNode * getVarNode(TupleId var) const { return variableMap.at(var); };

    phop::ITupleOperator* compile(DataGraph* dg);
};

/*
class DataGraphIterator {
  private:
    DataGraph * m_datagraph;
    std::vector<int> m_elements;
    tuple m_tuple;
  public:
    DataGraphIterator(DataGraph* datagraph, const TupleScheme& scheme);
    ~DataGraphIterator();

    const tuple & next();
    const tuple & get() const { return m_tuple; };
    bool eos() const { return m_tuple.is_eos(); };
};

class PPDataGraphAdapter : public PPIterator {
  private:
    DataGraph * m_datagraph;
  public:
    virtual void do_accept(PPVisitor& v);
    virtual void do_close();
    virtual PPIterator* do_copy(dynamic_context* _cxt_);
    virtual void do_next(tuple& t);
    virtual void do_open();
    virtual void do_reopen();

    PPDataGraphAdapter(dynamic_context* _cxt_, operation_info _info_, const char* _name_, DataGraph * datagraph, rqp::TupleId tid);
    virtual ~PPDataGraphAdapter();
};
*/

#endif /* DATAGRAPH_H */
