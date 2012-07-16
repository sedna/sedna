/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _DATAGRAPH_COLLECTION_H
#define _DATAGRAPH_COLLECTION_H

#include <map>
#include <vector>
#include <string>

#include "unistd.h"

#include "tr/opt/OptTypes.h"
#include "tr/executor/por2qep/scheme_tree.h"

namespace rqp {
    class RPBase;
}

namespace opt {

struct DataGraphBuilder;
 
struct VariableInfo {
    TupleId id;
    std::string name;

    rqp::RPBase * declaredIn;
    DataNode * producer;
    DataNodeSet nodes;

    TupleId pointsTo;

    VariableInfo(TupleId _id) : id(_id), declaredIn(NULL), producer(NULL), pointsTo(opt::invalidTupleId) {};
};

typedef std::map<TupleId, VariableInfo> VariableInfoMap;

class DataGraphMaster {
    friend class DataGraph;
    friend class DataGraphBuilder;
public:
    DataGraphMaster();
    ~DataGraphMaster();
private:
    TupleId lastIndex;

    DataGraphList allGraphs;

/*
    DataNode * createNode(DataGraph * dg);
    Predicate * createPredicate(DataGraph * dg, Predicate * predicate);
    DataGraph * createGraph();
    
    DataNode * createNodeFromLR(DataGraph * dg, const scheme_list * vf, VariableNameMap * vmap);
    Predicate * createPredicateFromLR(DataGraph * dg, const scheme_list * vf, VariableNameMap * vmap);
*/    
public:
    VariableInfoMap variableMap;
    MemoryTupleSequencePtr alwaysTrueSequence;

    /* Factory functions */
//    DataGraph * createGraphFromLR(const scheme_list * vf);

    void addVariable(DataNode * dn);
    void addVariableDecl(TupleId tid, rqp::RPBase * op);
    void removeVariable(DataNode * dn);
    void mergeVariables(TupleId t1, TupleId t2);

    void deleteGraph(DataGraph* dg);

    VariableInfo & getVariable(TupleId tid)
    {
        U_ASSERT(variableMap.find(tid) != variableMap.end());

        VariableInfo & info = variableMap.at(tid);

        while (info.pointsTo != opt::invalidTupleId) {
            info = variableMap.at(info.pointsTo);
        };
        
        return info;
    };

    void setVarName(TupleId varTupleId, const std::string & name);

 //    DataNode * getVarNode(TupleId var) const { return allNodes.at(var); };

    DataGraph * join(DataGraph * left, DataGraph * right);
    DataGraph * leftOuterJoin(DataGraph * left, DataGraph * right);
};

}

#endif /* _DATAGRAPH_COLLECTION_H */
