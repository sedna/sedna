/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "DataGraphCollection.h"

#include "tr/opt/OptTypes.h"
#include "tr/opt/phm/PhysicalModel.h"
#include "tr/opt/graphs/Predicates.h"
#include "tr/opt/graphs/DataGraphs.h"

#include "tr/structures/nodetypes.h"

#include "tr/models/XmlConstructor.h"

#include <algorithm>

using namespace std;
using namespace opt;

static const char * graphLRError = "Invalid datagraph LR represenation";
static const char * nodeLRError = "Invalid datagraph node LR represenation";
static const char * predicateLRError = "Invalid predicate LR represenation";

DataGraphMaster::DataGraphMaster() : lastIndex(0)
{
    alwaysTrueSequence = new MemoryTupleSequence();
    alwaysTrueSequence->push_back(tuple_cell::atomic(true));
}

DataGraphMaster::~DataGraphMaster()
{

}

inline static
DataNode * getDataNode(VariableNameMap* vmap, const char * x) {
    VariableNameMap::const_iterator i = vmap->find(x);

    if (i != vmap->end()) {
        return i->second;
    };

    return NULL;
};

/*
inline static
tuple_cell tc(const scm_elem & vf) {
    switch (vf.type) {
//      case SCM_BOOL : return tuple_cell::atomic(vf.internal.b);
//      case SCM_CHAR : return tuple_cell::atomic(vf.internal.ch);
      case SCM_NUMBER : return tuple_cell::atomic(atof(vf.internal.num));
      case SCM_STRING : return tuple_cell::atomic_deep(xs_string, vf.internal.str);
      default:
        U_ASSERT(false);
        return tuple_cell::eos();
    };
};

DataNode* DataGraphMaster::createNodeFromLR(DataGraph* dg, const scheme_list* lst, VariableNameMap* vmap)
{
    DataNode* result = NULL;

    unsigned i = 0;

    const char * node_id = scmGetSymbol(lst, i++, nodeLRError);
    const char * node_type = scmGetSymbol(lst, i++, nodeLRError);

    if (strcmp(node_type, "free") == 0) {
        result = createNode(dg);
        result->type = DataNode::dnFreeNode;
    } else if (strcmp(node_type, "const") == 0) {
        result = createNode(dg);
        result->type = DataNode::dnConst;
        result->sequence = new MemoryTupleSequence();
        result->sequence->push_back(tc(lst->at(i++)));
    } else if (strcmp(node_type, "root") == 0) {
        result = createNode(dg);
        result->type = DataNode::dnDatabase;
        result->root = scmGetList(lst, i+0, nodeLRError);
        result->path = scmGetList(lst, i+1, nodeLRError);
        i += 2;
    } else {
        throw USER_EXCEPTION2(SE1004, nodeLRError);
    }

    for (; i < lst->size(); ++i) {
        const char * option = scmGetSymbol(lst, i, nodeLRError);

        if (strcmp(option, "output") == 0) {
            dg->outputNodes |= result->indexBit;
        };
    };

    vmap->insert(VariableNameMap::value_type(node_id, result));
    result->varName = node_id;

    return result;
}

Predicate* DataGraphMaster::createPredicateFromLR(DataGraph* dg, const scheme_list* lst, VariableNameMap* vmap)
{
    Predicate* result = NULL;

    int i = 0;
    const char * node_type = scmGetSymbol(lst, i++, predicateLRError);

    if (strcmp(node_type, "sj") == 0) {
        result = new StructuralPredicate(
            getDataNode(vmap, scmGetSymbol(lst, i++, predicateLRError)),
            getDataNode(vmap, scmGetSymbol(lst, i++, predicateLRError)),
            pe::Path(scmGetList(lst, i++, predicateLRError))
        );
    } else if (strcmp(node_type, "vj") == 0) {
        result = new ValuePredicate(
            getDataNode(vmap, scmGetSymbol(lst, i++, predicateLRError)),
            getDataNode(vmap, scmGetSymbol(lst, i++, predicateLRError)),
            Comparison(scmGetList(lst, i++, predicateLRError)));
    }

    return result;
}

DataGraph* DataGraphMaster::createGraphFromLR(const scheme_list* vf)
{
    if (strcmp(scmGetSymbol(vf, 0, graphLRError), "datagraph") != 0) {
        throw USER_EXCEPTION2(SE1004, graphLRError);
    };

    scheme_list * nodes = scmGetList(vf, 1, graphLRError);
    scheme_list * predicates = scmGetList(vf, 2, graphLRError);

    VariableNameMap tmpMap;

    DataGraph * dg = createGraph();

    for (scheme_list::const_iterator i = nodes->begin(); i != nodes->end(); ++i) {
        if (i->type != SCM_LIST) {
            throw USER_EXCEPTION2(SE1004, graphLRError);
        };
        createNodeFromLR(dg, i->internal.list, &tmpMap);
    };

    for (scheme_list::const_iterator i = predicates->begin(); i != predicates->end(); ++i) {
        if (i->type != SCM_LIST) {
            throw USER_EXCEPTION2(SE1004, graphLRError);
        };
        createPredicateFromLR(dg, i->internal.list, &tmpMap);
    };

    return dg;
}
*/

void DataGraphMaster::addVariableDecl(TupleId tid, rqp::RPBase* op)
{
    U_ASSERT(tid != opt::invalidTupleId);
    
    if (variableMap.find(tid) == variableMap.end()) {
        variableMap.insert(VariableInfoMap::value_type(tid, VariableInfo(tid)));
    };

    VariableInfo & info = variableMap.at(tid);

    while (info.pointsTo != opt::invalidTupleId) {
        info = variableMap.at(info.pointsTo);
    };

    info.declaredIn = op;
}

void DataGraphMaster::addVariable(DataNode* dn)
{
    if (dn->varTupleId != opt::invalidTupleId) {
//        debug_xml("opt.var", dn->serialize());
      
        if (variableMap.find(dn->varTupleId) == variableMap.end()) {
            variableMap.insert(VariableInfoMap::value_type(dn->varTupleId, VariableInfo(dn->varTupleId)));
        };

        VariableInfo & info = variableMap.at(dn->varTupleId);

        while (info.pointsTo != opt::invalidTupleId) {
            info = variableMap.at(info.pointsTo);
        };

        if (dn->type != DataNode::dnAlias && dn->type != DataNode::dnExternal) {
            U_ASSERT(info.producer == dn || info.producer == NULL);
            info.producer = dn;
        } else {
            info.nodes.insert(dn);
        };
    };
}

void DataGraphMaster::setVarName(TupleId varTupleId, const string& name)
{
    if (variableMap.find(varTupleId) == variableMap.end()) {
        variableMap.insert(VariableInfoMap::value_type(varTupleId, VariableInfo(varTupleId)));
    };

    VariableInfo & info = variableMap.at(varTupleId);

    while (info.pointsTo != opt::invalidTupleId) {
        info = variableMap.at(info.pointsTo);
    };

    info.name = name;
}

void DataGraphMaster::mergeVariables(TupleId t1, TupleId t2)
{
    VariableInfo & var1 = getVariable(t1);
    VariableInfo & var2 = getVariable(t2);

    if (var1.id == var2.id) {
        return;
    }
    
    if (var1.name.empty() || (!var2.name.empty() && (var2.name[0] == '$'))) {
        var1.name = var2.name;
    };

    U_ASSERT(var1.producer == NULL || var2.producer == NULL);

    if (var1.producer == NULL) {
        var1.producer = var2.producer;

        if (var2.producer != NULL) {
            var2.producer->varTupleId = var1.id;
            var2.producer = NULL;
        }
    };

    for (DataNodeSet::iterator it = var1.nodes.begin(); it != var1.nodes.end(); ++it) {
        (*it)->varTupleId = var1.id;
    };

    var1.nodes.insert(var2.nodes.begin(), var2.nodes.end());
    var2.nodes.clear();
    var2.pointsTo = var1.id;
}

void DataGraphMaster::deleteGraph(DataGraph* dg)
{
    FOR_ALL_GRAPH_ELEMENTS(dg->dataNodes, i)
    {
        DataNode * dn = dg->dataNodes[i];

        if (dn->varTupleId != invalidTupleId)
        {
            removeVariable(dn);
        };
    };
}

void DataGraphMaster::removeVariable(DataNode* dn)
{
    VariableInfo & info = variableMap.at(dn->varTupleId);

    while (info.pointsTo != opt::invalidTupleId) {
        info = variableMap.at(info.pointsTo);
    };

    if (info.producer == dn) {
        U_ASSERT(info.nodes.size() == 0);

        TupleId tid = dn->varTupleId;

        do {
            TupleId ntid = variableMap.at(tid).pointsTo;
            variableMap.erase(tid);
            tid = ntid;
        } while (tid != opt::invalidTupleId);
    } else {
        info.nodes.erase(dn);
    };
}

DataGraph* DataGraphMaster::join(DataGraph* left, DataGraph* right)
{
    DataGraphIndex lg(left);
    DataGraphIndex rg(right);

    lg.nodes.insert(lg.nodes.end(), rg.nodes.begin(), rg.nodes.end());
    lg.predicates.insert(lg.predicates.end(), rg.predicates.begin(), rg.predicates.end());
    lg.out.insert(lg.out.end(), rg.out.begin(), rg.out.end());

    lg.rebuild();

    return left;
}


