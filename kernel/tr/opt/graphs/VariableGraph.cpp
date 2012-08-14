/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "VariableGraph.h"

#include "tr/opt/OptTypes.h"
#include "tr/opt/phm/PhysicalModel.h"
#include "tr/opt/graphs/Predicates.h"
#include "tr/opt/graphs/DataGraphs.h"
#include "tr/opt/algebra/IndependentPlan.h"

#include "tr/structures/nodetypes.h"

#include "tr/models/XmlConstructor.h"
#include "tr/models/StlFixes.h"

#include <algorithm>

using namespace std;
using namespace opt;

static const char * graphLRError = "Invalid datagraph LR represenation";
static const char * nodeLRError = "Invalid datagraph node LR represenation";
static const char * predicateLRError = "Invalid predicate LR represenation";

VariableUsageGraph::VariableUsageGraph()
{
    alwaysTrueSequence = new MemoryTupleSequence();
    alwaysTrueSequence->push_back(tuple_cell::atomic(true));
}

VariableUsageGraph::~VariableUsageGraph()
{

}

TupleInfo & VariableUsageGraph::addVariableDeclaration(TupleId tid, rqp::RPBase* op)
{
    if (variableMap.find(tid) == variableMap.end()) {
        variableMap.insert(TupleInfoMap::value_type(tid, TupleInfo(tid)));
    };

    TupleInfo & info = getVariable(tid);
    info.definedIn = op;

    return info;
}

TupleInfo & VariableUsageGraph::addVariableUsage(TupleId tid, rqp::RPBase* op, DataNode* dataNode)
{
    if (variableMap.find(tid) == variableMap.end()) {
        variableMap.insert(TupleInfoMap::value_type(tid, TupleInfo(tid)));
    };

    TupleInfo & info = getVariable(tid);

    if (op != NULL) {
        info.operations.insert(op);
    }

    if (dataNode != NULL) {
        info.nodes.insert(dataNode);
    }

    return info;
}

void VariableUsageGraph::cleanup()
{
    for (TupleInfoMap::iterator it = variableMap.begin(); it != variableMap.end(); )
    {
        if (it->second.pointsTo != invalidTupleId
          || it->second.definedIn == NULL)
        {
            U_ASSERT(it->second.definedIn == NULL);
            U_ASSERT(it->second.operations.empty());

            it = std::set_erase(variableMap, it);

            continue;
        };

        ++it;
    }
}

TupleInfo & VariableUsageGraph::addVariableName(TupleId tid, const string& name)
{
    if (variableMap.find(tid) == variableMap.end()) {
        variableMap.insert(TupleInfoMap::value_type(tid, TupleInfo(tid)));
    };

    TupleInfo & info = getVariable(tid);
    info.name = name;
    return info;
}

void VariableUsageGraph::addVariableDataNode(DataNode* dn)
{
    U_ASSERT(dn->varTupleId != opt::invalidTupleId);

    if (dn->type != DataNode::dnAlias && dn->type != DataNode::dnExternal) {
        addVariableDeclaration(dn->varTupleId, dn->parent->operation);
    } else {
        addVariableUsage(dn->varTupleId, dn->parent->operation, dn);
    };
}

void VariableUsageGraph::removeVariableDataNode(DataNode* dn)
{
    if (dn->varTupleId != opt::invalidTupleId) {
        TupleInfo & info = getVariable(dn->varTupleId);

        if (dn->type != DataNode::dnAlias && dn->type != DataNode::dnExternal) {
            info.definedIn = NULL;
        } else {
            info.nodes.erase(dn);

            /* WARNING: Check whether it is this really safe to delete operation at this point */
            info.operations.erase(dn->parent->operation);
        };
    }
}

TupleInfo & VariableUsageGraph::mergeVariables(TupleId master, TupleId alias)
{
    U_ASSERT(false);
/*
    TupleInfo & varMaster = getVariable(master);
    TupleInfo & varAlias = getVariable(alias);

    if (varMaster.id == varAlias.id) {
        return varMaster;
    }
    
    if (varMaster.name.empty() || (!varAlias.name.empty() && (varAlias.name[0] == '$'))) {
        varMaster.name = varAlias.name;
    };

    U_ASSERT(varMaster.producer == NULL || varAlias.producer == NULL);

    if (varMaster.definedIn == NULL) {
        varMaster.definedIn = varAlias.definedIn;

        if (varAlias.definedIn != NULL) {
            varAlias.definedIn = NULL;
        }
    };

    for (DataNodeSet::iterator it = varAlias.nodes.begin(); it != varAlias.nodes.end(); ++it) {
        (*it)->varTupleId = varMaster.id;
    };

    varMaster.nodes.insert(varAlias.nodes.begin(), varAlias.nodes.end());
    varAlias.nodes.clear();
    varAlias.pointsTo = varMaster.id;

    return varMaster;
*/    
}

XmlConstructor& VariableUsageGraph::toXML(XmlConstructor& constructor) const
{
    constructor.openElement(SE_EL_NAME("var-map"));

    for (TupleInfoMap::const_iterator it = variableMap.begin(); it != variableMap.end(); ++it)
    {
        constructor.openElement(SE_EL_NAME("var"));
        constructor.addAttributeValue(SE_EL_NAME("varId"), tuple_cell::atomic_int(it->first));

        if (it->second.pointsTo != invalidTupleId) {
            constructor.addAttributeValue(SE_EL_NAME("replaced by"), tuple_cell::atomic_int(it->second.pointsTo));
        } else {
            if (it->second.properties.notNull()) {
                constructor.addElementValue(SE_EL_NAME("flag"), "not-null");
            };

            if (it->second.properties.singleton()) {
                constructor.addElementValue(SE_EL_NAME("flag"), "singleton");
            };

            if (it->second.properties.nodes()) {
                constructor.addElementValue(SE_EL_NAME("flag"), "node-only");
            };

            if (it->second.definedIn != NULL) {
                constructor.openElement(SE_EL_NAME("definedIn"));
                constructor.addAttributeValue(SE_EL_NAME("type"), tuple_cell::atomic_deep(xs_string, it->second.definedIn->info()->name));
                constructor.addAttributeValue(SE_EL_NAME("id"), tuple_cell::atomic_int(it->second.definedIn->oid()));
                constructor.closeElement();
            }

            for (OperationSet::const_iterator jt = it->second.operations.begin(); jt != it->second.operations.end(); ++jt) {
                constructor.openElement(SE_EL_NAME("operation"));
                constructor.addAttributeValue(SE_EL_NAME("type"), tuple_cell::atomic_deep(xs_string, (*jt)->info()->name));
                constructor.addAttributeValue(SE_EL_NAME("id"), tuple_cell::atomic_int((*jt)->oid()));
                constructor.closeElement();
            };

            for (DataNodeSet::const_iterator jt = it->second.nodes.begin(); jt != it->second.nodes.end(); ++jt) {
                (*jt)->toXML(constructor);
            };
        };

        constructor.closeElement();
    };

    constructor.closeElement();

    return constructor;
}



