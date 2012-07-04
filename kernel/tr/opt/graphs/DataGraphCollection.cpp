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

void DataGraphMaster::addVariable(DataNode* dn)
{
    if (dn->varTupleId != opt::invalidTupleId) {
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

void DataGraphMaster::removeVariable(DataNode* dn)
{
    VariableInfo & info = variableMap.at(dn->varTupleId);

    while (info.pointsTo != opt::invalidTupleId) {
        info = variableMap.at(info.pointsTo);
    };

    info.nodes.erase(dn);
}

DataGraph* DataGraphMaster::join(DataGraph* left, DataGraph* right)
{
    DataGraphWrapper lg(left);
    DataGraphWrapper rg(right);

    lg.nodes.insert(lg.nodes.end(), rg.nodes.begin(), rg.nodes.end());
    lg.predicates.insert(lg.predicates.end(), rg.predicates.begin(), rg.predicates.end());
    lg.out.insert(lg.out.end(), rg.out.begin(), rg.out.end());

    lg.rebuild();

    return left;
}

// ***************************** Execution Plan ***************************

class PlanMap {
private:
    typedef std::map<PlanDesc, PlanInfo *> PlanInfoMap;
    PlanInfoMap planInfoMap;
    PlanInfo * last;
public:
    PlanInfo * get(PlanDesc i) const { return planInfoMap.at(i); };

    bool exists(PlanDesc i) const {
        return planInfoMap.find(i) != planInfoMap.end();
    };

    PlanInfo * getLastPlan() { return last; };

    PlanInfo * update(PlanInfo * _plan) {
        PlanDesc desc = _plan->getDesc();
        PlanInfoMap::iterator i = planInfoMap.find(desc);

        if (i == planInfoMap.end()) {
            planInfoMap.insert(PlanInfoMap::value_type(desc, _plan));
            last = _plan;
        } else if (i->second->getTotalCost() > _plan->getTotalCost()) {
            i->second = _plan;
            last = _plan;
        } else {
            last = i->second;
        }

        return last;
    };
};

// ***************************** Data Graph ***************************

#include <iostream>
#include <fstream>

#include "tr/models/XmlConstructor.h"

#include "tr/crmutils/serialization.h"
#include "tr/crmutils/exec_output.h"

phop::ITupleOperator* DataGraphMaster::compile(DataGraph* dg)
{
/*
    std::ofstream F("/tmp/datagraph.log");
    se_stdlib_ostream Fstream(F);

    SCElementProducer * vrt = SCElementProducer::getVirtualRoot(XNULL);
    GlobalSerializationOptions opt;

    opt.indent = true;
    opt.indentSequence = "  ";
    opt.separateTuples = true;
    opt.useCharmap = true;

    Serializer * serializer = Serializer::createSerializer(se_output_method_xml);
    serializer->prepare(&Fstream, &opt);
*/
    DataGraphIndex dgi(dg);
    DataGraphWrapper dgw(dg);

    PlanMap * planMap = new PlanMap();

    PlanDescSet set1, set2;

    PlanDescSet * currentStepSet = &set1;
    PlanDescSet * nextStepSet = &set2;

    PlanInfo * nullPlan = new PlanInfo(dgw.nodes.size());

    int branchLimit = 4;

    FOR_ALL_GRAPH_ELEMENTS(dg->dataNodes, i) {
        // TODO: External nodes MUST become evaluated initially.
        nullPlan->initTupleSet(dg->dataNodes[i]);
    };

    planMap->update(nullPlan);
    currentStepSet->insert(0);

    while (!currentStepSet->empty()) {
        nextStepSet->clear();

/*        
        F << "\n Next set : ";

        for (PlanDescSet::const_iterator it = currentStepSet->begin(); it != currentStepSet->end(); ++it) {
            F << *it << " ";
        }

        F << "\n\n";
*/

        for (PlanDescSet::const_iterator it = currentStepSet->begin(); it != currentStepSet->end(); ++it) {
            PlanInfo * info = planMap->get(*it);
            PlanDesc dsc = dgi.getNeighbours(info->getDesc());

//            if (dsc == 0 && branchLimit > 0) {
            if (branchLimit > 0) {
                branchLimit--;
                dsc = dgi.allPredicates & ~info->getDesc();
            };

            PlanDescIterator neighbours(dsc);

            int nei;
            while (-1 != (nei = neighbours.next())) {
                PlanInfo * candidate = info->extend(dg->predicates[nei]);

                if (candidate != NULL) {
                    candidate = planMap->update(candidate);
                    nextStepSet->insert(candidate->getDesc());

/*                    
                    serializer->serialize(tuple(candidate->toXML(vrt)->close()));

                    F << "\n--------------\n";
                    F.flush();
*/
                }
            };
        };

//        F << "\n============\n";

        PlanDescSet * swapset = currentStepSet;
        currentStepSet = nextStepSet;
        nextStepSet = swapset;
    };

//    serializer->serialize(tuple(planMap->getLastPlan()->toXML(vrt)->close()));
    
    return planMap->getLastPlan()->compile();
}


