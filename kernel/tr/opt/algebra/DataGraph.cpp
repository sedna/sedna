/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "DataGraph.h"

#include "tr/opt/OptTypes.h"
#include "tr/opt/phm/PhysicalModel.h"
#include "tr/opt/algebra/Predicates.h"

#include "tr/structures/nodetypes.h"

using namespace std;
using namespace opt;

static const char * graphLRError = "Invalid datagraph LR represenation";
static const char * nodeLRError = "Invalid datagraph node LR represenation";
static const char * predicateLRError = "Invalid predicate LR represenation";

DataGraphMaster::DataGraphMaster() : lastIndex(0)
{
    
}

DataGraphMaster::~DataGraphMaster()
{
    for (PredicateList::iterator i = allPredicates.begin(); i != allPredicates.end(); ++i) {
        delete *i;
    };
    
    for (DataNodeList::iterator i = allNodes.begin(); i != allNodes.end(); ++i) {
        delete *i;
    };

    for (DataGraphList::iterator i = allGraphs.begin(); i != allGraphs.end(); ++i) {
        delete *i;
    };
}

DataGraph* DataGraphMaster::createGraph()
{
    DataGraph * result = new DataGraph(this);
    allGraphs.push_back(result);
    return result;
}

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
        result = createFreeNode(dg);
    } else if (strcmp(node_type, "const") == 0) {
        result = createConstNode(dg, tc(lst->at(i++)));
    } else if (strcmp(node_type, "root") == 0) {
        result =
            createRootNode(dg,
              DataRoot(scmGetList(lst, i+0, nodeLRError)),
              pe::Path(scmGetList(lst, i+1, nodeLRError)));
        i += 2;
    } else {
        throw USER_EXCEPTION2(SE1004, nodeLRError);
    }

    for (; i < lst->size(); ++i) {
        const char * option = scmGetSymbol(lst, i, nodeLRError);

        if (strcmp(option, "output") == 0) {
            result->output = true;
        };
    };

    vmap->insert(VariableNameMap::value_type(node_id, result));
    result->varName = node_id;

    return result;
}

inline static
DataNode * getDataNode(VariableNameMap* vmap, const char * x) {
    VariableNameMap::const_iterator i = vmap->find(x);

    if (i != vmap->end()) {
        return i->second;
    };

    return NULL;
};

Predicate* DataGraphMaster::createPredicateFromLR(DataGraph* dg, const scheme_list* lst, VariableNameMap* vmap)
{
    Predicate* result = NULL;

    int i = 0;
    const char * node_type = scmGetSymbol(lst, i++, predicateLRError);

    if (strcmp(node_type, "sj") == 0) {
        SPredicate * sp = new SPredicate();

        sp->dataNodeList.push_back(getDataNode(vmap, scmGetSymbol(lst, i++, predicateLRError)));
        sp->dataNodeList.push_back(getDataNode(vmap, scmGetSymbol(lst, i++, predicateLRError)));

        sp->path = pe::Path(scmGetList(lst, i++, predicateLRError));

        result = createPredicate(dg, sp);
    } else if (strcmp(node_type, "vj") == 0) {
        VPredicate * vp = new VPredicate();

        vp->dataNodeList.push_back(getDataNode(vmap, scmGetSymbol(lst, i++, predicateLRError)));
        vp->dataNodeList.push_back(getDataNode(vmap, scmGetSymbol(lst, i++, predicateLRError)));

        vp->cmp = Comparison(scmGetList(lst, i++, predicateLRError));

        result = createPredicate(dg, vp);
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

    dg->updateIndex();

    return dg;
}

DataNode* DataGraphMaster::createNode(DataGraph* dg)
{
    DataNode * result = new DataNode(DataNode::dnFreeNode, lastIndex++, dg->lastIndex++);
    
    dg->dataNodes[result->index] = result;

    allNodes.push_back(result);
    U_ASSERT(allNodes.at(result->varIndex) == result);
    
    return result;
}

DataNode * DataGraphMaster::createFreeNode(DataGraph* dg)
{
    return createNode(dg);
}

DataNode* DataGraphMaster::createConstNode(DataGraph* dg)
{
    DataNode * result = createNode(dg);
    result->type = DataNode::dnConst;
    result->sequence = NULL;
    return result;
}


DataNode * DataGraphMaster::createConstNode(DataGraph* dg, const tuple_cell& tc)
{
    DataNode * result = createConstNode(dg);
    result->sequence = new MemoryTupleSequence();
    result->sequence->push_back(tc);
    return result;
}

DataNode * DataGraphMaster::createRootNode(DataGraph* dg, const DataRoot& root, const pe::Path& _path)
{
    DataNode * result = createNode(dg);
    result->type = DataNode::dnDatabase;
    result->root = root;
    result->path = _path;
    return result;
}

Predicate* DataGraphMaster::createPredicate(DataGraph* dg, Predicate* predicate)
{
    Predicate * result = predicate;

    result->index = dg->lastIndex++;
    result->indexBit = 1 << result->index;

    result->contextTuple = 0;
    result->createContext = false;

    dg->predicates[result->index] = result;
    allPredicates.push_back(result);

    return result;
}


DataGraph* DataGraphMaster::createPath(DataGraph* dg, TupleId left, TupleId right, const pe::Path& _path, bool outer)
{
    SPredicate * p = new SPredicate();
    createPredicate(dg, p);
    p->setVertices(dg, left, right);
    p->path = _path;
    p->outer = outer;
    return dg;
}

Predicate* DataGraphMaster::replacePredicate(DataGraph* dg, Predicate* predicate, Predicate* withPredicate)
{
    if (withPredicate != NULL) {
        withPredicate->index = predicate->index;
        withPredicate->dataNodeList = predicate->dataNodeList;
        withPredicate->dataNodeMask = predicate->dataNodeMask;
        withPredicate->indexBit = predicate->indexBit;
        withPredicate->neighbours = predicate->neighbours;

        allPredicates.push_back(withPredicate);
    }

    dg->predicates[predicate->index] = withPredicate;
    
    return withPredicate;
}


DataGraph* DataGraphMaster::createComparison(DataGraph* dg, TupleId left, TupleId right, const Comparison& cmp)
{
    VPredicate * p = new VPredicate();
    createPredicate(dg, p);
    p->setVertices(dg, left, right);
    p->cmp = cmp;
    return dg;
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
    PlanMap * planMap = new PlanMap();

    PlanDescSet set1, set2;

    PlanDescSet * currentStepSet = &set1;
    PlanDescSet * nextStepSet = &set2;

    PlanInfo * nullPlan = new PlanInfo(dg->nodeCount);

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
            PlanDesc dsc = dg->getNeighbours(info->getDesc());

//            if (dsc == 0 && branchLimit > 0) {
            if (branchLimit > 0) {
                branchLimit--;
                dsc = dg->allPredicates & ~info->getDesc();
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


