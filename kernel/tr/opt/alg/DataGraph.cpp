/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "DataGraph.h"

#include "tr/opt/OptTypes.h"
#include "tr/opt/phm/PhysicalModel.h"
#include "tr/opt/alg/Predicates.h"

#include "tr/structures/nodetypes.h"

using namespace std;

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
    result->varName = new std::string(node_id);

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

        sp->leftNode = getDataNode(vmap, scmGetSymbol(lst, i++, predicateLRError));
        sp->rightNode = getDataNode(vmap, scmGetSymbol(lst, i++, predicateLRError));
        sp->path = pe::Path(scmGetList(lst, i++, predicateLRError));

        result = createPredicate(dg, sp);

        sp->update();
    } else if (strcmp(node_type, "vj") == 0) {
        VPredicate * vp = new VPredicate();

        vp->leftNode = getDataNode(vmap, scmGetSymbol(lst, i++, predicateLRError));
        vp->rightNode = getDataNode(vmap, scmGetSymbol(lst, i++, predicateLRError));

        vp->cmp = Comparison(scmGetList(lst, i++, predicateLRError));

        result = createPredicate(dg, vp);

        vp->update();
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


DataNode* DataGraphMaster::createNode(DataGraph* dg)
{
    DataNode * result = new DataNode();
    
    result->varIndex = lastIndex++;
    result->index = dg->lastIndex++;
    result->indexBit = 1 << result->index;
    
    dg->dataNodes[result->index] = result;

    allNodes.push_back(result);
    variableMap.insert(VariableMap::value_type(result->varIndex, result));
    
    return result;
}

DataNode * DataGraphMaster::createFreeNode(DataGraph* dg)
{
    DataNode * result = createNode(dg);
    result->type = DataNode::dnFreeNode;
    return result;
}

DataNode * DataGraphMaster::createConstNode(DataGraph* dg, const tuple_cell& tc)
{
    DataNode * result = createNode(dg);
    result->type = DataNode::dnConst;
    result->sequence = new MemoryTupleSequence;
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

    PlanInfo * update(PlanDesc desc, PlanInfo * _plan) {
        PlanInfoMap::iterator i = planInfoMap.find(desc);

        if (i == planInfoMap.end()) {
            planInfoMap.insert(PlanInfoMap::value_type(desc, _plan));
            return _plan;
        } else if (i->second->getTotalCost() > _plan->getTotalCost()) {
            i->second = _plan;
            return _plan;
        } else {
            return i->second;
        }
    };
};

// ***************************** Data Graph ***************************

PPIterator* DataGraphMaster::compile(DataGraph* dg)
{
    PlanTupleScheme * initialScheme = new PlanTupleScheme();
    PlanDescSet set1, set2;

    PlanDescSet * currentStepSet = &set1;
    PlanDescSet * nextStepSet = &set2;

    PlanMap * planMap = new PlanMap();

    for (DataNodeList::iterator i = dg->dataNodes.begin(); i != dg->dataNodes.end(); ++i) {
        initialScheme->update((*i)->varIndex, PlanInfo::initSchemeElement(*i));
    };

    for (PredicateList::iterator p = dg->predicates.begin(); p != dg->predicates.end(); ++p) {
        PlanDesc desc = (*p)->indexBit;
        PlanInfo * initialPlanInfo = PlanInfo::evaluateInitialPlanInfo(initialScheme, *p, desc);

        if (initialPlanInfo != NULL) {
            planMap->update(desc, initialPlanInfo);
            currentStepSet->insert(desc);
        }
    };

    while (!currentStepSet->empty()) {
        nextStepSet->clear();

        for (PlanDescSet::const_iterator it = currentStepSet->begin(); it != currentStepSet->end(); ++it) {
            int nei;
            PlanInfo * info = planMap->get(*it);
            PlanDesc desc = info->getDesc();
            PlanInfo * candidate = NULL;
            PlanDescIterator neighbours(dg->getNeighbours(desc));

            while (-1 != (nei = neighbours.next())) {
                Predicate * p = dg->predicates[nei];
                PlanDesc ndesc = desc | p->indexBit;
                planMap->update(ndesc, info->clone()->apply(p));
                nextStepSet->insert(ndesc);
            };
        };

        PlanDescSet * swapset = currentStepSet;
        currentStepSet = nextStepSet;
        nextStepSet = swapset;
    };

    PPIterator * result = planMap->getLastPlan()->compile();

    return result;
}


//DataGraph::pre

/*

#include "tr/executor/methods/SequenceModel.h"
#include "tr/executor/methods/SequenceHelpers.h"


// ***************************** Data Graph ***************************

PPIterator* DataGraphMaster::compile(DataGraph* dg)
{
    PlanTupleScheme * initialScheme = new PlanTupleScheme(dg->dataNodes.size());
    PlanDescSet set1, set2;

    PlanDescSet * currentStepSet = &set1;
    PlanDescSet * nextStepSet = &set2;

    PlanMap * planMap = new PlanMap();

    for (DataNodeList::iterator i = dg->dataNodes.begin(); i != dg->dataNodes.end(); ++i) {
        initialScheme->update(i->index, PlanInfo::initSchemeElement(*i));
    };

    for (PredicateList::iterator p = dg->predicates.begin(); p != dg->predicates.end(); ++p) {
        PlanDesc desc = singlePlanDesc(p->index);
        PlanInfo * initialPlanInfo = PlanInfo::evaluateInitialPlanInfo(initialScheme, &(*p), desc);

        if (initialPlanInfo != NULL) {
            planMap->update(desc, initialPlanInfo);
            currentStepSet->insert(desc);
        }
    };

    while (!currentStepSet->empty()) {
        nextStepSet->clear();

        for (PlanDescSet::const_iterator it = currentStepSet->begin(); it != currentStepSet->end(); ++it) {
            PlanInfo * info = planMap->get(*it);
            PlanDesc desc = info->getDesc();
            PlanInfo * candidate = NULL;
            PredicateSet neighbours = dg->getNeighbours(desc);

            for (PredicateSet::const_iterator p = neighbours.begin(); p != neighbours.end(); ++p) {
                PlanDesc ndesc = desc | singlePlanDesc(p->index);
                planMap->update(ndesc, info->apply(*p));
                nextStepSet->insert(ndesc);
            };
        };

        PlanDescSet * swapset = currentStepSet;
        currentStepSet = nextStepSet;
        nextStepSet = swapset;
    };

    PPIterator * result = planMap->getLastPlan()->compile();

    return result;
}

struct Reduce : public AbstractSequence {
    AbstractSequence * inSequence;
    TupleMap savemap;

    Reduce(AbstractSequence * in, TupleMap map) : AbstractSequence(in->body.cells_number), inSequence(in), savemap(map) {
    };

    virtual bool open() {};

    virtual bool next() {
        inSequence->next();
        remap_tuple(body, inSequence->get(), savemap);
    };
};


inline DataNodeList & dataNodeUnion(const DataNodeList & A, const DataNodeList & B, DataNodeList & result) {
    std::set_union(A.begin(), A.end(), B.begin(), B.end(), back_inserter(result));
    return result;
}

/*
struct OpProt {
    SequenceElement source;
    Range getCardinality();
};

struct PlanData {
  private:
    typedef vector<OpProt *> Operations;
    Operations operations;

    typedef map<DataNode *, OpProt *> DataMap;
    DataMap dataMap;
  public:
    OpProt * getSource(DataNode * dn) {
        DataMap::iterator it = dataMap.find(dn);
//        Data
    };

    bool checkSource(DataNode* dn);
    void update(DataNode* dn, AbstractSequence * op, int pos);
};

struct DataNode {
    TupleId boundVariable;
    virtual OpProt * generate(PlanData * planData);
};

struct SchemaNodeSource : public DataNode {
    schema_node_cptr scn;
};

struct PPAdapterNode : public DataNode {
    PPOpIn op;
};

struct Atomic : public DataNode {
    tuple_cell value;
};

struct Predicate {
    virtual ~Predicate() {};
    virtual Predicate * map(Predicate *) = 0;
    virtual bool generate(PlanData * planData);
};

struct Bond : public Predicate {
    DataNode *in;
    vector<DataNode *> out;
};

struct SJoin : public Predicate {
    xpe::Path * path;
    DataNode *left, *right;

    virtual bool generate(PlanData* planData) {
        bool leftExists = planData->checkSource(left);
        bool rightExists = planData->checkSource(right);

        if (leftExists && rightExists) {
            OpProt * leftP = planData->getSource(left);
            OpProt * rightP = planData->getSource(right);

            GeneralizedSortMergeJoin * smj;

            planData->update(left, smj, leftP->source.pos);
            planData->update(right, smj, rightP->source.pos);
        } else if (leftExists) {
            OpProt * leftP = planData->getSource(left);

            PathStep * pathStep = new PathStep(leftP->source, path);

            planData->update(left, pathStep, leftP->source.pos);
            planData->update(right, pathStep, pathStep->outTuple);
        } else if (rightExists) {
            OpProt * leftP = planData->getSource(left);
            OpProt * rightP = planData->getSource(right);

            GeneralizedSortMergeJoin * smj;

            planData->update(left, smj, leftP->source.pos);
            planData->update(right, smj, rightP->source.pos);
        } else {
            OpProt * leftP = planData->getSource(left);

            PathStep * pathStep = new PathStep(leftP->source, path);

            planData->update(left, pathStep, leftP->source.pos);
            planData->update(right, pathStep, pathStep->outTuple);
        }

        OpProt * leftP = planData->getSource(left);;
        OpProt * rightP = planData->getSource(right);
    };
};

struct VJoin : public Predicate {
    DataNode *left, *right;

    virtual OpProt * generate(PlanData * planData) {
        OpProt * leftP = planData->getSource(left);
        if (leftP == NULL) { return NULL; }
        OpProt * rightP = planData->getSource(right);
        if (rightP == NULL) { return NULL; }

        Range cardLeft = leftP->getCardinality();
        Range cardRight = rightP->getCardinality();

        GeneralizedSortMergeJoin * vj = new GeneralizedSortMergeJoin(leftP->source, rightP->source);

        planData->update(left, vj, leftP->source.pos);
        planData->update(right, vj, rightP->source.pos);
    };
};


DataGraph* DataGraphMaster::collapse(DataGraph* dg)
{
}


/*


DataGraph* DataGraphMaster::apply(DataGraph* function, DataGraph* operand, rqp::TupleId var)
{
    DataGraph* result = new DataGraph();

    std::set_union(function->dataNodes.begin(), function->dataNodes.end(), operand->dataNodes.begin(), operand->dataNodes.end(), back_inserter(result->dataNodes));
    std::set_union(function->predicates.begin(), function->predicates.end(), operand->predicates.begin(), operand->predicates.end(), back_inserter(result->predicates));

    allGraphs.push_back(result);
    return result;
}


DataGraph* DataGraphMaster::map(DataGraph* function, DataGraph* operand)
{
    DataGraph* result = new DataGraph();

    dataNodeUnion(function->dataNodes, operand->dataNodes, result->dataNodes);

    allGraphs.push_back(result);
    return result;
}

DataGraph* DataGraphMaster::createFnDoc(counted_ptr<db_entity> db_ent)
{
    SchemaNodeSource * docNode = new SchemaNodeSource();
    DataGraph * result = new DataGraph();

    allPredicates.push_back(docNode);
    allGraphs.push_back(result);

    result->dataNodes.push_back(docNode);

    docNode->scn = get_schema_node(db_ent, "Unable to resolve document during optimization");

    return result;
}

DataGraph* DataGraphMaster::createSJ(DataNode* left, DataNode* right, const pe::Path& path)
{
    DataGraph * result = new DataGraph();
    SJoin * sj = new SJoin();

    allGraphs.push_back(result);
    allPredicates.push_back(sj);

    sj->path = new xpath::LightPathExpression(path);
    sj->left = left;
    sj->right = right;

    result->predicates.push_back(sj);
    result->dataNodes.push_back(left);
    result->dataNodes.push_back(right);

    return result;
}

DataNode* DataGraphMaster::getVarNode(rqp::TupleId var)
{
    VariableMap::const_iterator it = variableMap.find(var);
    
    if (it == variableMap.end()) {
        Variable * result = new Variable();

        result->var = var;
        result->boundVariables.push_back(var);
        allNodes.push_back(result);
        variableMap.insert(VariableMap::value_type(var, result));

        return result;
    } else {
        return it->second;
    }
}

*/



/* Iterator */

/*
DataGraphIterator::DataGraphIterator(DataGraph* datagraph, const rqp::TupleScheme & scheme) :
  m_datagraph(datagraph), m_elements(scheme.size()), m_tuple(scheme.size())
{
    std::copy(scheme.begin(), scheme.end(), back_inserter(m_elements));
}

const tuple & DataGraphIterator::next()
{
    for (vector<int>::size_type i = 0; i < m_elements.size(); ++i) {
        m_datagraph->groupByNodes
//        tuple.cells[i] = ;
    }
}

DataGraphIterator::~DataGraphIterator()
{

}

*/


















