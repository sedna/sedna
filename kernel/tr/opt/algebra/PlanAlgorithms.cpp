#include "PlanAlgorithms.h"

#include "tr/opt/graphs/Predicates.h"
#include "tr/opt/graphs/GraphRewriter.h"
#include "tr/opt/functions/FnHelpers.h"
#include "tr/opt/functions/Functions.h"

#include <algorithm>

using namespace rqp;
using namespace opt;

/*
void PlanRewriter::traverseVariableContext(TupleScheme& scheme)
{
    for (TupleScheme::iterator it = scheme.begin(); it != scheme.end(); ++it)
    {
        varMap.insert(*it, VarStatInfo());
    };

    traverseAll(traverseStack.back().children);

    for (TupleScheme::iterator it = scheme.begin(); it != scheme.end(); ++it)
    {
        if (!varMap[cinfo.item].used) { cinfo.item = invalidTupleId; };
        varMap.insert(*it, VarStatInfo());
    };

    if (!varMap[cinfo.position].used) { cinfo.item = invalidTupleId; };
    if (!varMap[cinfo.size].used) { cinfo.item = invalidTupleId; };

    varMap.erase(cinfo.item);
    varMap.erase(cinfo.position);
    varMap.erase(cinfo.size);
}
*/

void PlanRewriter::execute()
{
    try {
        traverseStack.push_back(inputOp);
        do_execute();
    } catch (std::exception & e) {
        throw USER_EXCEPTION(2303);
    };
}

/* Rewriting rules, based on each operation */
static
bool rule_post_XPathStep_to_DataGraph(PlanRewriter * pr, XPathStep * op)
{
    U_ASSERT(op->getList() != null_op);

    if (instanceof<XPathStep>(op->getList()))
    {
        return false;
    }

    if (instanceof<DataGraphOperation>(op->getList()))
    {
        DataGraphOperation * dgo = static_cast<DataGraphOperation *>(op->getList());
        DataGraphWrapper & dgw = dgo->graph();
        DataNode * node = new DataNode(opt::DataNode::dnFreeNode);

        dgw.nodes.push_back(node);
        dgw.out.clear();
        dgw.out.push_back(node);

        U_ASSERT(dgo->out != NULL); // TODO:: exception
        dgw.predicates.push_back(
            new StructuralPredicate(dgo->out, node, op->getStep()));

        dgo->out = node;

        dgw.rebuild();

        replaceInParent(pr, op, dgo);

        return true;
    };

    if (instanceof<VarIn>(op->getList()))
    {
        DataGraphMaster * dgm = op->getContext()->dgm();
        DataGraphBuilder dgb;
        DataGraphOperation * dgo = NULL;

        DataNode * nodeIn = initGraphNode(static_cast<VarIn *>(op->getList()));
        DataNode * nodeOut = new DataNode(opt::DataNode::dnFreeNode);

        dgb.nodes.push_back(nodeIn);
        dgb.nodes.push_back(nodeOut);
        dgb.out.push_back(nodeOut);

        dgb.predicates.push_back(
            new StructuralPredicate(nodeIn, nodeOut, op->getStep()));

        DataGraph * dg = dgb.build(dgm);

        dgo = new DataGraphOperation(dg, OperationList());

        replaceInParent(pr, op, dgo);

        return true;
    };

    return false;
};

static
void check_post_MapGraph(DataNodeList &outs)
{
    for (DataNodeList::const_iterator it = outs.begin(); it != outs.end(); ++it)
    {
        U_ASSERT((*it)->varTupleId != invalidTupleId);
    };
}

static
bool rule_post_MapConcat_to_MapGraph(PlanRewriter * pr, MapConcat * op)
{
    RPBase * subexpr = op->getSubplan();

    if (instanceof<DataGraphOperation>(subexpr))
    {
        DataGraphOperation * dgo = static_cast<DataGraphOperation *>(subexpr);

        // TODO:: substitute with EBV
        if (dgo->out == NULL) {
            return false;
        };

        U_ASSERT(dgo->out != NULL);
        U_ASSERT(std::find(dgo->graph().out.begin(), dgo->graph().out.end(), dgo->out) != dgo->graph().out.end());

        dgo->out->varTupleId = op->tid;

        MapGraph * mg = new MapGraph(op->getList(), dgo->graph().dg, dgo->children);
        mg->tupleMask.insert(op->tid);

        replaceInParent(pr, op, mg);

        check_post_MapGraph(dgo->graph().out);
        return true;
    }

    return false;
};

static
bool rule_delete_Singleton_Sequence(PlanRewriter * pr, Sequence * op)
{
    if (op->children.size() == 0)
    {
        replaceInParent(pr, op, null_op);
        return true;
    };

    if (op->children.size() == 1)
    {
        replaceInParent(pr, op, op->children.at(0));
        return true;
    };

    return false;
};

// TODO : to the library

template<class Set1, class Set2>
static
size_t intersection_size(const Set1 &set1, const Set2 &set2)
{
    if(set1.empty() || set2.empty()) return 0;

    typename Set1::const_iterator it1 = set1.begin(), it1End = set1.end();
    typename Set2::const_iterator it2 = set2.begin(), it2End = set2.end();

    if (*it1 > *set2.rbegin() || *it2 > *set1.rbegin()) return 0;

    size_t result = 0;

    while(it1 != it1End && it2 != it2End)
    {
        if (*it1 == *it2) {
            result++;
            it1++;
            it2++;
        }

        if (*it1 < *it2) {
            it1++;
        } else {
            it2++;
        }
    }

    return result;
}

// Exists[./DataGraphOperation and null-preserved(., condition(.))]

static
bool rule_conditional_Exists(PlanRewriter * pr, Exists * op)
{
    DataGraphOperation * child = dynamic_cast<DataGraphOperation *>(op->getList());

    if (child == NULL) {
        return false;
    };

    OperationList & backList = pr->traverseStack;

    OperationList::const_reverse_iterator cop = backList.rbegin();
    OperationList::const_reverse_iterator parentOp = cop + 1;

    while (parentOp != backList.rend()) {
        if (isConditional(*parentOp, *cop))
        {
            replaceInParent(pr, op, child);
            return true;
        };

        if (!preservesNull(*parentOp, *cop))
        {
            return false;
        }

        cop++;
        parentOp++;
    }

    return false;
};

static
void detachGraph(DataGraphWrapper & to, DataGraphWrapper & from)
{
    to.nodes.insert(to.nodes.end(), from.nodes.begin(), from.nodes.end());
    to.out.insert(to.out.end(), from.out.begin(), from.out.end());
    to.predicates.insert(to.predicates.end(), from.predicates.begin(), from.predicates.end());

    DataNodeList varList;
    DataNodeList::iterator it = from.out.begin();
    
    while (it != from.out.end())
    {
        DataNode * dn = new DataNode(opt::DataNode::dnExternal);
        dn->varTupleId = (*it)->varTupleId;
        varList.push_back(dn); 
        ++it;
    };

    from.out.clear();
    from.predicates.clear();
    from.nodes.clear();
    
    from.out.insert(from.out.end(), varList.begin(), varList.end());
    from.nodes.insert(from.nodes.end(), varList.begin(), varList.end());

    from.rebuild();
    to.rebuild();
};

static
bool rule_DataGraph_do_rewritings(PlanRewriter * pr, DataGraphOperation * op)
{
    {
        DataGraphRewriter dgw(op->graph().dg);

        dgw.doPathExpansion();
        dgw.aliasResolution();
        dgw.selfReferenceResolution();
    }

    op->graph().update();

    return false;
}


static
bool rule_DataGraph_try_join(PlanRewriter * pr, DataGraphOperation * op)
{
    DataGraphWrapper & dgRight = static_cast<DataGraphOperation *>(op)->graph();
    OperationList & backList = pr->traverseStack;

    OperationList::const_reverse_iterator cop = backList.rbegin();
    OperationList::const_reverse_iterator parentOp = cop + 1;

    bool preserveNull = true;

    while (parentOp != backList.rend()) {
        /* If dependent variable is defined on the path, we cannot   */
        if (NestedOperation * nop = dynamic_cast<NestedOperation *>(*parentOp)) {
            if (nop->getList() == *cop &&
                  dgRight.inTuples.find(nop->tid) != dgRight.inTuples.end())
            {
               // TODO : try to push down to declaration
                return false;
            };
        };

        MapGraph * candidate = dynamic_cast<MapGraph *>(*parentOp);

        if (NULL != candidate &&
                candidate->getList() == *cop &&
                intersection_size(candidate->graph().outTuples, dgRight.inTuples) > 0)
        {
            if (!preserveNull) {
                // TODO: left join
                return false;
            }

            if (instanceof<MapGraph>(op)) {
                detachGraph(candidate->graph(), dgRight);
            } else {
                U_ASSERT(instanceof<DataGraphOperation>(op));

                if (op->out == NULL) {
                    U_ASSERT(dgRight.out.empty());
                    replaceInParent(pr, op, new Const(tuple_cell::atomic(true)));
                } else if (op->out->varTupleId != invalidTupleId) {
                    replaceInParent(pr, op, new VarIn(op->out->varTupleId));
                } else {
    /* We must introduce new bogus variable to propagade graph to the parent,
     * this variable should be declared in rewriter first */
                    TupleId varId = PlanContext::current->generateTupleId();
                    op->out->varTupleId = varId;
                    replaceInParent(pr, op, new VarIn(varId));
                };

                candidate->joinGraph(dgRight);
            }
            return true;
        };

        preserveNull = preserveNull && preservesNull(*parentOp, *cop);
        
        cop++;
        parentOp++;
    }

    return false;
};

static
bool rule_Select_try_join(PlanRewriter * pr, Select * op)
{
    DataGraphOperation * condition = dynamic_cast<DataGraphOperation *>(op->getList());
    DataGraphOperation * result = dynamic_cast<DataGraphOperation *>(op->getSubplan());

    if (result != NULL && condition != NULL) {
        TupleScheme & condDep = condition->graph().inTuples;

        if (condDep.find(op->tid) != condDep.end()) {
            result->graph().update();
            addGraphToJoin(result->graph(), condition);
            result->graph().out.clear();
            result->graph().out.push_back(result->out);
            result->out->varTupleId = op->tid;
            result->graph().rebuild();

            replaceInParent(pr, op, result);
            return true;
        };
    };

    return false;
};

void PlanRewriter::do_execute()
{
#define RULE(x) do { if (x) { goto _end_label; } } while (false)
    do {
        RPBase * op = traverseStack.back();

        /* Before or instead traverse */
        switch (op->info()->opType) {
          case MapGraph::opid :
            {
              MapGraph * mg = static_cast<MapGraph *>(op);
              mg->graph().update();

              traverseChildren(OperationList(mg->children.begin(), mg->children.end() - 1));
              DataNodeList & dns = mg->graph().out;

              openScope();

              for (DataNodeList::const_iterator it = dns.begin(); it != dns.end(); ++it)
              {
                  U_ASSERT((*it)->varTupleId != invalidTupleId);

                  declVar((*it)->varTupleId).path.push_back(
                      VarStatInfoItem(rqp::VarStatInfoItem::inf_dg, op, *it));
              };

              traverse(mg->getList());

              closeScope();
            }
            break;
          case SequenceConcat::opid :
          case MapConcat::opid :
          case Select::opid :
            {
              NestedOperation * nop = static_cast<NestedOperation *>(op);

              traverse(nop->getSubplan());

              openScope();

              declVar(nop->tid).path.push_back(
                  VarStatInfoItem(rqp::VarStatInfoItem::inf_op, op, NULL));

              traverse(nop->getList());

              closeScope();
            }
            break;
          default:
            {
              traverseChildren(op->children);
            } break;
        }

        /* After traverse */
        switch (op->info()->opType) {
          case Select::opid :
            {
                RULE(rule_Select_try_join(this, static_cast<Select *>(op)));
            };
            break;
          case Exists::opid :
            {
                RULE(rule_conditional_Exists(this, static_cast<Exists *>(op)));
            };
            break;
          case MapGraph::opid :
          case DataGraphOperation::opid :
            {
                if (!instanceof<MapConcat>(getParent())) {
                    RULE(rule_DataGraph_try_join(this, static_cast<DataGraphOperation *>(op)));
                };
                RULE(rule_DataGraph_do_rewritings(this, static_cast<DataGraphOperation *>(op)));
            };
            break;
          case VarIn::opid :
            {
                if (varMap.find(static_cast<VarIn *>(op)->getTuple()) != varMap.end()) {
                    varMap.at(static_cast<VarIn *>(op)->getTuple()).used = true;
                }
            };
            break;
          case Sequence::opid :
            {
                RULE(rule_delete_Singleton_Sequence(this, static_cast<Sequence *>(op)));
            }
            break;
          case FunCall::opid :
            {
                FunCall * fun = static_cast<FunCall *>(op);

                if (fun->getFunction()->finfo->rule_func != NULL) {
                    RULE(fun->getFunction()->finfo->rule_func(this, static_cast<FunCall *>(op)));
                };
            }
            break;
          case MapConcat::opid :
            {
                RULE(rule_post_MapConcat_to_MapGraph(this, static_cast<MapConcat *>(op)));
            }
            break;
          case XPathStep::opid :
            {
                RULE(rule_post_XPathStep_to_DataGraph(this, static_cast<XPathStep *>(op)));
            }
            break;
          default : break;
        };

        U_ASSERT(!traverseStack.empty());

// This is far better then break. To be sure all is ok.
_end_label:
        if (traverseStack.back() == op) {
            traverseStack.pop_back();
            break;
        };
    } while (!traverseStack.empty());
}
