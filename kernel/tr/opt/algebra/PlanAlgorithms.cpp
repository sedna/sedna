#include "PlanAlgorithms.h"

#include "tr/opt/graphs/Predicates.h"
#include "tr/opt/graphs/GraphRewriter.h"
#include "tr/opt/functions/FnHelpers.h"
#include "tr/opt/functions/Functions.h"

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

        DataGraphRewriter dgw(dgo->graph().dg);
        MapGraph * mg = new MapGraph(op->getList(), dgw.graph.dg, dgo->children);

        DataNode * node = new DataNode(opt::DataNode::dnAlias);

        U_ASSERT(dgo->out != NULL); 

        node->aliasFor = dgo->out;
        node->varTupleId = op->tid;

        dgw.graph.nodes.push_back(node);
        dgw.graph.out.clear();
        dgw.graph.out.push_back(node);

        dgw.graph.rebuild();
        dgw.index.update();

        dgw.doPathExpansion();
        dgw.aliasResolution();
        dgw.selfReferenceResolution();

        replaceInParent(pr, op, mg);

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

static
bool rule_conditional_Exists(PlanRewriter * pr, Exists * op)
{
    if (!isGraphExpr(op->getList())) {
        return false;
    };

    DataGraphOperation * child = static_cast<DataGraphOperation *>(op->getList());

    RPBase * parent = pr->getParent();

    if (parent == NULL) {
        return false;
    };

    if ((instanceof<If>(parent) && static_cast<If *>(parent)->getCondition() == op) ||
          (instanceof<Select>(parent) && static_cast<Select *>(parent)->getList() == op)) {
        replaceInParent(pr, op, child);
        return true;
    };

    return false;
};

static
bool rule_DataGraph_try_join(PlanRewriter * pr, DataGraphOperation * op)
{
    DataGraphWrapper & dgRight = static_cast<DataGraphOperation *>(op)->graph();
    OperationList & backList = pr->traverseStack;

    OperationList::const_reverse_iterator cop = backList.rbegin();
    OperationList::const_reverse_iterator parentOp = cop + 1;

    bool preserveNull = true;

    while (parentOp != backList.rend()) {
        if (If * nop = dynamic_cast<If *>(*parentOp)) {
            preserveNull = preserveNull && (nop->getThen() == null_op || nop->getElse() == null_op);
        } else if (FunCall * nop = dynamic_cast<FunCall *>(*parentOp)) {
//            if (nop->getFunction()) {
            // TODO :: check! most of the functions do preserve null
            preserveNull == false;
//            };
        } else if (MapGraph * nop = dynamic_cast<MapGraph *>(*parentOp)) {
            if (nop->getList() == *cop) {
                TupleScheme & declared = nop->graph().outTuples;

                if (intersection_size(declared, dgRight.inTuples) > 0) {
                    if (preserveNull) {
                        nop->joinGraph(dgRight);

                        if (op->out == NULL) {
                            replaceInParent(pr, op, new VarIn(op->out->varTupleId));
                        } else {
                            replaceInParent(pr, op, new Const(tuple_cell::atomic(true)));
                        };

                        return true;
                    } else {
                        return false;
                    };
                }
            } else {
                preserveNull == false;
            };
        } else if (NestedOperation * nop = dynamic_cast<NestedOperation *>(*parentOp)) {
            if (nop->getList() == *cop) {
                if (dgRight.inTuples.find(nop->tid) != dgRight.inTuples.end()) {
                    return false;
                };
            };

            if (SequenceConcat * nop = dynamic_cast<SequenceConcat *>(*parentOp)) {
                preserveNull = preserveNull && nop->getList() == *cop;
            }
        } else {
            preserveNull = false;
        }

        cop++;
        parentOp++;
    }

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

              traverseChildren(OperationList(mg->children.begin(), mg->children.end() - 1));
              DataNodeList & dns = mg ->graph().out;

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
          case Exists::opid :
            {
                RULE(rule_conditional_Exists(this, static_cast<Exists *>(op)));
            };
            break;
          case DataGraphOperation::opid :
            {
                if (!instanceof<MapConcat>(getParent())) {
                    RULE(rule_DataGraph_try_join(this, static_cast<DataGraphOperation *>(op)));
                };
            };
            break;
          case VarIn::opid :
            {
                U_ASSERT(varMap.find(static_cast<VarIn *>(op)->getTuple()) != varMap.end());
                varMap.at(static_cast<VarIn *>(op)->getTuple()).used = true;
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
