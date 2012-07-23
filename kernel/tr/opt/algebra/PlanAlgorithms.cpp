#include "PlanAlgorithms.h"

#include "tr/opt/graphs/Predicates.h"
#include "tr/opt/graphs/GraphRewriter.h"
#include "tr/opt/functions/FnHelpers.h"
#include "tr/opt/functions/Functions.h"

#include <algorithm>
#include <sstream>

using namespace rqp;
using namespace opt;

/*
#include <fstream>
#include <iostream>

using std::fstream;
static std::ofstream tmpDebug("/tmp/replaces.log", fstream::app | fstream::ate);
*/

inline static
void debug_op_replace(const char * rule, const char * op, uint op1, uint op2)
{
/*
    std::stringstream str;
    str << op << " " << rule << " " << op1 << " " << op2 << "\n";
    tmpDebug << str.str();
    tmpDebug.flush();
 *    debug_string("opt.rule", str.str());
 */    
};

/*
bool PlanRewriter::__debug_trace_replace(uint op1, uint op2)
{
    std::stringstream str;
    str << "replace " << op1 << " " << op2 << "\n";
    debug_string("opt.rule", str.str());
    return true;
}
*/

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
        traverseStack.push_back(root);
        do_execute();
/*
        root->toStream(tmpDebug);
        tmpDebug << "\n";
        tmpDebug.flush();
*/
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
        DataGraphIndex & dgw = dgo->graph();
        DataNode * node = new DataNode(opt::DataNode::dnFreeNode);

        dgw.nodes.push_back(node);
        dgw.out.clear();
        dgw.out.push_back(node);

        U_ASSERT(dgo->out != NULL); // TODO:: exception
        dgw.predicates.push_back(
            new StructuralPredicate(dgo->out, node, op->getStep()));

        dgo->out = node;

        dgw.rebuild();

        debug_op_replace(__PRETTY_FUNCTION__, "replace", op->oid(), dgo->oid());
        pr->replaceInParent(op, dgo);

        return true;
    };

    if (instanceof<VarIn>(op->getList()))
    {
        DataGraphMaster * dgm = optimizer->dgm();
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

        debug_op_replace(__PRETTY_FUNCTION__, "replace", op->oid(), dgo->oid());
        pr->replaceInParent(op, dgo);

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

        if (dgo->out->varTupleId != invalidTupleId) {
            VariableInfo & varinfo = optimizer->dgm()->getVariable(dgo->out->varTupleId);

            if (varinfo.nodes.size() != 0) {
                U_ASSERT(false);
                return false;
            };

            optimizer->dgm()->removeVariable(dgo->out);
        }

        dgo->out->varTupleId = op->tid;
        optimizer->dgm()->addVariable(dgo->out);

        MapGraph * mg = new MapGraph(op->getList(), dgo->graph().dg, dgo->children);
        mg->tupleMask.insert(op->tid);

        debug_op_replace(__PRETTY_FUNCTION__, "replace", op->oid(), mg->oid());
        pr->replaceInParent(op, mg);

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
        pr->replaceInParent(op, null_op);
        return true;
    };

    if (op->children.size() == 1)
    {
        pr->replaceInParent(op, op->children.at(0));
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
            debug_op_replace(__PRETTY_FUNCTION__, "replace", op->oid(), child->oid());
            pr->replaceInParent(op, child);
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
void detachGraph(DataGraphIndex & to, DataGraphIndex & from)
{
    to.nodes.insert(to.nodes.end(), from.nodes.begin(), from.nodes.end());
    to.out.insert(to.out.end(), from.out.begin(), from.out.end());
    to.predicates.insert(to.predicates.end(), from.predicates.begin(), from.predicates.end());
    to.rebuild();

    DataNodeList varList;
    TupleScheme::iterator it = from.outTuples.begin();

    while (it != from.outTuples.end())
    {
        DataNode * dn = new DataNode(opt::DataNode::dnExternal);
        dn->varTupleId = (*it);
        varList.push_back(dn); 
        ++it;
    };

    from.out.clear();
    from.predicates.clear();
    from.nodes.clear();

    from.out.insert(from.out.end(), varList.begin(), varList.end());
    from.nodes.insert(from.nodes.end(), varList.begin(), varList.end());

    from.rebuild();
};

static
bool rule_DataGraph_do_rewritings(PlanRewriter * pr, DataGraphOperation * op)
{
    op->graph().rebuild();
    DataGraphRewriter dgw(op->graph());

    dgw.aliasResolution();
    dgw.selfReferenceResolution();
    dgw.doPathExpansion();

    return false;
}

static
bool rule_DataGraph_try_join(PlanRewriter * pr, DataGraphOperation * op)
{
    DataGraphIndex & dgRight = static_cast<DataGraphOperation *>(op)->graph();
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
                debug_op_replace(__PRETTY_FUNCTION__, "detach", op->oid(), candidate->oid());

                /* Not replaced, proceed rewriting */
                return false; 
            } else {
                U_ASSERT(instanceof<DataGraphOperation>(op));

                if (op->out == NULL) {
                    U_ASSERT(dgRight.out.empty());
                    debug_op_replace(__PRETTY_FUNCTION__, "replace with const", op->oid(), 0);
                    pr->replaceInParent(op, new Const(tuple_cell::atomic(true)));
                } else if (op->out->varTupleId != invalidTupleId) {
                    debug_op_replace(__PRETTY_FUNCTION__, "replace with varid", op->oid(), op->out->varTupleId);
                    pr->replaceInParent(op, new VarIn(op->out->varTupleId));
                } else {
    /* We must introduce new bogus variable to propagade graph to the parent,
     * this variable should be declared in rewriter first */
                    TupleId varId = optimizer->context()->generateTupleId();
                    op->out->varTupleId = varId;
                    debug_op_replace(__PRETTY_FUNCTION__, "replace with varid", op->oid(), varId);
                    pr->replaceInParent(op, new VarIn(varId));
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

struct DataNodeTupleLookup
{
    TupleId tid;

    inline
    DataNodeTupleLookup(TupleId _tid) : tid(_tid) {};
    
    inline
    bool operator()(const DataNode * dnode) {
        return dnode->varTupleId == tid;
    };
};

static 
bool rule_redundant_MapGraph(PlanRewriter * pr, MapGraph * op)
{
    if (op->graph().predicates.empty()) {
        DataNodeList & dnl = op->graph().nodes;

        for (DataNodeList::const_iterator it = dnl.begin(); it != dnl.end(); ++it)
        {
            DataNode * n = *it;

            if (n->type != opt::DataNode::dnExternal) {
                return false;
            }

            VariableInfo & varinfo = optimizer->dgm()->getVariable(n->varTupleId);

            if (!varinfo.producer->notNull) {
                return false;
            };
        };

        optimizer->dgm()->deleteGraph(op->graph().dg);

        debug_op_replace(__PRETTY_FUNCTION__, "replace", op->oid(), op->getList()->oid());
        pr->replaceInParent(op, op->getList());

        return true;
    }

    if (rqp::instanceof<rqp::VarIn>(op->getList())) {
        rqp::VarIn * var = static_cast<rqp::VarIn *>(op->getList());

        DataNodeList::const_iterator it = std::find_if(op->graph().out.begin(), op->graph().out.end(), DataNodeTupleLookup(var->getTuple()));

        if (it == op->graph().out.end()) {
            // TODO : actually it may be replaced with IF statement
            return false;
        } else {
            DataNode * dn = *it;

            op->graph().out.clear();
            op->graph().out.push_back(dn);
            op->graph().rebuild();
            op->graph().dg->operation = new DataGraphOperation(op->graph().dg);

            /* We should delete var variable from variable table, as it is deleted! */
            optimizer->dgm()->removeVariable(var->dnode);

            debug_op_replace(__PRETTY_FUNCTION__, "replace MapGraph_to_DataGraph", op->oid(), op->graph().dg->operation->oid());
            pr->replaceInParent(op, op->graph().dg->operation);
            return true;
        };

    };

    // TODO : constant in list is replacable too

    return false;
};

static
DataGraphSet & getDataGraphSet(DataGraphSet & dgs, VariableInfo & vinfo)
{
    for (DataNodeSet::const_iterator it = vinfo.nodes.begin(); it != vinfo.nodes.end(); ++it)
    {
        dgs.insert((*it)->parent);
    };
    
    return dgs;
};

static
bool rule_MapGraph_remove_unused(PlanRewriter * pr, MapGraph * op)
{
    DataGraphMaster * dgm = optimizer->dgm();
    DataGraphIndex & dgw = op->graph();
    
    bool modified = false;

    DataNodeList::iterator it = dgw.out.begin();

    while (it != dgw.out.end())
    {
        TupleId tid = (*it)->varTupleId;

        if (op->tupleMask.find(tid) == op->tupleMask.end() && dgm->getVariable(tid).nodes.empty())
        {
            debug_op_replace(__PRETTY_FUNCTION__, "erase var", tid, 0);
            it = dgw.out.erase(it);
            modified = true;
        } else {
            ++it;
        }
    };

    if (modified) {
        dgw.rebuild();
    };

    return false;
};

static 
bool rule_push_down_variable(PlanRewriter * pr, SequenceConcat * op)
{
    DataGraphMaster * dgm = optimizer->dgm();

    if (instanceof<DataGraphOperation>(op->getSubplan())) {
        DataGraphOperation * plan = static_cast<DataGraphOperation *>(op->getSubplan());

        DataGraphSet dgs;
        getDataGraphSet(dgs, dgm->getVariable(op->tid));

        if (dgs.size() == 1) {
            DataGraph * dg = *dgs.begin();
            DataGraphOperation * rop = dynamic_cast<DataGraphOperation *>(dg->operation);
            DataNode * out = plan->graph().out.at(0);

            U_ASSERT(plan->graph().out.size() == 1);
            U_ASSERT(dg != NULL);
            U_ASSERT(rop != NULL);

            out->varTupleId = op->tid;

            // TODO : not in any possible case
            DataGraphIndex & lg = rop->graph();
            DataGraphIndex & rg = plan->graph();

            lg.nodes.insert(lg.nodes.end(), rg.nodes.begin(), rg.nodes.end());
            lg.predicates.insert(lg.predicates.end(), rg.predicates.begin(), rg.predicates.end());
            lg.rebuild();

            debug_op_replace(__PRETTY_FUNCTION__, "replace", op->oid(), op->getList()->oid());
            pr->replaceInParent(op, op->getList());
            return true;
        };
    };

    return false;
};

static
bool rule_remove_If(PlanRewriter * pr, If * op)
{
    VarIn * condition = dynamic_cast<VarIn *>(op->getCondition());

    /* Replace If expression with its then part if result of
     * the condition is known to be constant in the circumestances
     *
     * TODO : make a more complex check
     * TODO : cleanup else
     */

    if (NULL != condition)
    {
        VariableInfo & varInfo = optimizer->dgm()->getVariable(condition->getTuple());
        U_ASSERT(varInfo.producer != NULL);

        if (varInfo.producer->alwaysTrue)
        {
            optimizer->dgm()->removeVariable(condition->dnode);
            debug_op_replace(__PRETTY_FUNCTION__, "replace", op->oid(), op->getThen()->oid());
            pr->replaceInParent(op, op->getThen());
            return true;
        };
    };

    return false;
};

void PlanRewriter::do_execute()
{
#define RULE(x) do { if (x) { rule_worked = true; goto _end_label; } } while (false)
    do {
        bool rule_worked = false;
        RPBase * op = traverseStack.back();

        /* Before or instead traverse */
        switch (op->info()->opType) {
          case MapGraph::opid :
            {
              MapGraph * mg = static_cast<MapGraph *>(op);
              mg->graph().update();

              traverseChildren(OperationList(mg->children.begin(), mg->children.end() - 1));

              for (TupleScheme::const_iterator it = mg->tupleMask.begin(); it != mg->tupleMask.end(); ++it)
              {
                  optimizer->dgm()->addVariableDecl(*it, op);
              };

              traverse(mg->getList());
            }
            break;
          case SequenceConcat::opid :
          case MapConcat::opid :
            {
              NestedOperation * nop = static_cast<NestedOperation *>(op);

              traverse(nop->getSubplan());
              optimizer->dgm()->addVariableDecl(nop->tid, op);
              traverse(nop->getList());
            }
            break;
          default:
            {
              traverseChildren(op->children);
            } break;
        }

        /* After traverse */
        switch (op->info()->opType) {
          case SequenceConcat::opid :
            {
                RULE(rule_push_down_variable(this, static_cast<SequenceConcat *>(op)));
            };
          case If::opid :
            {
                RULE(rule_remove_If(this, static_cast<If *>(op)));
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
                DataGraphOperation * dgo = static_cast<DataGraphOperation *>(op);
                /* Set parent operation */
                dgo->graph().dg->operation = dgo;

                if (!instanceof<MapConcat>(getParent())) {
                    RULE(rule_DataGraph_try_join(this, static_cast<DataGraphOperation *>(op)));
                };

                if (instanceof<MapGraph>(op)) {
                    RULE(rule_redundant_MapGraph(this, static_cast<MapGraph *>(op)));
                    RULE(rule_MapGraph_remove_unused(this, static_cast<MapGraph *>(op)));
                }

                RULE(rule_DataGraph_do_rewritings(this, static_cast<DataGraphOperation *>(op)));
            };
            break;
          case VarIn::opid :
            {
                optimizer->dgm()->addVariable(static_cast<VarIn *>(op)->dnode);
/*
                if (varMap.find(static_cast<VarIn *>(op)->getTuple()) != varMap.end()) {
                    varMap.at(static_cast<VarIn *>(op)->getTuple()).used = true;
                }
*/
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
/*
        if (rule_worked) {
            root->toStream(tmpDebug);
            tmpDebug << "\n";
            tmpDebug.flush();
        };
*/
        if (traverseStack.back() == op) {
            traverseStack.pop_back();
            break;
        };
    } while (!traverseStack.empty());
}
