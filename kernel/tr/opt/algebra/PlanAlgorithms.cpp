#include "PlanAlgorithms.h"

#include "tr/opt/graphs/Predicates.h"
#include "tr/opt/graphs/GraphRewriter.h"
#include "tr/opt/functions/FnHelpers.h"
#include "tr/opt/functions/Functions.h"

#include <algorithm>
#include <sstream>

using namespace rqp;
using namespace opt;


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

/**
 * @brief Looks for declaration of given variables in 
 */
struct TreePathAnalisys
{
    PlanRewriter * rewriter;
    bool preserveNull;
    RPBase * result;

    TreePathAnalisys(PlanRewriter * pr)
      : rewriter(pr), preserveNull(true), result(NULL) {};

    bool findDeclaration(const TupleScheme & tuplesToSearch);
    bool isConditional();
};

bool TreePathAnalisys::isConditional()
{
    OperationList & backList = rewriter->traverseStack;
    OperationList::const_reverse_iterator cop = backList.rbegin();
    OperationList::const_reverse_iterator parentOp = cop + 1;

    result = NULL;

    while (parentOp != backList.rend()) {
        RPBase * parent = *parentOp;

        if (parent->result() != *cop) {
            if (::isConditional(parent, *cop)) {
                result = parent;
                return true;
            };
            return false;
        };

        cop++;
        parentOp++;
    }

    return false;
}


bool TreePathAnalisys::findDeclaration(const TupleScheme & tuplesToSearch)
{
    OperationList & backList = rewriter->traverseStack;
    OperationList::const_reverse_iterator cop = backList.rbegin();
    OperationList::const_reverse_iterator parentOp = cop + 1;

    result = NULL;
    
    while (parentOp != backList.rend()) {
        /* If dependent variable is defined on the path, we cannot   */
        if (NestedOperation * nop = dynamic_cast<NestedOperation *>(*parentOp)) {
            if (nop->getList() == *cop &&
                  tuplesToSearch.find(nop->tid) != tuplesToSearch.end())
            {
              // TODO : try to push down to declaration
                result = nop;
                return false;
            };
        };

        MapGraph * candidate = dynamic_cast<MapGraph *>(*parentOp);

        if (NULL != candidate &&
              candidate->getList() == *cop &&
              intersection_size(candidate->graph().outTuples, tuplesToSearch) > 0)
        {
            result = candidate;
            return true;
        };

        preserveNull = preserveNull && preservesNull(*parentOp, *cop);

        cop++;
        parentOp++;
    }

    return false;
}

/* Rewriting rules, based on each operation */
static
bool rule_XPathStep_to_DataGraph(PlanRewriter * pr, XPathStep * op)
{
    U_ASSERT(op->getList() != null_obj);

    if (instanceof<VarIn>(op->getList()))
    {
        VarIn * varop = static_cast<VarIn *>(op->getList());
        RPBase * result = NULL;

        scoped_ptr<DataGraphIndex> dg_builder(NULL);
        DataGraphIndex * dgi;

        TreePathAnalisys treeAnalisys(pr);
        treeAnalisys.findDeclaration(singleTupleScheme(varop->tuple()));
        MapGraph * mgraph = dynamic_cast<MapGraph *>(treeAnalisys.result);

        DataNode * nodeIn;
        DataNode * nodeOut = new DataNode(opt::DataNode::dnFreeNode);
        nodeOut->varTupleId = optimizer->context()->generateTupleId();
        optimizer->dgm()->addVariable(nodeOut);

        if (treeAnalisys.preserveNull && null_op != mgraph)
        {
            dgi = &(mgraph->graph());

            DataNodeList::const_iterator it =
                std::find_if(dgi->nodes.begin(), dgi->nodes.end(),
                    DataNodeTupleLookup(varop->tuple()));

            if (it == dgi->nodes.end()) {
                U_ASSERT(false);
            };

            nodeIn = *it;

            /* A new life for an old varop */
            optimizer->dgm()->resetVariable(varop->dnode, nodeOut->varTupleId);
            result = varop;
        } else {
            dg_builder = new DataGraphIndex(new DataGraph(optimizer->dgm()));
            dgi = dg_builder.get();

            nodeIn = varop->dnode;
            dgi->nodes.push_back(nodeIn);

            varop->dnode = new DataNode(opt::DataNode::dnExternal);
            varop->dnode->varTupleId = nodeOut->varTupleId;
            optimizer->dgm()->addVariable(varop->dnode);

            result = new MapGraph(varop, dgi->dg, TupleScheme());
        };

        Predicate * pred = new StructuralPredicate(nodeIn, nodeOut, op->getStep());

        dgi->nodes.push_back(nodeOut);
        dgi->out.push_back(nodeOut);
        dgi->predicates.push_back(pred);

        dgi->rebuild();

        debug_op_replace(__PRETTY_FUNCTION__, "replace", op->oid(), varop->oid());
        pr->replaceInParent(op, result);

        return true;
    };

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
bool rule_Graph_try_join(PlanRewriter * pr, MapGraph * op)
{
    DataGraphIndex & dgRight = static_cast<MapGraph *>(op)->graph();
    TreePathAnalisys treeAnalisys(pr);

    treeAnalisys.findDeclaration(dgRight.inTuples);

    if (NestedOperation * nestedOp = dynamic_cast<NestedOperation *>(treeAnalisys.result))
    {
        // TODO : try to push down to declaration
        return false;
    };

    if (MapGraph * nestedOp = dynamic_cast<MapGraph *>(treeAnalisys.result))
    {
        if (!treeAnalisys.preserveNull) {
            // TODO: left join
            return false;
        }

        if (isGraphExpr(op->getList())) {
            nestedOp->joinGraph(dgRight);
            pr->replaceInParent(op, op->getList());
            debug_op_replace(__PRETTY_FUNCTION__, "replace", op->oid(), op->getList()->oid());
        } else {
            detachGraph(nestedOp->graph(), dgRight);
            debug_op_replace(__PRETTY_FUNCTION__, "detach", op->oid(), nestedOp->oid());
        };

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
bool rule_pull_nested_MapGraph(PlanRewriter * pr, MapConcat * op)
{
    RPBase * subexpr = op->getSubplan();

    if (instanceof<MapGraph>(subexpr))
    {
        return do_operation_push_down(pr, op, 1);
    };

    return false;
};

static
bool rule_bind_MapConcat(PlanRewriter * pr, MapConcat * op)
{
    RPBase * subexpr = op->getSubplan();

    if (instanceof<VarIn>(subexpr))
    {
        TupleId tid = static_cast<VarIn*>(subexpr)->tuple();
        optimizer->dgm()->mergeVariables(tid, op->tid);
        optimizer->dgm()->removeVariable(static_cast<VarIn*>(subexpr)->dnode);

        pr->replaceInParent(op, op->getList());
        debug_op_replace(__PRETTY_FUNCTION__, "replace", op->oid(), op->getList()->oid());

        return true;
    };

    return false;
};

static
bool rule_post_MapConcat_to_MapGraph(PlanRewriter * pr, MapConcat * op)
{
    return false;
/*    
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
*/    
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

static
bool rule_DataGraph_do_rewritings(PlanRewriter * pr, MapGraph * op)
{
    DataGraphRewriter dgw(op->graph());

    dgw.aliasResolution();
    dgw.selfReferenceResolution();
    dgw.doPathExpansion();

    return false;
}

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

        if (op->groupBy.find(tid) == op->groupBy.end() && dgm->getVariable(tid).nodes.empty())
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

/*
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
*/

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
        VariableInfo & varInfo = optimizer->dgm()->getVariable(condition->tuple());
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

static
bool rule_conditional_function_remove(PlanRewriter * pr, RPBase * op, unsigned idx)
{
    TreePathAnalisys pathAnalisys(pr);
    if (pathAnalisys.isConditional()) {
        debug_op_replace(__PRETTY_FUNCTION__, "replace", op->oid(), op->children[idx]->oid());
        pr->replaceInParent(op, op->children[idx]);
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
        switch (op->info()->clsid) {
        CASE_TYPE_CAST(MapGraph, mapGraphOp, op)
            {
              mapGraphOp->graph().update();

              traverseChildren(OperationList(mapGraphOp->children.begin(), mapGraphOp->children.end() - 1));

              for (TupleScheme::const_iterator it = mapGraphOp->groupBy.begin(); it != mapGraphOp->groupBy.end(); ++it)
              {
                  optimizer->dgm()->addVariableDecl(*it, op);
              };

              traverse(mapGraphOp->getList());
            }
            break;
        CASE_TYPE(SequenceConcat)
        CASE_TYPE(MapConcat)
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

        switch (op->info()->clsid) {
        CASE_TYPE_CAST(SequenceConcat, typed_op, op)
            {
//                RULE(rule_push_down_variable(this, typed_op));
            };
        CASE_TYPE_CAST(If, typed_op, op)
            {
//                RULE(rule_remove_If(this, typed_op));
            };
            break;
        CASE_TYPE(Exists)
        CASE_TYPE(FalseIfNull)
            {
                RULE(rule_conditional_function_remove(this, op, 0));
            };
            break;
        CASE_TYPE_CAST(MapGraph, typed_op, op)
            {
                /* Set parent operation */
                typed_op->graph().dg->operation = typed_op;

                if (!instanceof<MapConcat>(getParent())) {
//                    RULE(rule_Graph_try_join(this, typed_op));
                };

//                RULE(rule_redundant_MapGraph(this, typed_op));
                RULE(rule_MapGraph_remove_unused(this, typed_op));

//                RULE(rule_DataGraph_do_rewritings(this, typed_op));
            };
            break;
        CASE_TYPE_CAST(VarIn, typed_op, op)
            {
                optimizer->dgm()->addVariable(typed_op->dnode);
            };
            break;
        CASE_TYPE_CAST(Sequence, typed_op, op)
            {
//                RULE(rule_delete_Singleton_Sequence(this, typed_op));
            }
            break;
        CASE_TYPE_CAST(FunCall, typed_op, op)
            {
                if (typed_op->getFunction()->finfo->rule_func != NULL) {
                    RULE(typed_op->getFunction()->finfo->rule_func(this, typed_op));
                };
            }
            break;
        CASE_TYPE_CAST(MapConcat, typed_op, op)
            {
//                RULE(rule_pull_nested_MapGraph(this, typed_op));
//                RULE(rule_bind_MapConcat(this, typed_op));
            }
            break;
        CASE_TYPE_CAST(XPathStep, typed_op, op)
            {
                RULE(do_operation_push_down(this, op, 0));

                if (instanceof<VarIn>(typed_op->getList())) {
                    RULE(rule_XPathStep_to_DataGraph(this, typed_op));
                }
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
