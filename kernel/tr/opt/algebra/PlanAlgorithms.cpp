#include "PlanAlgorithms.h"

#include "tr/opt/graphs/Predicates.h"
#include "tr/opt/graphs/GraphRewriter.h"
#include "tr/opt/functions/FnHelpers.h"
#include "tr/opt/functions/Functions.h"

#include "tr/models/StlFixes.h"

#include <algorithm>
#include <sstream>

using namespace rqp;
using namespace opt;

static
bool replaceTupleInOperation(RPBase * base, TupleId alias, TupleId master)
{
    if (VarIn * varIn = dynamic_cast<VarIn *>(base)) {
        U_ASSERT(varIn->variable == alias);
        varIn->variable = master;
        base->getContext()->varGraph.addVariableUsage(master, varIn);
    } else if (FunCallParams * funCall = dynamic_cast<FunCallParams *>(base)) {
        for (ParamList::iterator it = funCall->paramList.begin(); it != funCall->paramList.end(); ++it) {
            if (*it == alias) {
                *it = master;
                base->getContext()->varGraph.addVariableUsage(master, funCall);
            };
        }
    } else if (MapGraph * mgraph = dynamic_cast<MapGraph *>(base)) {
        DataGraphIndex & graph = mgraph->graph();
      
        for (DataNodeList::iterator it = graph.nodes.begin(); it != graph.nodes.end(); ++it) {
            if ((*it)->varTupleId == alias) {
                (*it)->varTupleId = master;
                base->getContext()->varGraph.addVariableDataNode(*it);
            };
        }

        graph.update();

        if (mgraph->groupBy.count(alias) > 0) {
            mgraph->groupBy.erase(alias);
            mgraph->groupBy.insert(master);
        };
    }

    return false;
};

static
void replaceTupleInSet(OperationSet & opset, TupleId alias, TupleId master)
{
    for (OperationSet::iterator it = opset.begin(); it != opset.end(); ++it) {
        replaceTupleInOperation(*it, alias, master);
    };

    opset.clear();
};

void VarGraphRewriting::execute()
{
    VariableUsageGraph * varGraph = &optimizer->planContext()->varGraph;

    for (TupleInfoMap::iterator it = varGraph->variableMap.begin(); it != varGraph->variableMap.end(); ++it) {
        RPBase * def = it->second.definedIn;

        if (instanceof<SequenceConcat>(def) &&
              instanceof<VarIn>(static_cast<SequenceConcat*>(def)->getSubplan()))
        {
            TupleId tid = static_cast<VarIn *>(static_cast<SequenceConcat*>(def)->getSubplan())->getTuple();
            replaceTupleInSet(it->second.operations, it->first, tid);
        };
    };
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
bool RewritingContext::__debug_trace_replace(uint op1, uint op2)
{
    std::stringstream str;
    str << "replace " << op1 << " " << op2 << "\n";
    debug_string("opt.rule", str.str());
    return true;
}
*/

void RewritingContext::execute()
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
                  tuplesToSearch.find(nop->getTuple()) != tuplesToSearch.end())
            {
                result = nop;
                return false;
            };
        };

        MapGraph * candidate = dynamic_cast<MapGraph *>(*parentOp);

        if (NULL != candidate) {
            TupleScheme candidateOut;
            candidate->graph().tuplesInOut(NULL, &candidateOut);

            if (std::intersection_size(candidateOut, tuplesToSearch) > 0) {
                result = candidate;
                return true;
            };
        }

        preserveNull = preserveNull && preservesNull(*parentOp, *cop);

        cop++;
        parentOp++;
    }

    return false;
}



/* Rewriting rules, based on each operation 
static
bool rule_XPathStep_to_DataGraph(RewritingContext * pr, XPathStep * op)
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
        nodeOut->varTupleId = optimizer->planContext()->generateTupleId();
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

            optimizer->dgm()->resetVariable(varop->dnode, nodeOut->varTupleId);
            result = varop;
        } else {
            dg_builder = new DataGraphIndex(new DataGraph(optimizer->dgm()));
            dgi = dg_builder.get();

            nodeIn = varop->dnode;
            dgi->nodes.push_back(nodeIn);

            varop->setDataNode(nodeOut->varTupleId);

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
*/

/*
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
*/
/*
static
bool rule_Graph_try_join(RewritingContext * pr, MapGraph * op)
{
    DataGraphIndex & dgRight = static_cast<MapGraph *>(op)->graph();
    TupleScheme inTuples;
    TreePathAnalisys treeAnalisys(pr);

    dgRight.tuplesInOut(&inTuples, NULL);
    treeAnalisys.findDeclaration(inTuples);

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
        } /* else {
            detachGraph(nestedOp->graph(), dgRight);
            debug_op_replace(__PRETTY_FUNCTION__, "detach", op->oid(), nestedOp->oid());
        }; 

        return true;
    };

    return false;
};
*/

/*
static
void check_post_MapGraph(DataNodeList &outs)
{
    for (DataNodeList::const_iterator it = outs.begin(); it != outs.end(); ++it)
    {
        U_ASSERT((*it)->varTupleId != invalidTupleId);
    };
}

static
bool rule_pull_nested_MapGraph(RewritingContext * pr, MapConcat * op)
{
    RPBase * subexpr = op->getSubplan();

    if (instanceof<MapGraph>(subexpr))
    {
        return do_operation_push_down(pr, op, 1);
    };

    return false;
};
*/
/*
static
bool rule_bind_MapConcat(RewritingContext * pr, MapConcat * op)
{
    RPBase * subexpr = op->getSubplan();

    if (instanceof<VarIn>(subexpr))
    {
        TupleId tid = static_cast<VarIn*>(subexpr)->tuple();

        optimizer->dgm()->removeVariable(op->dnode);
        optimizer->dgm()->mergeVariables(tid, op->tuple());

        DataGraphIndex joinBuilder(new DataGraph(optimizer->dgm()));
        DataNode * result = static_cast<VarIn*>(subexpr)->dnode;

        joinBuilder.addOutNode(result);
        joinBuilder.rebuild();

        pr->replaceInParent(op,
            new MapGraph(op->getList(), joinBuilder.dg,
                singleTupleScheme(tid)));

        debug_op_replace(__PRETTY_FUNCTION__, "replace", op->oid(), op->getList()->oid());

        return true;
    };

    return false;
};
*/

static
bool map_eat_graph(RewritingContext * pr, MapConcat * op)
{
    MapGraph * nestedGraph = dynamic_cast<MapGraph *>(op->getSubplan());

    if (NULL != nestedGraph && instanceof<VarIn>(nestedGraph->getList()))
    {
        VarIn * varin = static_cast<VarIn *>(nestedGraph->getList());

        TupleId alias = varin->getTuple();
        TupleId master = op->getTuple();

        /* Replace tuple id in nested graph */
        op->getContext()->varGraph.getVariable(master).definedIn = null_op;
        replaceTupleInOperation(nestedGraph, alias, master);
        /* This should automatically replace definition pointer */

        /* Cleanup variable info */
        TupleInfo & info = op->getContext()->varGraph.getVariable(alias);
        info.definedIn = null_op;
        info.operations.clear();
        info.pointsTo = master;

        /* Change subexpression of the nested graph */
        nestedGraph->children[0] = op->getList();

        nestedGraph->groupBy.clear();
        nestedGraph->groupBy.insert(master);

        pr->replaceInParent(op, nestedGraph);

        return true;
    }

    return false;
};

static
bool delete_singleton_sequence(RewritingContext * pr, Sequence * op)
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
bool datagraph_rewritings(RewritingContext * pr, MapGraph * op)
{
    DataGraphIndex & dgi = op->graph();

    for (DataNodeList::iterator it = dgi.out.begin(); it != dgi.out.end(); ) {
        TupleInfo & info = op->getContext()->varGraph.getVariable((*it)->varTupleId);

        if (info.operations.empty() && op->groupBy.count(info.id) == 0) {
            (*it)->varTupleId = invalidTupleId;
            info.definedIn = null_op;
            it = dgi.out.erase(it);
        } else {
            ++it;
        }
    }

    dgi.rebuild();

    DataGraphRewriter dgw(dgi);

    dgw.constResolution();
    dgw.aliasResolution();
    dgw.selfReferenceResolution();
    dgw.doPathExpansion();

    return false;
}

/*
static 
bool rule_redundant_MapGraph(RewritingContext * pr, MapGraph * op)
{
    if (op->graph().predicates.empty()) {
        DataNodeList & dnl = op->graph().nodes;

        for (DataNodeList::const_iterator it = dnl.begin(); it != dnl.end(); ++it)
        {
            DataNode * n = *it;

            if (n->type != opt::DataNode::dnExternal) {
                return false;
            }

            TupleInfo & varinfo = optimizer->dgm()->getVariable(n->varTupleId);

            if (!varinfo.producer->properties.notnull()) {
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
*/
/*
static
DataGraphSet & getDataGraphSet(DataGraphSet & dgs, TupleInfo & vinfo)
{
    for (DataNodeSet::const_iterator it = vinfo.nodes.begin(); it != vinfo.nodes.end(); ++it)
    {
        dgs.insert((*it)->parent);
    };

    return dgs;
};
*/

/*
static
bool rule_MapGraph_remove_unused(RewritingContext * pr, MapGraph * op)
{
    VariableUsageGraph * dgm = optimizer->dgm();
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
*/

/*
static 
bool rule_push_down_variable(RewritingContext * pr, SequenceConcat * op)
{
//    VariableUsageGraph * dgm = optimizer->dgm();

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

    return false;
};
*/

/*
static
bool rule_remove_If(RewritingContext * pr, If * op)
{
    VarIn * condition = dynamic_cast<VarIn *>(op->getCondition());

    /* Replace If expression with its then part if result of
     * the condition is known to be constant in the circumestances
     *
     * TODO : make a more complex check
     * TODO : cleanup else
     /

    if (NULL != condition)
    {
        TupleInfo & varInfo = optimizer->dgm()->getVariable(condition->tuple());
        U_ASSERT(varInfo.producer != NULL);

        if (varInfo.producer->properties.alwaysTrue() &&
          varInfo.producer->properties.notnull())
        {
            optimizer->dgm()->removeVariable(condition->dnode);
            debug_op_replace(__PRETTY_FUNCTION__, "replace", op->oid(), op->getThen()->oid());
            pr->replaceInParent(op, op->getThen());
            return true;
        };
    };

    return false;
};
*/


/*
static
bool rule_conditional_function_remove(RewritingContext * pr, RPBase * op, unsigned idx)
{
    TreePathAnalisys pathAnalisys(pr);

    if (pathAnalisys.isConditional()) {
        debug_op_replace(__PRETTY_FUNCTION__, "replace", op->oid(), op->children[idx]->oid());
        pr->replaceInParent(op, op->children[idx]);
    };

    return false;
};
*/
/*
static
bool shorten_fun_calls(RewritingContext * pr, FunCallParams * funCall)
{
    VariableUsageGraph * varGraph = &optimizer->planContext()->varGraph;

    for (ParamList::iterator it = funCall->paramList.begin(); it != funCall->paramList.end(); ++it) {
        TupleInfo & info = varGraph->getVariable(*it);

        if (instanceof<SequenceConcat>(info.definedIn)) {
            VarIn * value = dynamic_cast<VarIn*>(
              static_cast<SequenceConcat *>(info.definedIn)->getSubplan());

            if (value != null_op) {
                TupleId oldtid = *it;

                *it = value->getTuple();
            };
        };
    };

    return false;
};
*/

static
bool remove_redundant_maps(RewritingContext * pr, MapConcat * map)
{
    if (instanceof<VarIn>(map->getSubplan())) {
        VarIn * varin = static_cast<VarIn *>(map->getSubplan());

        TupleInfo & master = map->getContext()->varGraph.getVariable(varin->getTuple());

        if (!master.properties.singleton()) {
            return false;
        };

        TupleInfo & alias = map->getContext()->varGraph.getVariable(map->getTuple());

        /* Clear alias data */
        alias.definedIn = null_op;

        /* Replace id in all operations */
        replaceTupleInSet(alias.operations, alias.id, master.id);
        
        pr->replaceInParent(map, map->getList());

        /* Variable is now deleted */
        master.operations.erase(varin);

        return true;
    };
    
    return false;
};

static
bool remove_unused_sequences(RewritingContext * pr, SequenceConcat * bind)
{
    TupleInfo &info = bind->getContext()->varGraph.getVariable(bind->getTuple());

    if (instanceof<VarIn>(bind->getSubplan())) {
        VarIn * varin = static_cast<VarIn *>(bind->getSubplan());
        replaceTupleInSet(info.operations, bind->getTuple(), varin->getTuple());
    }

    if (info.operations.empty()) {
        pr->replaceInParent(bind, bind->getList());
        info.definedIn = null_op;
        return true;
    };

    return false;
};


void RewritingContext::do_execute()
{
#define RULE(x) do { if (x) { rule_worked = true; goto _end_label; } } while (false)
    do {
        bool rule_worked = false;
        RPBase * op = traverseStack.back();

        /* Before or instead traverse */
        switch (op->info()->clsid) {
          default:
            {
              traverseChildren(op->children);
            } break;
        }

        /* After traverse */

        switch (op->info()->clsid) {
        CASE_TYPE_CAST(SequenceConcat, typed_op, op)
            {
                RULE(remove_unused_sequences(this, typed_op));
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
//                RULE(rule_conditional_function_remove(this, op, 0));
            };
            break;
        CASE_TYPE_CAST(MapGraph, typed_op, op)
            {
                /* Set parent operation */
//                typed_op->graph().dg->operation = typed_op;

//                RULE(rule_Graph_try_join(this, typed_op));

//                RULE(rule_redundant_MapGraph(this, typed_op));
//                RULE(rule_MapGraph_remove_unused(this, typed_op));
                RULE(datagraph_rewritings(this, typed_op));
            };
            break;
        CASE_TYPE_CAST(VarIn, typed_op, op)
            {
//                optimizer->dgm()->addVariable(typed_op->dnode);
            };
            break;
        CASE_TYPE_CAST(Sequence, typed_op, op)
            {
                RULE(delete_singleton_sequence(this, typed_op));
            }
            break;
        CASE_TYPE_CAST(FunCallParams, typed_op, op)
            {
                RULE(typed_op->getFunction()->transform(typed_op, this));
            }
            break;
        CASE_TYPE_CAST(MapConcat, typed_op, op)
            {
                RULE(remove_redundant_maps(this, typed_op));
                RULE(map_eat_graph(this, typed_op));
//                RULE(rule_pull_nested_MapGraph(this, typed_op));
            }
            break;
        CASE_TYPE_CAST(XPathStep, typed_op, op)
            {
//                RULE(do_operation_push_down(this, op, 0));

//                if (instanceof<VarIn>(typed_op->getList())) {
//                    RULE(rule_XPathStep_to_DataGraph(this, typed_op));
//                }
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


