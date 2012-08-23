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

#define IF_IS(OP, TYPE, VARNAME) if (TYPE * VARNAME = dynamic_cast<TYPE *>(OP))
#define RULE_LOCALS \
    VariableUsageGraph * varGraph = &op->getContext()->varGraph;

static
bool replaceTupleInOperation(RPBase * op, TupleId alias, TupleId master)
{
    RULE_LOCALS

    if (VarIn * varIn = dynamic_cast<VarIn *>(op)) {
        U_ASSERT(varIn->variable == alias);
        varIn->variable = master;
        varGraph->addVariableUsage(master, varIn);
    } else if (FunCallParams * funCall = dynamic_cast<FunCallParams *>(op)) {
        for (ParamList::iterator it = funCall->paramList.begin(); it != funCall->paramList.end(); ++it) {
            if (*it == alias) {
                *it = master;
                varGraph->addVariableUsage(master, funCall);
            };
        }
    } else if (MapGraph * mgraph = dynamic_cast<MapGraph *>(op)) {
        DataGraphIndex & graph = mgraph->graph();
      
        for (DataNodeList::iterator it = graph.nodes.begin(); it != graph.nodes.end(); ++it) {
            DataNode * dn = *it;

            if (dn->varTupleId == alias) {
                DataNodeIndex & dni = graph.nodeIndex[dn->index];
                
                if (dni.input || dni.output) {
                    dn->varTupleId = master;
                    varGraph->addVariableDataNode(dn);
                } else {
                    varGraph->removeVariableDataNode(dn);
                    dn->varTupleId = master;
                };
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
    traverseStack.push_back(root);
    do_execute();
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

static
bool map_to_graph(RewritingContext * pr, MapConcat * op)
{
    RULE_LOCALS

    IF_IS(op->getSubplan(), MapGraph, graph) 
    IF_IS(graph->getList(), VarIn, graphValue)
    {
        TupleInfo & alias = varGraph->getVariable(graphValue->getTuple());

        if (alias.definedIn != graph) {
            return false;
        };

        TupleInfo & master = varGraph->getVariable(op->getTuple());

        /* Replace tuple id in nested graph */
        replaceTupleInOperation(graph, alias.id, master.id);
        /* This should automatically replace definition pointer */

        /* Copy all statistics */
        master.properties = alias.properties;
        master.properties.flags |= tuple_info_t::sf_singleton;

        alias.undefine(master.id);

        graph->setList(op->getList());
        graph->groupBy.clear();
        graph->groupBy.insert(master.id);

        pr->replaceInParent(op, graph);

        return true;
    };
    
    return false;
};

static
bool join_mapgraph(RewritingContext * pr, MapGraph * op)
{
    RULE_LOCALS

    IF_IS(op->getList(), MapGraph, graph)
    {
        TupleScheme myOut, childIn;

        op->graph().tuplesInOut(NULL, &myOut);
        graph->graph().tuplesInOut(&childIn, NULL);

        if (std::intersection_size(myOut, childIn) > 0) {
            op->joinGraph(graph->graph());
            op->groupBy.insert(graph->groupBy.begin(), graph->groupBy.end());
            op->setList(graph->getList());

            for (TupleScheme::const_iterator it = childIn.begin(); it != childIn.end(); ++it) {
                varGraph->getVariable(*it).operations.erase(graph);
            };
        }
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
    VariableUsageGraph * varGraph = &op->getContext()->varGraph;

    /* Try to join input variables */
    for (DataNodeList::iterator it = dgi.in.begin(); it != dgi.in.end(); ) {
        DataNode * node = *it;
        TupleInfo & info = varGraph->getVariable((*it)->varTupleId);
        RPBase * definedIn = info.definedIn;

        if (SequenceConcat * definition = dynamic_cast<SequenceConcat *>(definedIn)) {
            /* This is a condition for plan push down */
            if (isGraphExpr(definition->getSubplan()) && (info.operations.size() == 1)) {
                RPBase * definitionBody = definition->getSubplan();

                varGraph->getVariable(node->varTupleId).operations.erase(op);

                switch (definitionBody->info()->clsid) {
                    CASE_TYPE_CAST(VarIn, varin, definitionBody) {
                        node->varTupleId = varin->getTuple();
                    } break;
                    CASE_TYPE_CAST(Const, cnst, definitionBody) {
                        node->varTupleId = invalidTupleId;
                        node->type = opt::DataNode::dnConst;
                        node->constValue = cnst->getSequence();
                    } break;
                    CASE_TYPE_CAST(MapGraph, graph, definitionBody) {
                        U_ASSERT(instanceof<VarIn>(graph->getList()));
                        rqp::VarIn * varin = static_cast<rqp::VarIn *>(graph->getList());
                        node->varTupleId = varin->getTuple();

                        dgi.predicates.insert(dgi.predicates.end(), graph->graph().predicates.begin(), graph->graph().predicates.end());
                        dgi.nodes.insert(dgi.nodes.end(), graph->graph().nodes.begin(), graph->graph().nodes.end());

                        for (DataNodeList::iterator it = graph->graph().nodes.begin(); it != graph->graph().nodes.end(); ++it) {
                            if ((*it)->type == DataNode::dnExternal) {
                                varGraph->addVariableDataNode(*it);
                            };
                        }
                    }
                }
            }
        }

        if (node->varTupleId == invalidTupleId) {
            it = dgi.in.erase(it);
        } else {
            ++it;
        }
    }

    dgi.rebuild();

    DataGraphRewriter dgw(dgi);

    dgw.constResolution();
    dgw.aliasResolution();
    dgw.selfReferenceResolution();

    /* Analize output variables usage. If not used, get rid of them. */
    for (DataNodeList::iterator it = dgi.out.begin(); it != dgi.out.end(); ) {
        TupleInfo & info = varGraph->getVariable((*it)->varTupleId);

        if (info.operations.empty() && op->groupBy.count(info.id) == 0) {
            (*it)->varTupleId = invalidTupleId;
            info.definedIn = null_op;
            it = dgi.out.erase(it);
        } else {
            ++it;
        }
    }
    dgi.rebuild();

    dgw.staticFndoc();
    dgw.doPathExpansion();

    return false;
}

static
bool remove_redundant_maps(RewritingContext * pr, MapConcat * op)
{
    RULE_LOCALS

    IF_IS(op->getSubplan(), VarIn, varin)
    {
        TupleInfo & master = varGraph->getVariable(varin->getTuple());
        TupleInfo & alias = varGraph->getVariable(op->getTuple());

        /* Clear alias data */
        alias.definedIn = null_op;
        /* Replace id in all operations */
        replaceTupleInSet(alias.operations, alias.id, master.id);
        /* Variable will be deleted */
        master.operations.erase(varin);

        if (master.properties.singleton()) {
            pr->replaceInParent(op, op->getList());
        } else {
            pr->replaceInParent(op, new GroupBy(op->getList(), master.id));
        };

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
        pr->deleteOp(bind->getSubplan());
        return true;
    };

    return false;
};

static
bool remove_nested_group_by(RewritingContext * pr, GroupBy * groupBy)
{
    TupleInfo &info = groupBy->getContext()->varGraph.getVariable(groupBy->getTuple());

    if (info.properties.singleton()) {
        pr->replaceInParent(groupBy, groupBy->getList());
        info.operations.erase(groupBy);

        return true;
    };

    return false;
};

static
bool group_by_to_graph(RewritingContext * pr, GroupBy * groupBy)
{
    TupleInfo &info = groupBy->getContext()->varGraph.getVariable(groupBy->getTuple());

    if (info.definedIn == pr->getParent() && instanceof<MapGraph>(info.definedIn)) {
        static_cast<MapGraph *>(info.definedIn)->groupBy.insert(info.id);

        pr->replaceInParent(groupBy, groupBy->getList());
        info.operations.erase(groupBy);

        return true;
    };

    if (instanceof<MapGraph>(groupBy->getList()) && info.operations.count(groupBy->getList()) == 1) {
        static_cast<MapGraph *>(groupBy->getList())->groupBy.insert(info.id);

        pr->replaceInParent(groupBy, groupBy->getList());
        info.operations.erase(groupBy);

        return true;
    };

    return false;
};


static
bool remove_redundant_group_by(RewritingContext * pr, GroupBy * groupBy)
{
    if (instanceof<VarIn>(groupBy->getList())) {
        pr->replaceInParent(groupBy, groupBy->getList());
        groupBy->getContext()->varGraph.getVariable(groupBy->getTuple()).operations.erase(groupBy);
        return true;
    };

    return false;
};

static
bool remove_redundant_group_in_graph(RewritingContext * pr, MapGraph * mapGraph)
{
    if (instanceof<VarIn>(mapGraph->getList())) {
        mapGraph->groupBy.clear();
    };

    return false;
};

static
bool push_down_if(RewritingContext * pr, If * ifexpr)
{
    if (ifexpr->getElse() == null_op) {
        if (MapGraph * mapGraphChild = dynamic_cast<MapGraph *>(ifexpr->getCondition())) {
            if (mapGraphChild->groupBy.empty()) {
                ifexpr->setCondition(mapGraphChild->getList());
                mapGraphChild->setList(ifexpr);
                pr->replaceInParent(ifexpr, mapGraphChild);
                return true;
            };
        };
    };

    return false;
};

static
bool remove_constant_if(RewritingContext * pr, If * ifexpr)
{
    if (ifexpr->getElse() == null_op) {
        if (Const * value = dynamic_cast<Const *>(ifexpr->getCondition())) {
            pr->replaceInParent(ifexpr, ifexpr->getThen());
            return true;
        } else if (VarIn * value = dynamic_cast<VarIn *>(ifexpr->getCondition())) {
            /* Case, when condition is always non-null node sequence */
            TupleInfo & info = value->getContext()->varGraph.getVariable(value->getTuple());

            if (staticallyTrue(info)) {
                pr->deleteOp(ifexpr->getCondition());
                pr->replaceInParent(ifexpr, ifexpr->getThen());
                return true;
            };
        };
    };

    return false;
};

static
bool sequence_const_to_const_sequence(RewritingContext * pr, Sequence * seq)
{
    // TODO implement true sequence of const

    for (OperationList::iterator it = seq->children.begin(); it != seq->children.end(); ++it) {
        if (!isConstExpr(*it)) {
            return false;
        };
    };

    MemoryTupleSequencePtr constSequence(new MemoryTupleSequence());

    for (OperationList::iterator it = seq->children.begin(); it != seq->children.end(); ++it) {
        rqp::Const * op = static_cast<rqp::Const *>(*it);

        if (op != null_op) {
            constSequence->insert(constSequence->end(), op->getSequence()->begin(), op->getSequence()->end());
        };
    };

    pr->deleteOp(seq);
    pr->replaceInParent(seq, new rqp::Const(constSequence));

    return true;
};

/*
static
bool make_bind_into_graph(RewritingContext * pr, SequenceConcat * bind)
{
    if (MapGraph * mapGraph = bind->getSubplan()) {
        if (VarIn * varChild = mapGraph->getList()) {
            mapGraph->setList(bind->getList());
            pr->deleteOp(varChild);
            pr->replaceInParent(bind, bind->getSubplan());
            return true;
        };
    };
};
*/

void RewritingContext::do_execute()
{
#define RULE(x) do { if (x) { rule_worked = true; goto _end_label; } } while (false)
    do {
        bool rule_worked = false;
        RPBase * op = traverseStack.back();

        /* Before or instead traverse */
        switch (op->info()->clsid) {
          CASE_TYPE_CAST(GroupBy, typed_op, op)
            {
                RULE(remove_nested_group_by(this, typed_op));

                TupleInfo & tinfo = typed_op->getContext()->varGraph.getVariable(typed_op->getTuple());
                typed_op->tmp_tinfo = tinfo.properties;
                tinfo.properties.flags |= tuple_info_t::sf_singleton;
            }
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
            };
            break;
        CASE_TYPE_CAST(If, typed_op, op)
            {
                RULE(push_down_if(this, typed_op));
                RULE(remove_constant_if(this, typed_op));
            };
            break;
        CASE_TYPE_CAST(GroupBy, typed_op, op)
            {
                TupleInfo & tinfo = typed_op->getContext()->varGraph.getVariable(typed_op->getTuple());
                tinfo.properties = typed_op->tmp_tinfo;

                RULE(remove_redundant_group_by(this, typed_op));
                RULE(group_by_to_graph(this, typed_op));
            };
            break;
        CASE_TYPE_CAST(MapGraph, typed_op, op)
            {
                RULE(join_mapgraph(this, typed_op));
                RULE(remove_redundant_group_in_graph(this, typed_op));
                RULE(datagraph_rewritings(this, typed_op));
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
                RULE(map_to_graph(this, typed_op));
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

void RewritingContext::deleteOp(RPBase* op)
{
    for (rqp::OperationList::const_iterator it = op->children.begin(); it != op->children.end(); ++it) {
        deleteOp(*it);
    };

    VariableUsageGraph * varGraph = &op->getContext()->varGraph;
    
    switch (op->info()->clsid) {
    CASE_TYPE_CAST(SequenceConcat, typed_op, op) {
            varGraph->getVariable(typed_op->getTuple()).definedIn = NULL;
        }; break;
    CASE_TYPE_CAST(GroupBy, typed_op, op) {
            varGraph->getVariable(typed_op->getTuple()).operations.erase(op);
        }; break;
    CASE_TYPE_CAST(VarIn, typed_op, op) {
            varGraph->getVariable(typed_op->getTuple()).operations.erase(op);
        }; break;
    CASE_TYPE_CAST(MapGraph, typed_op, op) {
        DataGraphIndex & graph = typed_op->graph();

        for (DataNodeList::iterator it = graph.out.begin(); it != graph.out.end(); ++it) {
            U_ASSERT((*it)->varTupleId != invalidTupleId);
            varGraph->getVariable((*it)->varTupleId).definedIn = null_op;
        };

        for (DataNodeList::iterator it = graph.nodes.begin(); it != graph.nodes.end(); ++it) {
            if ((*it)->varTupleId != invalidTupleId) {
                varGraph->getVariable((*it)->varTupleId).operations.erase(typed_op);
            }
        };
    }; break;
    CASE_TYPE_CAST(MapConcat, typed_op, op) {
            // TODO : remove all
        }; break;
    CASE_TYPE_CAST(FunCallParams, typed_op, op) {
            for (ParamList::iterator it = typed_op->paramList.begin(); it != typed_op->paramList.end(); ++it) {
                varGraph->getVariable(*it).operations.erase(op);
            }
        }; break;
    default : break;
    };
}


