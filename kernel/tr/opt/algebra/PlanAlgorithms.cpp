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
        DataGraphWrapper dgw(dgo->getGraph());

        DataNode * node = new DataNode(opt::DataNode::dnFreeNode);

        dgw.nodes.push_back(node);
        dgw.out.clear();
        dgw.out.push_back(node);

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

        DataNode * nodeIn = initGraph(dgb, static_cast<VarIn *>(op->getList()));
        DataNode * nodeOut = new DataNode(opt::DataNode::dnFreeNode);

        dgb.nodes.push_back(nodeOut);
        dgb.out.push_back(nodeOut);

        dgb.predicates.push_back(
            new StructuralPredicate(nodeIn, nodeOut, op->getStep()));

        DataGraph * dg = dgb.build(dgm);
        dgm->addVariable(nodeIn);

        dgo = new DataGraphOperation(dg, OperationList());

        replaceInParent(pr, op, dgo);

        return true;
    };

    return false;
};

bool rule_post_MapConcat_to_MapGraph(PlanRewriter * pr, MapConcat * op)
{
    RPBase * subexpr = op->getSubplan();

    if (instanceof<DataGraphOperation>(subexpr))
    {
        DataGraphOperation * dgo = static_cast<DataGraphOperation *>(subexpr);
        DataGraphRewriter dgw(dgo->getGraph());
        MapGraph * mg = new MapGraph(op->getList(), dgo->getGraph(), dgo->children);

        DataNode * node = new DataNode(opt::DataNode::dnAlias);

        node->aliasFor = dgo->out;
        node->varTupleId = op->context.item;

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


void PlanRewriter::do_execute()
{
#define RULE(x) do { if (x) { goto _end_label; } } while (false)
    do {
        RPBase * op = traverseStack.back();

        /* Before or instead traverse */
        switch (op->info()->opType) {
          default: {
            traverseChildren(op->children);
          } break;
        }

        /* After traverse */
        switch (op->info()->opType) {
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
