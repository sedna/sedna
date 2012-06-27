#include "PlanAlgorithms.h"

#include "tr/opt/graphs/Predicates.h"
#include "tr/opt/graphs/GraphRewriter.h"
#include "tr/opt/functions/FnHelpers.h"

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
        pr->traverseStack.pop_back();

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
        pr->traverseStack.pop_back();

        return true;
    };

    return false;
};

bool rule_post_MapConcat_to_MapGraph(PlanRewriter * pr, MapConcat * op)
{
    if (instanceof<DataGraphOperation>(op->getSubplan()))
    {
        DataGraphOperation * dgo = static_cast<DataGraphOperation *>(op->getList());
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

        pr->traverseStack.pop_back();
        pr->traverseStack.push_back(mg);
        pr->do_execute();

        return true;
    }

    return false;
};

void PlanRewriter::do_execute()
{
    RPBase * op = traverseStack.back();

    switch (op->info()->opType) {
/*
        case rqp::VarIn::opid : {
            VarIn * xop = static_cast<VarIn *>(op);
            varMap[xop->getTuple()].used = true;
        } break;
*/
      case FunCall::opid :
        {
            FunCall * fun = static_cast<FunCall *>(op);
            traverseChildren(op->children);
            if (fun->getFunction()->)
                { return; };
        } break;

      case MapConcat::opid :
        {
            traverseChildren(op->children);
            if (rule_post_MapConcat_to_MapGraph(this, static_cast<MapConcat *>(op)))
                { return; };
        } break;

      case XPathStep::opid :
        {
            traverseChildren(op->children);
            if (rule_post_XPathStep_to_DataGraph(this, static_cast<XPathStep *>(op)))
                { return; };
        } break;

        default :
          traverseChildren(op->children);
    };

    traverseStack.pop_back();
}
