#ifndef _FN_HELPERS_H
#define _FN_HELPERS_H

#include "tr/opt/algebra/FunctionOperations.h"
#include "tr/opt/algebra/GraphOperations.h"
#include "tr/opt/algebra/PlanRewriter.h"

inline static
bool isGraphExpr(rqp::RPBase * op)
{
    return
      rqp::instanceof<rqp::DataGraphOperation>(op) ||
      rqp::instanceof<rqp::VarIn>(op);
};

inline static
opt::DataNode * addGraphToJoin(opt::DataGraphBuilder & builder, rqp::RPBase * op)
{
    if (rqp::instanceof<rqp::DataGraphOperation>(op))
    {
        rqp::DataGraphOperation * dgo = static_cast<rqp::DataGraphOperation *>(op);
        opt::DataGraphWrapper dgw(dgo->getGraph());

        builder.nodes.insert(builder.nodes.back(), dgw.nodes.begin(), dgw.nodes.end());
        builder.out.insert(builder.out.back(), dgw.out.begin(), dgw.out.end());
        builder.predicates.insert(builder.predicates.back(), dgw.predicates.begin(), dgw.predicates.end());

        return dgo->out;
    } else if (rqp::instanceof<rqp::VarIn>(op))
    {
        opt::DataGraphMaster master = rqp::PlanContext::current->dgm();
        opt::DataNode * node = new opt::DataNode(opt::DataNode::dnExternal);
        node->varTupleId = static_cast<rqp::VarIn *>(op)->getTuple();
        master.addVariable(node);

        builder.nodes.insert(node);
        builder.out.insert(node);

        return node;
    } else {
        U_ASSERT(false);
        return NULL;
    };   
};

inline static
rqp::OperationList & addSuboperations(rqp::OperationList & oplist, rqp::RPBase * op)
{
    if (rqp::instanceof<rqp::DataGraphOperation>(op))
    {
        rqp::DataGraphOperation * dgo = static_cast<rqp::DataGraphOperation *>(op);
        oplist.insert(oplist.end(), dgo->children.begin(), dgo->children.end());
    }

    return oplist;
};


inline static
opt::DataNode * createTrueNode()
{
    opt::DataNode * result = new opt::DataNode(opt::DataNode::dnConst);
    result->sequence = rqp::PlanContext::current->dgm()->alwaysTrueSequence;
    return result;
};

inline static
opt::DataNode * initGraph(opt::DataGraphBuilder & dgb, rqp::VarIn * invar)
{
    opt::DataNode * nodeIn = new opt::DataNode(opt::DataNode::dnExternal);
    nodeIn->varTupleId = invar->getTuple();
    dgb.nodes.push_back(nodeIn);
    return nodeIn;
};

inline static
void replaceInParent(rqp::PlanRewriter * pr, rqp::RPBase * op1, rqp::RPBase * op2)
{
    size_t sz = pr->traverseStack.size() - 1;

    if (sz > 0) {
        rqp::RPBase * parent = pr->traverseStack.at(sz - 1);
        U_ASSERT(parent != op1);
        parent->replace(op1, op2);
    }
};


#endif /* _FN_HELPERS_H */
