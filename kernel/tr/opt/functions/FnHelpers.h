#ifndef _FN_HELPERS_H
#define _FN_HELPERS_H

#include "tr/opt/algebra/FunctionOperations.h"
#include "tr/opt/algebra/GraphOperations.h"
#include "tr/opt/algebra/PlanRewriter.h"

inline static
bool isConstExpr(rqp::RPBase * op)
{
    return op == rqp::null_op || rqp::instanceof<rqp::Const>(op);
};

inline static
bool isGraphExpr(rqp::RPBase * op)
{
    return
      rqp::instanceof<rqp::Const>(op) ||
      rqp::instanceof<rqp::DataGraphOperation>(op) ||
//      (rqp::instanceof<rqp::DataGraphOperation>(op) && static_cast<rqp::DataGraphOperation *>(op)->out != NULL) ||
      rqp::instanceof<rqp::VarIn>(op);
};


inline static
opt::DataNode * addGraphToJoin(opt::DataGraphBuilder & builder, rqp::RPBase * op)
{
    opt::DataGraphMaster * master = rqp::PlanContext::current->dgm();

    if (rqp::instanceof<rqp::DataGraphOperation>(op))
    {
        rqp::DataGraphOperation * dgo = static_cast<rqp::DataGraphOperation *>(op);
        U_ASSERT(dgo->out != NULL);
        opt::DataGraphWrapper & dgw = dgo->graph();

        builder.nodes.insert(builder.nodes.end(), dgw.nodes.begin(), dgw.nodes.end());
        builder.out.insert(builder.out.end(), dgw.out.begin(), dgw.out.end());
        builder.predicates.insert(builder.predicates.end(), dgw.predicates.begin(), dgw.predicates.end());

        return dgo->out;
    } else if (rqp::instanceof<rqp::VarIn>(op))
    {
        opt::DataNode * node = new opt::DataNode(opt::DataNode::dnExternal);
        node->varTupleId = static_cast<rqp::VarIn *>(op)->getTuple();

        builder.nodes.push_back(node);
        builder.out.push_back(node);

        return node;
    } else if (rqp::instanceof<rqp::Const>(op))
    {
        opt::DataNode * node = new opt::DataNode(opt::DataNode::dnConst);
        node->sequence = static_cast<rqp::Const *>(op)->getSequence();

        builder.nodes.push_back(node);
        builder.out.push_back(node);

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
opt::DataNode * initGraphNode(rqp::VarIn * invar)
{
    opt::DataNode * nodeIn = new opt::DataNode(opt::DataNode::dnExternal);
    nodeIn->varTupleId = invar->getTuple();
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

    pr->traverseStack.pop_back();
    pr->traverseStack.push_back(op2);
};

#define REGISTER_FUNCTIONS_BEGIN(LIB) \
struct RegisterFunctions##LIB { RegisterFunctions##LIB() {

#define REGISTER_FUNCTIONS_END(LIB) \
}; }; \
static const RegisterFunctions##LIB onModuleInit##LIB;


#endif /* _FN_HELPERS_H */
