#ifndef _FN_HELPERS_H
#define _FN_HELPERS_H

#include "tr/opt/algebra/AllOperations.h"
#include "tr/opt/graphs/DataGraphCollection.h"

inline static
bool isConstExpr(rqp::RPBase * op)
{
    return op == rqp::null_op || instanceof<rqp::Const>(op);
};

/* Conditional expressions implies EBV on result */
inline static
bool isConditional(rqp::RPBase * parent, rqp::RPBase * child)
{
    return
      (instanceof<rqp::If>(parent) && static_cast<rqp::If *>(parent)->getCondition() == child);
};

inline static
bool isResultOp(rqp::RPBase * parent, rqp::RPBase * child)
{
    return parent->result() == child;
};

/* Expression preserves null iff it returns null on null input form child */
inline static
bool preservesNull(rqp::RPBase * parent, rqp::RPBase * child)
{
    return
    /* Result passthrough */
      isResultOp(parent, child) || 
    /* Condition of IF expression with only THEN branch */
      (instanceof<rqp::If>(parent) && static_cast<rqp::If *>(parent)->getCondition() == child &&
          parent->result() == static_cast<rqp::If *>(parent)->getThen()) ||
    /* Any branch of MapConcat */
      instanceof<rqp::MapConcat>(parent);
};

inline static
bool isGraphExpr(rqp::RPBase * op)
{
    return
      instanceof<rqp::Const>(op) ||
      instanceof<rqp::DataGraphOperation>(op) ||
//      (instanceof<rqp::DataGraphOperation>(op) && static_cast<rqp::DataGraphOperation *>(op)->out != NULL) ||
      instanceof<rqp::VarIn>(op);
};

//inline static
//void 

/*
inline static
void deleteOperation(rqp::RPBase * op)
{
    switch (op->info()->opType) {
      case rqp::VarIn :
        {
            op->getContext()->dgm()->deleteGraph(static_cast<rqp::VarIn *>(op)->dnode->parent);
        }; break;
      case rqp::DataGraphOperation :
        {
            op->getContext()->dgm()->deleteGraph(static_cast<rqp::DataGraphOperation *>(op)->graph().dg);
        }; break;
      case rqp::MapGraph :
        U_ASSERT(false);
      default :
        break;      
    };
};
*/

inline static
opt::DataNode * initGraphNode(rqp::VarIn * invar)
{
    return invar->dnode;
};

template<typename BuilderType>
inline static
opt::DataNode * addGraphToJoin(BuilderType & builder, rqp::RPBase * op)
{
    opt::DataGraphMaster * master = optimizer->dgm();

    if (instanceof<rqp::DataGraphOperation>(op))
    {
        rqp::DataGraphOperation * dgo = static_cast<rqp::DataGraphOperation *>(op);
        U_ASSERT(dgo->out != NULL);
        opt::DataGraphIndex & dgw = dgo->graph();

        builder.nodes.insert(builder.nodes.end(), dgw.nodes.begin(), dgw.nodes.end());
        builder.out.insert(builder.out.end(), dgw.out.begin(), dgw.out.end());
        builder.predicates.insert(builder.predicates.end(), dgw.predicates.begin(), dgw.predicates.end());

        return dgo->out;
    } else if (instanceof<rqp::VarIn>(op))
    {
        opt::DataNode * node = initGraphNode(static_cast<rqp::VarIn *>(op));

        builder.nodes.push_back(node);
        builder.out.push_back(node);

        return node;
    } else if (instanceof<rqp::Const>(op))
    {
        opt::DataNode * node = new opt::DataNode(opt::DataNode::dnConst);
        node->constValue = static_cast<rqp::Const *>(op)->getSequence();

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
    if (instanceof<rqp::DataGraphOperation>(op))
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
    result->constValue = optimizer->dgm()->alwaysTrueSequence;
    result->alwaysTrue = true;
    return result;
};

#define REGISTER_FUNCTIONS_BEGIN(LIB) \
struct RegisterFunctions##LIB { RegisterFunctions##LIB() {

#define REGISTER_FUNCTIONS_END(LIB) \
}; }; \
static const RegisterFunctions##LIB onModuleInit##LIB;


#endif /* _FN_HELPERS_H */
