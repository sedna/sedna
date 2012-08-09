#ifndef _FN_HELPERS_H
#define _FN_HELPERS_H

#include "tr/opt/algebra/AllOperations.h"
#include "tr/opt/graphs/DataGraphs.h"

void cleanupFunCall(rqp::FunCallParams * funCall);

bool do_operation_push_down(rqp::RewritingContext * pr, rqp::RPBase * op, unsigned idx);
bool do_outer_bind_parameter(rqp::RewritingContext * pr, rqp::RPBase * op, unsigned idx, bool preserveNull);

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
    /* Condition of IF expression with only THEN branch */
      (instanceof<rqp::If>(parent) && static_cast<rqp::If *>(parent)->getCondition() == child &&
          parent->result() == static_cast<rqp::If *>(parent)->getThen()) ||
    /* Any branch of MapConcat */
      instanceof<rqp::MapConcat>(parent) ||
      instanceof<rqp::MapGraph>(parent);
};

inline static
bool isGraphExpr(rqp::RPBase * op)
{
    return instanceof<rqp::Const>(op) || instanceof<rqp::VarIn>(op);
};

/*
inline static
opt::DataNode * addGraphToJoin(opt::DataGraphIndex & builder, rqp::RPBase * op)
{
    opt::VariableUsageGraph * master = optimizer->dgm();
    opt::DataNode * node = NULL;

    if (instanceof<rqp::VarIn>(op)) {
        node = static_cast<rqp::VarIn *>(op)->dnode;
    } else if (instanceof<rqp::Const>(op)) {
        node = new opt::DataNode(opt::DataNode::dnConst);
        node->constValue = static_cast<rqp::Const *>(op)->getSequence();
    }

    U_ASSERT(node != NULL);

    builder.nodes.push_back(node);

    return node;
};

inline static
opt::DataNode * createTrueNode()
{
    opt::DataNode * result = new opt::DataNode(opt::DataNode::dnConst);
    result->constValue = optimizer->planContext()->variableGraph->alwaysTrueSequence;
    result->properties.flags |=  opt::variable_properties_t::sf_alwaysTrue;
    return result;
};
*/

#define REGISTER_FUNCTIONS_BEGIN(LIB) \
struct RegisterFunctions##LIB { RegisterFunctions##LIB() {

#define REGISTER_FUNCTIONS_END(LIB) \
}; }; \
static const RegisterFunctions##LIB onModuleInit##LIB;


#endif /* _FN_HELPERS_H */
