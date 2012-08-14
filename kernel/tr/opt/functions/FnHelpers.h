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
bool staticallyTrue(opt::TupleInfo & info)
{
    // TODO : implement static check for boolean value
    return info.properties.notNull() &&
      (info.properties.alwaysTrue() || info.properties.nodes());
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
bool isTrueGraphExpr(rqp::RPBase * op)
{
    if (rqp::MapGraph * graph = dynamic_cast<rqp::MapGraph * >(op)) {
        if (rqp::VarIn * varin = dynamic_cast<rqp::VarIn *>(graph->getList())) {
            return op->getContext()->varGraph.getVariable(varin->getTuple()).definedIn == op;
        };
    };

    return false;
};

inline static
bool isGraphExpr(rqp::RPBase * op)
{
    return instanceof<rqp::Const>(op) || instanceof<rqp::VarIn>(op) || isTrueGraphExpr(op);
};

/*
inline static
void addGraphToJoin(opt::DataGraphIndex & builder, rqp::RPBase * op)
{
    opt::VariableUsageGraph * master = op->getContext()->varGraph;

    if (rqp::MapGraph * graph = dynamic_cast<rqp::MapGraph *>(op)) {
        builder.nodes.insert(builder.nodes.end(), graph->graph().nodes.begin(), graph->graph().nodes.end());
        builder.predicates.insert(builder.predicates.end(), graph->graph().predicates.begin(), graph->graph().predicates.end());
    } else if (rqp::VarIn * varin = dynamic_cast<rqp::VarIn *>(op)) {
        builder.nodes.push_back(new opt::DataNode(opt::DataNode::dnExternal, varin->getTuple()));
    } else if (rqp::Const * cnst = dynamic_cast<rqp::Const *>(op)) {
        opt::DataNode * node = new opt::DataNode(opt::DataNode::dnConst, varin->getTuple());
        builder.nodes.push_back(node);
        node->constValue = cnst->getSequence();
    }

    builder.rebuild();
};
*/

inline static
opt::DataNode * createTrueNode()
{
    opt::DataNode * result = new opt::DataNode(opt::DataNode::dnConst);
    result->constValue = optimizer->planContext()->varGraph.alwaysTrueSequence;
    result->properties.flags |=  opt::tuple_info_t::sf_alwaysTrue;
    return result;
};

#define REGISTER_FUNCTIONS_BEGIN(LIB) \
struct RegisterFunctions##LIB { RegisterFunctions##LIB() {

#define REGISTER_FUNCTIONS_END(LIB) \
}; }; \
static const RegisterFunctions##LIB onModuleInit##LIB;


#endif /* _FN_HELPERS_H */
