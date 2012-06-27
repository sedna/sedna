#include "FnComparison.h"
#include "FnHelpers.h"

#include "tr/opt/graphs/Predicates.h"

using namespace rqp;
using namespace opt;

static
bool rule_general_comparison_to_graph(PlanRewriter * pr, rqp::FunCall * op)
{
    U_ASSERT(op->children.size() == 2);

    RPBase * left = op->children[0];
    RPBase * right = op->children[1];

    ComparisonData * data = dynamic_cast<ComparisonData *>(op->getData());

    if (isGraphExpr(left) && isGraphExpr(right)) {
        DataGraphBuilder joinBuilder;

        DataNode * nodeLeft  = addGraphToJoin(joinBuilder, left);
        DataNode * nodeRight = addGraphToJoin(joinBuilder, right);
        DataNode * result = createTrueNode();

        ValuePredicate * predicate = new ValuePredicate(nodeLeft, nodeRight, data->cmp);

        joinBuilder.predicates.insert(predicate);
        joinBuilder.nodes.push_back(result);
        joinBuilder.out.clear();
        joinBuilder.out.push_back(result);

        OperationList oplist;
        addSuboperations(oplist, left);
        addSuboperations(oplist, right);

        RPBase * newop = new rqp::Exists(
          new DataGraphOperation(
            joinBuilder.build(op->getContext()->dgm()),
            oplist
          ));

        replaceInParent(pr, op, newop);        
    };
};

phop::function_info_t cmp_function = {rule_general_comparison_to_graph };


