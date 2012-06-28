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

        joinBuilder.predicates.push_back(predicate);
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

        return true;
    };
    return false;
};

using namespace phop;

phop::function_info_t cmp_function = {rule_general_comparison_to_graph };

FunctionInfo * general_comparison_function = NULL;

FunctionInfo * g_eq_function = NULL;
FunctionInfo * g_ge_function = NULL;
FunctionInfo * g_gt_function = NULL;
FunctionInfo * g_le_function = NULL;
FunctionInfo * g_lt_function = NULL;

REGISTER_FUNCTIONS_BEGIN(CMP)
    FunctionLibrary * lib = getFunctionLibrary();

    general_comparison_function = lib->registerFunction("internal", "general_comparison", &cmp_function);

#define CMP_FUNCTION(OP) \
    OP##_function = lib->registerFunction("internal", #OP, &cmp_function); \
    OP##_function->default_data = new ComparisonData(Comparison(opt::Comparison::OP));

    CMP_FUNCTION(g_eq)
    CMP_FUNCTION(g_ge)
    CMP_FUNCTION(g_gt)
    CMP_FUNCTION(g_le)
    CMP_FUNCTION(g_lt)

#undef CMP_FUNCTION

REGISTER_FUNCTIONS_END(CMP)
