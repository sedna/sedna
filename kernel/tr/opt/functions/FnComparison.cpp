#include "FnComparison.h"
#include "FnHelpers.h"

#include "tr/opt/graphs/Predicates.h"
#include "tr/opt/algebra/PlanAlgorithms.h"

using namespace rqp;
using namespace opt;

bool do_ebv_operation_push_down(rqp::PlanRewriter * pr, rqp::RPBase * op, unsigned idx)
{
    RPBase * child = op->children[idx];

    if (child == null_op) {
        return false;
    };

    if (child->resultChild != -1)
    {
        RPBase * grandChild = child->result();

        child->children[child->resultChild] = op;
        op->children[idx] = grandChild;

        pr->replaceInParent(op, new FalseIfNull(child));
        return true;
    };

    return false;
};

static
bool rule_general_comparison_to_graph(PlanRewriter * pr, rqp::FunCall * op)
{
    U_ASSERT(op->children.size() == 2);

    RPBase * left = op->children[0];
    RPBase * right = op->children[1];

    /* Push down left */
    if (do_ebv_operation_push_down(pr, op, 0)) {
        return true;
    };

    /* Push down right */
    if (do_ebv_operation_push_down(pr, op, 1)) {
        return true;
    };

    ComparisonData * data = dynamic_cast<ComparisonData *>(op->getData());

    if (isGraphExpr(left) && isGraphExpr(right)) {
        DataGraphIndex joinBuilder(new DataGraph(optimizer->dgm()));

        DataNode  * nodeLeft  = addGraphToJoin(joinBuilder, left);
        DataNode  * nodeRight = addGraphToJoin(joinBuilder, right);
        DataNode  * result = createTrueNode();
        Predicate * predicate = new ValuePredicate(nodeLeft, nodeRight, data->cmp);

        result->varTupleId = optimizer->context()->generateTupleId();

        joinBuilder.predicates.push_back(predicate);
        joinBuilder.addOutNode(result);

        joinBuilder.rebuild();

        RPBase * newop =
          new rqp::FalseIfNull(
            new MapGraph(
              new VarIn(result->varTupleId), joinBuilder.dg,
              TupleScheme()));

        pr->replaceInParent(op, newop);

        return true;
    };
    return false;
};

using namespace phop;

phop::function_info_t cmp_function = {rule_general_comparison_to_graph };

FunctionInfo * general_comparison_function = NULL;

/*
FunctionInfo * g_eq_function = NULL;
FunctionInfo * g_ge_function = NULL;
FunctionInfo * g_gt_function = NULL;
FunctionInfo * g_le_function = NULL;
FunctionInfo * g_lt_function = NULL;
*/

REGISTER_FUNCTIONS_BEGIN(CMP)
    FunctionLibrary * lib = getFunctionLibrary();

    general_comparison_function = lib->registerFunction(NULL, "internal", "general_comparison", &cmp_function);

#define CMP_FUNCTION(OP) \
    OP##_function = lib->registerFunction(NULL, "internal", #OP, &cmp_function); \
    OP##_function->default_data = new ComparisonData(Comparison(opt::Comparison::OP));
/*
    CMP_FUNCTION(g_eq)
    CMP_FUNCTION(g_ge)
    CMP_FUNCTION(g_gt)
    CMP_FUNCTION(g_le)
    CMP_FUNCTION(g_lt)
*/
#undef CMP_FUNCTION

REGISTER_FUNCTIONS_END(CMP)
