#include "FnComparison.h"
#include "FnHelpers.h"

#include "tr/opt/graphs/Predicates.h"
#include "tr/opt/algebra/PlanAlgorithms.h"
#include "tr/opt/evaluation/DynamicContext.h"

using namespace rqp;
using namespace opt;

class GeneralComparisonFunction : public phop::FunctionInfo
{
    static phop::function_info_t gcmp_name;
public:
    GeneralComparisonFunction() : FunctionInfo(&gcmp_name) {};
    GeneralComparisonFunction(const phop::function_info_t * f) : FunctionInfo(f) {};

    virtual void execute(FunCallParams* funcall, executor::DynamicContext* dynamicContext);
    virtual bool transform(FunCallParams* funcall, RewritingContext* p);
};

phop::function_info_t
GeneralComparisonFunction::gcmp_name = {NULL, "internal", "general_comparison"};

void GeneralComparisonFunction::execute(FunCallParams* funcall, executor::DynamicContext* dynamicContext)
{
    //
}


bool GeneralComparisonFunction::transform(FunCallParams* funcall, RewritingContext* p)
{
    // FIXME : make conditional check
    bool conditional = true;

    if (conditional) {
        ComparisonData * data = static_cast<ComparisonData *>(funcall->getData());

        DataGraphIndex builder(new DataGraph(&funcall->getContext()->varGraph));

        DataNode * left = new DataNode(DataNode::dnExternal, funcall->getParams().at(0));
        DataNode * right = new DataNode(DataNode::dnExternal, funcall->getParams().at(1));

        builder.nodes.push_back(left);
        builder.nodes.push_back(right);
        builder.predicates.push_back(new ValuePredicate(left, right, data->cmp));

        builder.rebuild();

        RPBase * newOp = new MapGraph(
          new Const(funcall->getContext()->varGraph.alwaysTrueSequence),
          builder.dg, TupleScheme());

        p->replaceInParent(funcall, newOp);

        cleanupFunCall(funcall);

        return true;
    }

    return false;
}

XmlConstructor& ComparisonData::toXML(XmlConstructor& constructor) const
{
    constructor.addElementValue(SE_EL_NAME("path"),
        tuple_cell::atomic_deep(xs_string, cmp.toString().c_str()));

    return constructor;
}


/*
bool do_ebv_operation_push_down(rqp::RewritingContext * pr, rqp::RPBase * op, unsigned idx)
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

        pr->replaceInParent(op, new EffectiveBooleanValue(child));
        return true;
    };

    return false;
};


static
bool rule_general_comparison_to_graph(RewritingContext * pr, rqp::FunCall * op)
{
    U_ASSERT(op->children.size() == 2);

    RPBase * left = op->children[0];
    RPBase * right = op->children[1];

    if (do_ebv_operation_push_down(pr, op, 0)) {
        return true;
    };

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

        result->varTupleId = optimizer->planContext()->generateTupleId();

        joinBuilder.predicates.push_back(predicate);
        joinBuilder.addOutNode(result);

        joinBuilder.rebuild();

        RPBase * newop =
          new rqp::EffectiveBooleanValue(
            new MapGraph(
              new VarIn(result->varTupleId), joinBuilder.dg,
              TupleScheme()));

        pr->replaceInParent(op, newop);

        return true;
    };
    return false;
};
*/

using namespace phop;

FunctionInfo * generalComparisonFunction = NULL;

REGISTER_FUNCTIONS_BEGIN(CMP)
    FunctionLibrary * lib = getFunctionLibrary();

    generalComparisonFunction = lib->registerFunction(new GeneralComparisonFunction());

REGISTER_FUNCTIONS_END(CMP)
