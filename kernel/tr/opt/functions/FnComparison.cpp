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

    virtual bool transform(FunCallParams* funcall, RewritingContext* p);
};

phop::function_info_t
GeneralComparisonFunction::gcmp_name = {NULL, "internal", "general_comparison", 2};

bool GeneralComparisonFunction::transform(FunCallParams* funcall, RewritingContext* p)
{
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

XmlConstructor& ComparisonData::toXML(XmlConstructor& constructor) const
{
    constructor.addElementValue(SE_EL_NAME("path"),
        tuple_cell::atomic_deep(xs_string, cmp.toString().c_str()));

    return constructor;
}

using namespace phop;

FunctionInfo * generalComparisonFunction = NULL;

REGISTER_FUNCTIONS_BEGIN(CMP)
    FunctionLibrary * lib = getFunctionLibrary();

    generalComparisonFunction = lib->registerFunction(new GeneralComparisonFunction());

REGISTER_FUNCTIONS_END(CMP)
