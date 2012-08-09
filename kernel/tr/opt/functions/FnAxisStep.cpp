#include "FnAxisStep.h"
#include "FnHelpers.h"

#include "tr/opt/algebra/PlanAlgorithms.h"
#include "tr/opt/graphs/Predicates.h"

using rqp::FunCallParams;
using rqp::RewritingContext;

class AxisStepFunction : public phop::FunctionInfo
{
    static phop::function_info_t axis_step_name;
public:
    AxisStepFunction() : FunctionInfo(&axis_step_name) {};

    virtual void execute(FunCallParams* funcall, executor::DynamicContext* dynamicContext);
    virtual bool transform(FunCallParams* funcall, RewritingContext* p);
};

phop::function_info_t
AxisStepFunction::axis_step_name = {NULL, "internal", "axis_step"};

XmlConstructor& AxisStepData::toXML(XmlConstructor& constructor) const
{
    constructor.addElementValue(SE_EL_NAME("path"),
        tuple_cell::atomic_deep(xs_string, step.toXPathString().c_str()));

    return constructor;
}


void AxisStepFunction::execute(FunCallParams* funcall, executor::DynamicContext* dynamicContext)
{
    //
}

using namespace opt;

bool AxisStepFunction::transform(FunCallParams* funcall, RewritingContext* p)
{
    TupleId arg = funcall->getParams().at(0);
    TupleInfo & info = funcall->getContext()->varGraph.getVariable(arg);
    AxisStepData * data = static_cast<AxisStepData *>(funcall->getData());

    if (instanceof<rqp::MapGraph>(info.definedIn)) {
        rqp::MapGraph * mgraph = static_cast<rqp::MapGraph *>(info.definedIn);
        DataGraphIndex * graph = &mgraph->graph();

        DataNode * inNode = graph->getNode(arg);
        TupleId resultTuple = funcall->getContext()->generateTupleId();
        DataNode * result = new DataNode(opt::DataNode::dnFreeNode, resultTuple);
        Predicate * predicate = new StructuralPredicate(inNode, result, data->step);

        funcall->getContext()->varGraph.addVariableDeclaration(resultTuple, mgraph)
            .properties.flags = tuple_info_t::sf_notNull;

        graph->addOutNode(result);
        graph->predicates.push_back(predicate);

        graph->rebuild();

        p->replaceInParent(funcall, new rqp::VarIn(resultTuple));
        info.operations.erase(funcall);

        return true;
    };
    
    return false;
}

using namespace phop;

FunctionInfo * axisStepFunction = NULL;

REGISTER_FUNCTIONS_BEGIN(AXIS)
    FunctionLibrary * lib = getFunctionLibrary();

    axisStepFunction = lib->registerFunction(new AxisStepFunction());

REGISTER_FUNCTIONS_END(AXIS)
