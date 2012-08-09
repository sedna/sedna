#include "FnAxisStep.h"
#include "FnHelpers.h"

#include "tr/opt/algebra/PlanAlgorithms.h"

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

void AxisStepFunction::execute(FunCallParams* funcall, executor::DynamicContext* dynamicContext)
{
    //
}


bool AxisStepFunction::transform(FunCallParams* funcall, RewritingContext* p)
{
//    p->replaceVariable(22, 19);
    return false;
}

using namespace phop;

FunctionInfo * axisStepFunction = NULL;

REGISTER_FUNCTIONS_BEGIN(AXIS)
    FunctionLibrary * lib = getFunctionLibrary();

    axisStepFunction = lib->registerFunction(new AxisStepFunction());

REGISTER_FUNCTIONS_END(AXIS)
