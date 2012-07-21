#ifndef _EXECUTION_CONTEXT_H_
#define _EXECUTION_CONTEXT_H_

#include "tr/opt/OptTypes.h"
#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/opt/algorithms/ExecutionStack.h"
#include "tr/opt/graphs/GraphCompiler.h"
#include "tr/executor/base/sequence.h"

namespace rqp {
class RPBase;
}

namespace executor {

class VariableModel;

struct DynamicContext
{
    IElementProducer * constructorContext;
    executor::VariableModel * variables;
};

};

struct PlanExecutor
{
    executor::DynamicContext baseContext;
    executor::ExecutionStack * executionStack;
    opt::GraphCompiler gc;

    // TODO: optimize
    executor::DynamicContext * newContext(const executor::DynamicContext& cxt) { return new executor::DynamicContext(cxt); };
    executor::Result getOperationResult(rqp::RPBase * op, executor::DynamicContext * context);

    void execute(rqp::RPBase * op);
    
    PlanExecutor();
    ~PlanExecutor();
};


#endif /* _EXECUTION_CONTEXT_H_ */
