#ifndef _EXECUTION_CONTEXT_H_
#define _EXECUTION_CONTEXT_H_

#include "tr/opt/OptTypes.h"
#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/opt/algorithms/ExecutionStack.h"
#include "tr/opt/graphs/GraphCompiler.h"
#include "tr/executor/base/sequence.h"

namespace executor {

struct DynamicContext
{
private:
    IElementProducer * _constructorContext;
    void createVirtualRoot();
public:
    void setConstructorContext(IElementProducer * constructorContext)
    {
        _constructorContext = constructorContext;
    }
    
    IElementProducer * constructorContext()
    {
        if (_constructorContext == NULL)
        {
            createVirtualRoot();
        };

        return _constructorContext;
    };

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

    /* Pushes operation to execution context */

    void push(
        executor::ExecutionStack * executionStack,
        executor::DynamicContext * context,
        rqp::RPBase * op
    );

    void execute(rqp::RPBase * op);
    
    PlanExecutor();
    ~PlanExecutor();
};


#endif /* _EXECUTION_CONTEXT_H_ */
