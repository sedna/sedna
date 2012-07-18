#ifndef _EXECUTION_CONTEXT_H_
#define _EXECUTION_CONTEXT_H_

#include "tr/opt/OptTypes.h"
#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/executor/base/sequence.h"

namespace rqp {
class RPBase;
}

class CollationHandler;

namespace phop {

class GraphExecutionBlock;

class DynamicContext {
public:
//    Sequence * ;
};

class ExecutionContext {
public:
    CollationHandler * collation;
};

class ConstructorContext
{
    std::stack<IElementProducer *> producerStack;
public:
    IElementProducer * producer() { return producerStack.top(); };
    
    IElementProducer * push(IElementProducer * producer)
    {
        producerStack.push(producer);
    };

    void pop()
    {
        delete producerStack.top();
        producerStack.pop();
    };

    ~ConstructorContext();
};

class PlanExecutor
{
    ConstructorContext * constructorContext;
public:
    ResultStack result;
    ResultStack::iterator resultIterator;
  
    VariableProducer * getProducer(opt::TupleId var);
    VarIterator getVarIterator(opt::TupleId var);
    ConstructorContext * getConstructorContext() { return constructorContext; };
};

}

#endif /* _EXECUTION_CONTEXT_H_ */
