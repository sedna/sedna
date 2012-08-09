#ifndef _DYNAMIC_CONTEXT_H_
#define _DYNAMIC_CONTEXT_H_

#include "tr/opt/OptTypes.h"
#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/opt/evaluation/VirtualSequence.h"
#include "tr/opt/graphs/GraphCompiler.h"
#include "tr/executor/base/sequence.h"

namespace executor {

/** @brief Describes current execution environment
 * Dynamic context is created for each execution environment
 * It contains all lazy sequences and all caches
 */
struct DynamicContext
{
private:
    /** @brief Stack of lazy sequences */
    std::stack<executor::VirtualSequence *> stackStack;
public:
    /* We have no focus for dynamic context.
     * Focus is accessed via variables that are generated on demand in static.
     * /

    /* This is normal part of dynamic context */
    executor::VariableModel * variables;

    /* This is a stack being modified. */
    executor::VirtualSequence * stack;

    /** @brief Change current execution environment and save previous state */
    void push(executor::VirtualSequence * newStack)
    {
        stackStack.push(stack);
        stack = newStack;
        stack->context = this;
    };

    /** @brief Return previous execution environment */
    void pop()
    {
        stack = stackStack.top();
        stackStack.pop();
    }
    
//    static executor::MemoryStack * memoryStack;

    DynamicContext();
    ~DynamicContext();
};

};


#endif /* _DYNAMIC_CONTEXT_H_ */
