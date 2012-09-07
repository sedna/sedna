#ifndef _DYNAMIC_CONTEXT_H_
#define _DYNAMIC_CONTEXT_H_

#include "tr/opt/OptTypes.h"

namespace executor {

/** @brief Describes current execution environment
 * Dynamic context is created for each execution environment
 * It contains all lazy sequences and all caches
 */
struct DynamicContext
{
public:
    /* We have no focus for dynamic context.
     * Focus is accessed via variables that are generated on demand in static.
     */

    /* This is normal part of dynamic context */
    executor::VariableModel * variables;

    /* This is a stack being modified. */
    executor::VirtualSequence * stack;

    /* This is a stack being modified. */
    executor::UpdateSequence * updates;

    DynamicContext();
    ~DynamicContext();
};

};


#endif /* _DYNAMIC_CONTEXT_H_ */
