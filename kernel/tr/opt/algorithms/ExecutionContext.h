#ifndef _EXECUTION_CONTEXT_H_
#define _EXECUTION_CONTEXT_H_

#include "tr/opt/OptTypes.h"

class CollationHandler;

namespace phop {

class StaticContext {
};

class IIterator {
    
};

class VariableMap {
public:
    IIterator * get(opt::TupleId tid);
    void bind(opt::TupleId tid, IIterator * value);
};

class DynamicContext {
public:
};
  
class ExecutionContext {
public:
    CollationHandler * collation;
};

}

#endif /* _EXECUTION_CONTEXT_H_ */
