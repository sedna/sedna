#ifndef _EXECUTION_CONTEXT_H_
#define _EXECUTION_CONTEXT_H_

#include "tr/opt/OptTypes.h"

class CollationHandler;

namespace phop {

class StaticContext {
};

struct VarIterator {
    //
};

class VariableMap {
public:
    VarIterator & get(opt::TupleId tid);
    void bind(opt::TupleId tid, VarIterator value);
};

class DynamicContext {
public:
//    Sequence * ;
};
  
class ExecutionContext {
public:
    CollationHandler * collation;
};

}

#endif /* _EXECUTION_CONTEXT_H_ */
