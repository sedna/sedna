#ifndef _EXECUTION_CONTEXT_H_
#define _EXECUTION_CONTEXT_H_

class CollationHandler;

namespace phop {

class ExecutionContext {
public:
    CollationHandler * collation;
};

}

#endif /* _EXECUTION_CONTEXT_H_ */