#ifndef _OPTIMIZING_EXECUTOR_H_
#define _OPTIMIZING_EXECUTOR_H_

#include "tr/models/SednaModule.h"
#include "tr/cat/catmem.h"
#include "tr/opt/OptSpace.h"

namespace rqp {
    class PlanContext;
}

class OptimizationSpace;

namespace opt {

class DataGraphMaster;

class OptimizingExecutor : SednaModule
{
private:
    DataGraphMaster * _dgm;
    rqp::PlanContext * _context;
    
    MemoryPool memoryPool;
    FastPointerArray ptrs;
public:

    inline 
    void * createObject(size_t n) {
        void * ptr = memoryPool.alloc(n);
        ptrs.add(ptr);
        return ptr;
    };
  
    virtual void onSessionBegin();
    virtual void onSessionEnd();

    virtual void onTransactionBegin();
    virtual void onTransactionEnd();

    DataGraphMaster * dgm() const { return _dgm; };
    rqp::PlanContext * context() const { return _context; };
};

};

extern opt::OptimizingExecutor * optimizer;

#endif /* _OPTIMIZING_EXECUTOR_H_ */
