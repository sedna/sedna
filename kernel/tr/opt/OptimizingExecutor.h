#ifndef _OPTIMIZING_EXECUTOR_H_
#define _OPTIMIZING_EXECUTOR_H_

#include "tr/models/SednaModule.h"
#include "tr/models/MemoryPool.h"
#include "tr/models/PointerArray.h"

#include "tr/opt/OptForewards.h"

namespace opt {

class OptimizingExecutor : SednaModule
{
private:
    DataGraphMaster * _dgm;
    rqp::PlanContext * _context;
    PlanExecutor * _executor;
    CostModel * _costModel;

public:
    MemoryPool planGenerationPool;
    MemoryPool costModelPool;
//    MemoryPool gataGraphPool;
//    MemoryPool executionPool;

    OptimizingExecutor()
      : _dgm(NULL), _context(NULL), _executor(NULL), _costModel(NULL),
        planGenerationPool(MEMORY_BLOCK_SIZE),
        costModelPool(MEMORY_BLOCK_SIZE)
          {};
  
    virtual void onSessionBegin();
    virtual void onSessionEnd();

    virtual void onTransactionBegin();
    virtual void onTransactionEnd();

    DataGraphMaster * dgm() const { return _dgm; };
    rqp::PlanContext * context() const { return _context; };
    PlanExecutor * executor() const { return _executor; };
    CostModel * costModel() const { return _costModel; };
};

};

extern opt::OptimizingExecutor * optimizer;

#endif /* _OPTIMIZING_EXECUTOR_H_ */
