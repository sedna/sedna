#include "OptimizingExecutor.h"

#include "tr/opt/OptSpace.h"
#include "tr/opt/OptTypes.h"
#include "tr/opt/graphs/DataGraphCollection.h"
#include "tr/opt/algebra/IndependentPlan.h"

using namespace opt;
using namespace rqp;

OptimizingExecutor optimizerInstance;
OptimizingExecutor * optimizer = &optimizerInstance;

void opt::OptimizingExecutor::onSessionBegin()
{

}

void opt::OptimizingExecutor::onSessionEnd()
{

}

void opt::OptimizingExecutor::onTransactionBegin()
{
    _dgm = new DataGraphMaster();
    _context = new PlanContext();
}

void opt::OptimizingExecutor::onTransactionEnd()
{
    delete _context;
    delete _dgm;
  
    elog(EL_LOG, ("Optimizer used : %llu / %llu", memoryPool.totalAllocated(), memoryPool.total()));

    ptrs.destroyAll<opt::IPlanDisposable>(NULL);
    ptrs.clear();
    memoryPool.clear();
}
