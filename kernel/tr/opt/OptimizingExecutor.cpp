#include "OptimizingExecutor.h"

#include "tr/opt/OptSpace.h"
#include "tr/opt/OptTypes.h"
#include "tr/opt/graphs/DataGraphCollection.h"
#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/opt/algorithms/ExecutionContext.h"

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
    _executor = new PlanExecutor();
}

void opt::OptimizingExecutor::onTransactionEnd()
{
    delete _context;
    delete _dgm;
    delete _executor;

    elog(EL_LOG, ("Optimizer used : %llu / %llu", memoryPool.totalAllocated(), memoryPool.total()));

    ptrs.destroyAll<opt::IPlanDisposable>(NULL);
    ptrs.clear();
    memoryPool.clear();
}
