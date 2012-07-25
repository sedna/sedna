#include "OptimizingExecutor.h"

#include "tr/opt/OptTypes.h"
#include "tr/opt/graphs/DataGraphCollection.h"
#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/opt/algorithms/ExecutionContext.h"
#include "tr/opt/cost/Statistics.h"

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
    _costModel = new CostModel();
}

void opt::OptimizingExecutor::onTransactionEnd()
{
    delete _costModel;
    delete _executor;
    delete _context;
    delete _dgm;

    elog(EL_LOG, ("Optimizer used : %llu / %llu", planGenerationPool.totalAllocated(), planGenerationPool.total()));
    elog(EL_LOG, ("Cost model used : %llu / %llu", costModelPool.totalAllocated(), costModelPool.total()));

//    ptrs.destroyAll<opt::IPlanDisposable>(NULL);
//    ptrs.clear();

    planGenerationPool.clear();
    costModelPool.clear();
}
