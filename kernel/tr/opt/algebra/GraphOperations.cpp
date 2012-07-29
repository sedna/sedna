#include "GraphOperations.h"

#include "tr/opt/graphs/DataGraphs.h"
#include "tr/opt/graphs/DataGraphCollection.h"
#include "tr/opt/algorithms/ExecutionContext.h"
#include "tr/opt/algorithms/SequenceModel.h"
#include "tr/opt/algorithms/VariableMap.h"

using namespace rqp;
using namespace opt;

RTTI_DEF(MapGraph)

XmlConstructor& MapGraph::__toXML(XmlConstructor& element) const
{
    for (TupleScheme::const_iterator it = groupBy.begin(); it != groupBy.end(); ++it) {
        element.openElement(SE_EL_NAME("group-by"));
        element.addAttributeValue(SE_EL_NAME("tid"), tuple_cell::atomic_int(*it));
        element.closeElement();
    };

    graph().dg->toXML(element);

    if (getList() != null_obj) {
        getList()->toXML(element);
    }

    return element;
};

void MapGraph::joinGraph(DataGraphIndex& rg)
{
    func.nodes.insert(func.nodes.end(), rg.nodes.begin(), rg.nodes.end());
    func.predicates.insert(func.predicates.end(), rg.predicates.begin(), rg.predicates.end());
    func.out.insert(func.out.end(), rg.out.begin(), rg.out.end());

    func.rebuild();
}

using namespace executor;

class GroupByNext : public executor::IExecuteProc
{
public:
    DynamicContext * context;
    VariableProducer * producer;
    rqp::RPBase * nextOp;
    const TupleScheme & restrictMask;

    explicit GroupByNext(DynamicContext * _context, VariableProducer * _producer, RPBase * _nextOp, const TupleScheme & _restrictMask)
      : context(_context), producer(_producer), nextOp(_nextOp), restrictMask(_restrictMask) {};

    virtual void execute(ExecutionStack* executor);
};

void GroupByNext::execute(ExecutionStack* executor)
{
    U_ASSERT(producer->valueSequence == NULL);

    phop::ITupleOperator * op = producer->graphSequence->top();
    ExecutionStack* saveStack = optimizer->swapStack(executor);

    if (!op->get().is_eos()) {
        executor->push(Result(new GroupByNext(*this)));
        optimizer->pexecutor()->push(context, nextOp);
    };

    optimizer->swapStack(saveStack);

    // TODO : GroupBy Mask
/*
    uint64_t saveRestrickMask = producer->restrictMask;

    if (saveRestrickMask == 0) {
        saveRestrickMask = tupleMask;
    };

    producer->restrictMask = 0;
*/
//    if (!producer->next()) {
//        return;
//    };
//    producer->restrictMask = saveRestrickMask;

}

uint64_t getRestrictMask(const TupleScheme & tscheme)
{
    uint64_t result;

    for (TupleScheme::const_iterator it = tscheme.begin(); it != tscheme.end(); ++it)
    {
        result |= (1ULL << *it);
    };

    return result;
};

executor::IExecuteProc* MapGraph::getExecutor()
{
    PlanExecutor * executor = optimizer->pexecutor();

    // TODO: optimize graph execution. There is no need in most cases to rebuild the graph
    phop::GraphExecutionBlock * geb = executor->gc.compile(func, executor->currentContext);

    geb->prepare(&(func));
    geb->top()->reset();

    VariableProducer * producer =
      executor->currentContext->variables->bindGraph(geb, &func);

    if (!groupBy.empty()) {
        return new GroupByNext(executor->currentContext, producer, getList(), groupBy);
    } else {
        U_ASSERT(false);
        return NULL;
//        return new GroupByNext(executor->currentContext, producer, getList(), groupBy);
    };

}
