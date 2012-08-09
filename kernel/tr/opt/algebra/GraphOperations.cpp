#include "GraphOperations.h"

#include "tr/opt/graphs/DataGraphs.h"
#include "tr/opt/graphs/GraphCompiler.h"
#include "tr/opt/algorithms/SequenceModel.h"
#include "tr/opt/evaluation/VariableMap.h"
#include "tr/opt/evaluation/DynamicContext.h"
#include "tr/opt/evaluation/LazyIterators.h"
#include "tr/opt/cost/Statistics.h"

using namespace rqp;
using namespace opt;

RTTI_DEF(MapGraph)

void MapGraph::updateVarGraph()
{
    for (DataNodeList::const_iterator it = func.nodes.begin(); it != func.nodes.end(); ++it)
    {
        context->varGraph.addVariableDataNode(*it);
    }
}


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

/*
uint64_t getRestrictMask(const TupleScheme & tscheme, )
{
    uint64_t result;

    for (TupleScheme::const_iterator it = tscheme.begin(); it != tscheme.end(); ++it)
    {
        result |= (1ULL << *it);
    };

    return result;
};
*/

void MapGraph::evaluateTo(executor::DynamicContext* dynamicContext)
{
    optimizer->costModel()->dynamicContext = dynamicContext;
    // TODO: optimize graph execution. There is no need in most cases to rebuild the graph
    phop::GraphExecutionBlock * geb = optimizer->gcmpler()->compile(func);

    geb->dynamicContext = dynamicContext;

    geb->prepare(&func);
    geb->top()->reset();

    executor::VariableProducer * producer =
      dynamicContext->variables->bindGraph(geb, &func);

      // TODO : implement restrictions, next window for each variable
    dynamicContext->stack->push(
      executor::Result(
        new executor::NextWindow(producer, getList(), 0)));
}
