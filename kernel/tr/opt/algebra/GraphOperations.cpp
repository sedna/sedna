#include "GraphOperations.h"

#include "tr/opt/graphs/DataGraphs.h"
#include "tr/opt/graphs/DataGraphCollection.h"
#include "tr/opt/algorithms/ExecutionContext.h"
#include "tr/opt/SequenceModel.h"

using namespace rqp;
using namespace opt;

OPERATION_INFO(DataGraphOperation)
OPERATION_INFO(MapGraph)

XmlConstructor& DataGraphOperation::__toXML(XmlConstructor& element) const
{
    graph().dg->toXML(element);

    element.openElement(CDGQNAME("suboperations"));
    for (OperationList::const_iterator it = children.begin(); it != children.end(); ++it) {
        if (*it != null_op) {
            (*it)->toXML(element);
        }
    };
    element.closeElement();

    return element;
};

void DataGraphOperation::detectOutNode()
{
    U_ASSERT(func.out.size() == 1);

    out = func.out.at(0);
}

void DataGraphOperation::execute()
{
    phop::ExecutionBlock * plan = NULL;

    plan->top()->reset();

    context->executor->resultIterator =
      context->executor->result.insert(context->executor->resultIterator, phop::result_t(plan));
}

XmlConstructor& MapGraph::__toXML(XmlConstructor& element) const
{
    for (TupleScheme::const_iterator it = tupleMask.begin(); it != tupleMask.end(); ++it) {
        element.openElement(CDGQNAME("tuple"));
        element.addAttributeValue(CDGQNAME("tid"), tuple_cell::atomic_int(*it));
        element.closeElement();
    };

    graph().dg->toXML(element);

    element.openElement(CDGQNAME("suboperations"));
    for (OperationList::const_iterator it = children.begin(); it != children.end()-1; ++it) {
        if (*it != null_op) {
            (*it)->toXML(element);
        }
    };
    element.closeElement();

    if (getList() != null_op) {
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

void MapGraph::execute()
{
    phop::ExecutionBlock * plan = NULL;

    U_ASSERT(!tupleMask.empty());

    uint64_t tupleMaskInt = 0;
    phop::VariableProducer * vp = NULL;

    if (NULL != (vp = context->executor->getProducer(tupleMask))) {
        tupleMaskInt = vp->boundVarMask;
        plan = vp->top;

        while ((plan->top()->flags().changed_flags & tupleMaskInt) == 0) {
            plan->top()->next();
        };

        plan->top()->flags().changed_flags = 0;
    } else {
        if (NULL == compiledGraph)
        {
            compiledGraph = NULL;
            U_ASSERT(false);
        };

        vp = new phop::VariableProducer(compiledGraph);

        for (TupleScheme::const_iterator it = tupleMask.begin(); it != tupleMask.end(); ++it)
        {
            tupleMaskInt |= (1 << plan->resultMap.at(*it));
        };

        vp->boundVarMask = tupleMaskInt;
        context->executor->bind(graph().outTuples, vp, graph().inTuples);

        plan->top()->reset();
        plan->top()->next();
        plan->top()->flags().changed_flags = 0;
    }

    if (NULL != plan && !plan->top()->get().is_eos()) {
        if (getList() != null_op) {
            phop::ResultStack::iterator it = context->executor->resultIterator;
            it = context->executor->result.insert(it, phop::result_t(this->getList()));
            it = context->executor->result.insert(it, phop::result_t(this));
            context->executor->resultIterator = it;
        }
    }
}
