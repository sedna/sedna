#include "GraphOperations.h"

#include "tr/opt/graphs/DataGraphs.h"
#include "tr/opt/graphs/DataGraphCollection.h"

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

void MapGraph::execute() const
{
    context->executor->bindVarible();
    context->executor->pushStack();
}
