#include "GraphOperations.h"

#include "tr/opt/graphs/DataGraphs.h"

using namespace rqp;
using namespace opt;

OPERATION_INFO(DataGraphOperation)
OPERATION_INFO(MapGraph)

XmlConstructor& DataGraphOperation::__toXML(XmlConstructor& element) const
{
    element.openElement(CDGQNAME("graph"));
    func->toXML(element);
    element.closeElement();

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
    DataGraphWrapper dgw(func);

    U_ASSERT(dgw.out.size() == 1);

    out = dgw.out.at(0);
}


XmlConstructor& MapGraph::__toXML(XmlConstructor& element) const
{
    element.openElement(CDGQNAME("graph"));
    func->toXML(element);
    element.closeElement();

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

