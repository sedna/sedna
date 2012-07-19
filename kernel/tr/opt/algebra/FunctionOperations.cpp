#include "FunctionOperations.h"

#include "tr/opt/functions/Functions.h"
#include "tr/opt/algorithms/ExecutionContext.h"

using namespace rqp;
using namespace opt;

OPERATION_INFO(FunCall)
OPERATION_INFO(Construct)
OPERATION_INFO(XPathStep)

XmlConstructor& FunCall::__toXML(XmlConstructor& element) const
{
    element.addAttributeValue(CDGQNAME("name"), function->getQName().getColonizedName());

    int i = 0;
    for (OperationList::const_iterator it = children.begin(); it != children.end(); ++it) {
        element.openElement(CDGQNAME("param"));
        element.addAttributeValue(CDGQNAME("index"), tuple_cell::atomic_int(i++));

        if (*it != null_op) {
            (*it)->toXML(element);
        }

        element.closeElement();
    };

    return element;
};

XmlConstructor& Construct::__toXML(XmlConstructor& element) const
{
    element.addElementValue(CDGQNAME("type"), type2string(type));

    if (getName() != null_op) {
        element.openElement(CDGQNAME("name"));
        getName()->toXML(element);
        element.closeElement();
    }

    return rqp::ListOperation::__toXML(element);
};

XmlConstructor& XPathStep::__toXML(XmlConstructor& element) const
{
    element.addElementValue(CDGQNAME("step"), step.toXPathString());
    return rqp::ListOperation::__toXML(element);
};



