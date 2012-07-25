#include "FunctionOperations.h"

#include "tr/opt/functions/Functions.h"
#include "tr/opt/algorithms/ExecutionContext.h"

using namespace rqp;
using namespace opt;

RTTI_DEF(FunCall)
RTTI_DEF(Construct)
RTTI_DEF(XPathStep)

XmlConstructor& FunCall::__toXML(XmlConstructor& element) const
{
    element.addAttributeValue(SE_EL_NAME("name"), function->getQName().getColonizedName());

    int i = 0;
    for (OperationList::const_iterator it = children.begin(); it != children.end(); ++it) {
        element.openElement(SE_EL_NAME("param"));
        element.addAttributeValue(SE_EL_NAME("index"), tuple_cell::atomic_int(i++));

        if (*it != null_obj) {
            (*it)->toXML(element);
        }

        element.closeElement();
    };

    return element;
};

XmlConstructor& Construct::__toXML(XmlConstructor& element) const
{
    element.addElementValue(SE_EL_NAME("type"), type2string(type));

    if (getName() != null_obj) {
        element.openElement(SE_EL_NAME("name"));
        getName()->toXML(element);
        element.closeElement();
    }

    return rqp::ListOperation::__toXML(element);
};

XmlConstructor& XPathStep::__toXML(XmlConstructor& element) const
{
    element.addElementValue(SE_EL_NAME("step"), step.toXPathString());
    return rqp::ListOperation::__toXML(element);
};



