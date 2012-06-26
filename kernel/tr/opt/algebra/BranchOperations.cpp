#include "BranchOperations.h"

using namespace rqp;
using namespace opt;

OPERATION_INFO(If)

XmlConstructor& If::__toXML(XmlConstructor& element) const
{
    element.openElement(CDGQNAME("condition"));

    if (getCondition() != null_op) {
        getCondition()->toXML(element);
    }

    element.closeElement();

    element.openElement(CDGQNAME("then"));

    if (getThen() != null_op) {
        getThen()->toXML(element);
    }

    element.closeElement();

    element.openElement(CDGQNAME("else"));

    if (getElse() != null_op) {
        getElse()->toXML(element);
    }

    element.closeElement();

    return element;
};

