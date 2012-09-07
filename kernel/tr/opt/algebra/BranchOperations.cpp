#include "BranchOperations.h"

using namespace rqp;
using namespace opt;

RTTI_DEF(If)

XmlConstructor& If::__toXML(XmlConstructor& element) const
{
    element.openElement(SE_EL_NAME("condition"));

    if (getCondition() != null_obj) {
        getCondition()->toXML(element);
    }

    element.closeElement();

    element.openElement(SE_EL_NAME("then"));

    if (getThen() != null_obj) {
        getThen()->toXML(element);
    }

    element.closeElement();

    element.openElement(SE_EL_NAME("else"));

    if (getElse() != null_obj) {
        getElse()->toXML(element);
    }

    element.closeElement();

    return element;
};

