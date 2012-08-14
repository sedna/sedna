#include "MapOperations.h"

using namespace rqp;
using namespace opt;

RTTI_DEF(MapConcat)
RTTI_DEF(SequenceConcat)
RTTI_DEF(NestedOperation)
RTTI_DEF(GroupBy)

XmlConstructor& NestedOperation::__toXML(XmlConstructor& element) const
{
    element.addAttributeValue(SE_EL_NAME("tuple"), tuple_cell::atomic_int(getTuple()));
    element.addAttributeValue(SE_EL_NAME("name"), getContext()->varGraph.getVariable(getTuple()).toString() );

    element.openElement(SE_EL_NAME("nested"));

    if (getSubplan() != null_obj) {
        getSubplan()->toXML(element);
    }

    element.closeElement();

    if (getList() != null_obj) {
        getList()->toXML(element);
    }

    return element;
}

XmlConstructor& GroupBy::__toXML(XmlConstructor& constructor) const
{
    constructor.openElement(SE_EL_NAME("group-by"));
    constructor.addAttributeValue(SE_EL_NAME("var-id"), tuple_cell::atomic_int(getTuple()));
    constructor.closeElement();
    
    return rqp::ListOperation::__toXML(constructor);
}
