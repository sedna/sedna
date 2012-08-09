#include "MapOperations.h"

using namespace rqp;
using namespace opt;

RTTI_DEF(MapConcat)
RTTI_DEF(SequenceConcat)
RTTI_DEF(NestedOperation)

XmlConstructor& NestedOperation::__toXML(XmlConstructor& element) const
{
    element.addAttributeValue(SE_EL_NAME("tuple"), tuple_cell::atomic_int(getTuple()));
    element.addAttributeValue(SE_EL_NAME("name"), getContext()->varGraph.getVariable(getTuple()).toString() );

    element.openElement(SE_EL_NAME("nested"));

    if (getSubplan() != null_obj) {
        getSubplan()->toXML(element);
    }

    element.closeElement();

    return rqp::ListOperation::__toXML(element);
}
