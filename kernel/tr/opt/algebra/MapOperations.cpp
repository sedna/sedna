#include "MapOperations.h"

using namespace rqp;
using namespace opt;

OPERATION_INFO(MapConcat)
OPERATION_INFO(SequenceConcat)
OPERATION_INFO(Select)

XmlConstructor& MapConcat::__toXML(XmlConstructor& element) const
{
    element.addAttributeValue(CDGQNAME("tuple"), tuple_cell::atomic_int(context.item));
    element.addAttributeValue(CDGQNAME("name"), getContext()->getVarDef(context.item)->getVarLabel() );
    return rqp::NestedOperation::__toXML(element);
};


XmlConstructor& SequenceConcat::__toXML(XmlConstructor& element) const
{
    element.addAttributeValue(CDGQNAME("tuple"), tuple_cell::atomic_int(tid));
    element.addAttributeValue(CDGQNAME("name"), getContext()->getVarDef(tid)->getVarLabel() );
    return rqp::NestedOperation::__toXML(element);
};

XmlConstructor& Select::__toXML(XmlConstructor& element) const
{
    return rqp::NestedOperation::__toXML(element);
};

