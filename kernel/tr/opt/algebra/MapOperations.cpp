#include "MapOperations.h"

using namespace rqp;
using namespace opt;

OPERATION_INFO(MapConcat)
OPERATION_INFO(SequenceConcat)

XmlConstructor& MapConcat::__toXML(XmlConstructor& element) const
{
    return rqp::NestedOperation::__toXML(element);
};

XmlConstructor& SequenceConcat::__toXML(XmlConstructor& element) const
{
    return rqp::NestedOperation::__toXML(element);
};

