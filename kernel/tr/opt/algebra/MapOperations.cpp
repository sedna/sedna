#include "MapOperations.h"

using namespace rqp;
using namespace opt;

RTTI_DEF(MapConcat)
RTTI_DEF(SequenceConcat)

XmlConstructor& MapConcat::__toXML(XmlConstructor& element) const
{
    return rqp::NestedOperation::__toXML(element);
};

XmlConstructor& SequenceConcat::__toXML(XmlConstructor& element) const
{
    return rqp::NestedOperation::__toXML(element);
};

