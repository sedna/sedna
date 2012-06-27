#include "ElementaryOperations.h"

using namespace rqp;
using namespace opt;

OPERATION_INFO(VarIn)
OPERATION_INFO(Const)
OPERATION_INFO(Exists)
OPERATION_INFO(Sequence)

XmlConstructor& VarIn::__toXML(XmlConstructor& element) const
{
    element.addAttributeValue(CDGQNAME("tuple"), tuple_cell::atomic_int(tid));
    element.addAttributeValue(CDGQNAME("name"), getContext()->getVarDef(tid)->getVarLabel() );

    return rqp::ConstantOperation::__toXML(element);
};

XmlConstructor& Const::__toXML(XmlConstructor& element) const
{
    for (MemoryTupleSequence::const_iterator it = sequence->begin(); it != sequence->end(); ++it) {
        element.addElementValue(CDGQNAME("value"), *it);
    };

    return rqp::ConstantOperation::__toXML(element);
};

XmlConstructor& Exists::__toXML(XmlConstructor& element) const
{
    return rqp::ListOperation::__toXML(element);
};

XmlConstructor& Sequence::__toXML(XmlConstructor& element) const
{
    for (OperationList::const_iterator it = children.begin(); it != children.end(); ++it) {
        if (*it != null_op) {
            (*it)->toXML(element);
        }
    };

    return element;
};

