#include "ElementaryOperations.h"
#include "tr/opt/graphs/DataGraphs.h"
#include "tr/opt/graphs/DataGraphCollection.h"

using namespace rqp;
using namespace opt;

RTTI_DEF(VarIn)
RTTI_DEF(Const)
RTTI_DEF(Exists)
RTTI_DEF(FalseIfNull)
RTTI_DEF(Sequence)

XmlConstructor& VarIn::__toXML(XmlConstructor& element) const
{
    element.addAttributeValue(SE_EL_NAME("tuple"), tuple_cell::atomic_int(tuple()));
    element.addAttributeValue(SE_EL_NAME("name"), getContext()->getVarDef(tuple())->getVarLabel() );

    return rqp::ConstantOperation::__toXML(element);
};

void VarIn::setDataNode(TupleId _tid)
{
    /* Create a virtual DataGraph, that will be collected as garbage */

    dnode = new DataNode(opt::DataNode::dnExternal);
    dnode->varTupleId = _tid;
    dnode->parent = new DataGraph(optimizer->dgm());
    dnode->parent->dataNodes[0] = dnode;
    dnode->parent->operation =  this;
    optimizer->dgm()->addVariable(dnode);
}

XmlConstructor& Const::__toXML(XmlConstructor& element) const
{
    for (MemoryTupleSequence::const_iterator it = sequence->begin(); it != sequence->end(); ++it) {
        element.addElementValue(SE_EL_NAME("value"), *it);
    };

    return rqp::ConstantOperation::__toXML(element);
};

XmlConstructor& Sequence::__toXML(XmlConstructor& element) const
{
    for (OperationList::const_iterator it = children.begin(); it != children.end(); ++it) {
        if (*it != null_obj) {
            (*it)->toXML(element);
        }
    };

    return element;
};

