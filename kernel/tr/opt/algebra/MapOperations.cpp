#include "MapOperations.h"
#include "tr/opt/graphs/DataGraphCollection.h"

using namespace rqp;
using namespace opt;

RTTI_DEF(MapConcat)
RTTI_DEF(SequenceConcat)
RTTI_DEF(NestedOperation)

XmlConstructor& NestedOperation::__toXML(XmlConstructor& element) const
{
    element.addAttributeValue(SE_EL_NAME("tuple"), tuple_cell::atomic_int(tuple()));
    element.addAttributeValue(SE_EL_NAME("name"), getContext()->getVarDef(tuple())->getVarLabel() );

    element.openElement(SE_EL_NAME("nested"));

    if (getSubplan() != null_obj) {
        getSubplan()->toXML(element);
    }

    element.closeElement();

    return rqp::ListOperation::__toXML(element);
}

void NestedOperation::setDataNode(TupleId _tid)
{
    /* Create a virtual DataGraph, that will be collected as garbage */

    dnode = new DataNode(opt::DataNode::dnFreeNode);

    dnode->varTupleId = _tid;
    dnode->parent = new DataGraph(optimizer->dgm());
    dnode->parent->dataNodes[0] = dnode;
    dnode->parent->operation =  this;

    optimizer->dgm()->addVariable(dnode);
}
