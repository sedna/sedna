#include "MapOperations.h"
#include "tr/opt/evaluation/DynamicContext.h"
#include "tr/opt/evaluation/VariableMap.h"

using namespace rqp;
using namespace opt;

RTTI_DEF(MapConcat)
RTTI_DEF(SequenceConcat)
RTTI_DEF(NestedOperation)
RTTI_DEF(GroupBy)

void SequenceConcat::evaluateTo(executor::DynamicContext* dynamicContext)
{
    executor::VarCacheInfo * varInfo = dynamicContext->variables->getProducer(getTuple());

    if (varInfo == NULL) {
        dynamicContext->variables->bind(new executor::VariableProducer(getTuple()));
        varInfo = dynamicContext->variables->getProducer(getTuple());
        varInfo->producer->valueSequence->context = dynamicContext;
    }

    U_ASSERT(varInfo->producer->valueSequence != NULL);

    {
        executor::SetContext _setContext(dynamicContext, varInfo->producer->valueSequence);
        varInfo->producer->reset();
        getSubplan()->evaluateTo(dynamicContext);
    }

    getList()->evaluateTo(dynamicContext);
}

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
