#include "ElementaryOperations.h"

#include "tr/opt/graphs/DataGraphs.h"
#include "tr/opt/evaluation/DynamicContext.h"
#include "tr/opt/evaluation/VariableMap.h"

using namespace rqp;
using namespace opt;

RTTI_DEF(VarIn)
RTTI_DEF(Const)
RTTI_DEF(Exists)
RTTI_DEF(EffectiveBooleanValue)
RTTI_DEF(Sequence)

struct VariableIterator : public executor::IExecuteProc
{
    executor::VarIterator varIterator;

    VariableIterator(const executor::VarIterator & it)
      : varIterator(it) {};

    virtual void execute(executor::VirtualSequence* sequence);
};

void VariableIterator::execute(executor::VirtualSequence* sequence)
{
    tuple_cell result = varIterator.next();

    if (!result.is_eos()) {
        sequence->push(executor::Result(new VariableIterator(*this)));
        sequence->push(executor::Result(result));
    }
}

void VarIn::evaluateTo(executor::DynamicContext* dynamicContext)
{
    dynamicContext->stack->push(
      executor::Result(
        new VariableIterator(
          dynamicContext->variables->getIterator(getTuple()))));;
}


XmlConstructor& VarIn::__toXML(XmlConstructor& element) const
{
    element.addAttributeValue(SE_EL_NAME("tuple"), tuple_cell::atomic_int(getTuple()));
    element.addAttributeValue(SE_EL_NAME("name"), context->varGraph.getVariable(getTuple()).toString() );

    return rqp::ConstantOperation::__toXML(element);
};

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

