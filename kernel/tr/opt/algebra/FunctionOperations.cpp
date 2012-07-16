#include "FunctionOperations.h"

#include "tr/opt/functions/Functions.h"
#include "tr/opt/algorithms/ExecutionContext.h"

using namespace rqp;
using namespace opt;

OPERATION_INFO(FunCall)
OPERATION_INFO(Construct)
OPERATION_INFO(XPathStep)

XmlConstructor& FunCall::__toXML(XmlConstructor& element) const
{
    element.addAttributeValue(CDGQNAME("name"), function->getQName().getColonizedName());

    int i = 0;
    for (OperationList::const_iterator it = children.begin(); it != children.end(); ++it) {
        element.openElement(CDGQNAME("param"));
        element.addAttributeValue(CDGQNAME("index"), tuple_cell::atomic_int(i++));

        if (*it != null_op) {
            (*it)->toXML(element);
        }

        element.closeElement();
    };

    return element;
};

XmlConstructor& Construct::__toXML(XmlConstructor& element) const
{
    element.addElementValue(CDGQNAME("type"), type2string(type));

    if (getName() != null_op) {
        element.openElement(CDGQNAME("name"));
        getName()->toXML(element);
        element.closeElement();
    }

    return rqp::ListOperation::__toXML(element);
};

void Construct::execute()
{
    TupleId name = context->generateTupleId();
    TupleId content = context->generateTupleId();
    
    context->executor->bind(name, getName());
    context->executor->bind(content, getList());
    
    phop::ResultStack::iterator it = context->executor->resultIterator;
    
    phop::VarIterator nameIterator = context->executor->getVarIterator(name);
    phop::VarIterator contentIterator = context->executor->getVarIterator(content);

    U_ASSERT(!nameIterator.next().is_eos());
    tuple_cell nameValue = nameIterator.get();
    U_ASSERT(nameIterator.next().is_eos());

    phop::ConstructorContext * constructorContext = context->executor->getConstructorContext();
    constructorContext->push(constructorContext->producer()->addElement(nameValue));

    while (!contentIterator.next().is_eos()) {
        constructorContext->producer()->addValue(contentIterator.get());
    };

    context->executor->result.insert(it, constructorContext->producer()->close());
    constructorContext->pop();

    context->executor->resultIterator = it;
}


XmlConstructor& XPathStep::__toXML(XmlConstructor& element) const
{
    element.addElementValue(CDGQNAME("step"), step.toXPathString());
    return rqp::ListOperation::__toXML(element);
};



