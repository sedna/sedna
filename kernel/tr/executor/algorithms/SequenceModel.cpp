#include "SequenceModel.h"
#include "tr/structures/producer.h"

using namespace phop;

std::stack<ExecutionBlock * > ExecutionBlock::blockStack;

OPINFO_DEF(TupleFromItemOperator)
OPINFO_DEF(ReduceToItemOperator)

void BinaryTupleOperator::reset()
{
    ITupleOperator::reset();
    left.op->reset();
    right.op->reset();
}

void BinaryTupleOperator::setContext(ExecutionContext* __context)
{
    phop::IOperator::setContext(__context);
    left.op->setContext(__context);
    right.op->setContext(__context);
}

void UnaryTupleOperator::reset()
{
    phop::ITupleOperator::reset();
    in.op->reset();
}

void UnaryTupleOperator::setContext(ExecutionContext* __context)
{
    phop::IOperator::setContext(__context);
    in.op->setContext(__context);
}

void ItemOperator::reset()
{
    phop::IValueOperator::reset();
    in->reset();
}

void ItemOperator::setContext(ExecutionContext* __context)
{
    phop::IOperator::setContext(__context);
    in->setContext(__context);
}


IOperator::IOperator(OPINFO_T _opinfo) : opinfo(_opinfo)
{
    ExecutionBlock::current()->body.push_back(this);
}

IOperator::~IOperator()
{
    //
}


void TupleFromItemOperator::do_next()
{
    U_ASSERT(false);
}

void TupleFromItemOperator::reset()
{
    phop::ITupleOperator::reset();
    _convert_op->reset();
}

void TupleFromItemOperator::setContext(ExecutionContext* __context)
{
    phop::IOperator::setContext(__context);
    _convert_op->setContext(__context);
}

ITupleOperator::ITupleOperator(OPINFO_T _opinfo, IValueOperator* __convert_op)
    : IOperator(_opinfo), _convert_op(__convert_op)
{

}

TupleFromItemOperator::TupleFromItemOperator(IValueOperator* convert_op)
    : ITupleOperator(OPINFO_REF, convert_op)
{
    
}

ReduceToItemOperator::ReduceToItemOperator(const phop::TupleIn& op, bool _nested)
    : IValueOperator(OPINFO_REF), in(op), nested(_nested)
{

}

void ReduceToItemOperator::do_next()
{
    in->next();
    push(in.get());
}

void ReduceToItemOperator::reset()
{
    phop::IValueOperator::reset();
    in->reset();
}

void ReduceToItemOperator::setContext(ExecutionContext* __context)
{
    phop::IOperator::setContext(__context);
    in->setContext(__context);
}




IElementProducer* IOperator::toXML(IElementProducer* element) const
{
    element = element->addElement(PHOPQNAME(info()->name));
    __toXML(element);
    element->close();
    return element;
}

IElementProducer* BinaryTupleOperator::toXML(IElementProducer* element ) const
{
    element = element->addElement(PHOPQNAME(info()->name));
    __toXML(element);
    left.op->toXML(element);
    right.op->toXML(element);
    element->close();
    return element;
}

IElementProducer* UnaryTupleOperator::toXML(IElementProducer* element ) const
{
    element = element->addElement(PHOPQNAME(info()->name));
    __toXML(element);
    in.op->toXML(element);
    element->close();
    return element;
}

IElementProducer* TupleFromItemOperator::__toXML(IElementProducer* element ) const
{
    return element;
}

IElementProducer* TupleFromItemOperator::toXML(IElementProducer* element ) const
{
//    element = element->addElement(PHOPQNAME(info()->name));
    return _convert_op->toXML(element);
//    element->close();
//    return element;
}

IElementProducer* ReduceToItemOperator::__toXML(IElementProducer* element) const
{
    return element;
}

IElementProducer* ReduceToItemOperator::toXML(IElementProducer* element) const
{
    if (!nested) {
//        element = element->addElement(PHOPQNAME(info()->name));
        return in.op->toXML(element);
//        element->close();
    }
    return element;
}

IElementProducer* ItemOperator::toXML(IElementProducer* element ) const
{
    element = element->addElement(PHOPQNAME(info()->name));
    __toXML(element);
    in->toXML(element);
    element->close();
    return element;
}
