#include "SequenceModel.h"

#include "tr/models/XmlConstructor.h"

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

TupleFromItemOperator::TupleFromItemOperator(IValueOperator* convert_op, unsigned _size)
    : ITupleOperator(OPINFO_REF, convert_op, _size)
{
    
}

ReduceToItemOperator::ReduceToItemOperator(const phop::TupleIn& op, bool _nested)
    : IValueOperator(OPINFO_REF), in(op), nested(_nested)
{

}

void ReduceToItemOperator::do_next()
{
    in->next();
    
    if (in->get().is_eos()) {
        seteos();
        return;
    };

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




XmlConstructor & IOperator::toXML(XmlConstructor & element) const
{
    element.openElement(PHOPQNAME(info()->name));
    __toXML(element);
    element.closeElement();
    return element;
}

XmlConstructor & BinaryTupleOperator::__toXML(XmlConstructor & element ) const
{
    left.op->toXML(element);
    right.op->toXML(element);
    return element;
}

XmlConstructor & UnaryTupleOperator::__toXML(XmlConstructor & element ) const
{
    in.op->toXML(element);
    return element;
}

XmlConstructor & TupleFromItemOperator::__toXML(XmlConstructor & element ) const
{
    return element;
}

XmlConstructor & TupleFromItemOperator::toXML(XmlConstructor & element ) const
{
//    element = element->addElement(PHOPQNAME(info()->name));
    return _convert_op->toXML(element);
//    element->close();
//    return element;
}

XmlConstructor & ReduceToItemOperator::__toXML(XmlConstructor & element) const
{
    return element;
}

XmlConstructor & ReduceToItemOperator::toXML(XmlConstructor & element) const
{
    if (!nested) {
//        element = element->addElement(PHOPQNAME(info()->name));
        return in.op->toXML(element);
//        element->close();
    }
    return element;
}

XmlConstructor & ItemOperator::__toXML(XmlConstructor & element ) const
{
    in->toXML(element);
    return element;
}
