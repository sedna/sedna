#include "Joins.h"

#include "tr/executor/base/tuple.h"
#include "tr/executor/base/ITupleSerializer.h"
#include "tr/executor/base/SortedSequence.h"
#include "tr/executor/base/sequence.h"
#include "tr/executor/base/PPUtils.h"

#include "tr/models/XmlConstructor.h"

#include "tr/nid/nidalloc.h"
#include "tr/nid/nid.h"

#include "ExecutionContext.h"

using namespace phop;

OPINFO_DEF(DocOrderMerge)
OPINFO_DEF(TupleSort)
OPINFO_DEF(TupleJoinFilter)
OPINFO_DEF(TuplePredicateFilter)

struct NidStringCmp {
    bool operator()(const NIDMergeHeap::value_type & x, const NIDMergeHeap::value_type& y) const
        { return x.first.compare(y.first) > 0; }
};

static
phop::NIDMergeHeap::value_type getMergeValue(const MappedTupleIn & t, TupleList::size_type i) {
    return NIDMergeHeap::value_type(t.get().get_node(), i);
};

DocOrderMerge::DocOrderMerge(unsigned int _size, const phop::TupleList& _tin)
    : ITupleOperator(OPINFO_REF, _size), tin(_tin)
{
    mergeHeap.reserve(_tin.size());
};

void DocOrderMerge::do_next()
{
    if (!initialized) {
        for (TupleList::size_type i = 0; i < tin.size(); ++i) {
            const MappedTupleIn & t = tin.at(i);
            t->next();

            if (!t.eos()) {
                mergeHeap.push_back(getMergeValue(t, i));
            };
        };
        std::make_heap(mergeHeap.begin(), mergeHeap.end(), NidStringCmp());

        initialized = true;
    }

    if (mergeHeap.empty()) {
        seteos();
    };

    TupleList::size_type idx = mergeHeap.front().second;
    const MappedTupleIn & t = tin.at(idx);
    t.assignTo(value());

    std::pop_heap(mergeHeap.begin(), mergeHeap.end(), NidStringCmp());
    mergeHeap.pop_back();

    t->next();

    if (!t.eos()) {
        mergeHeap.push_back(getMergeValue(t, idx));
        std::push_heap(mergeHeap.begin(), mergeHeap.end(), NidStringCmp());
    };
};

void DocOrderMerge::reset()
{
    for (TupleList::const_iterator it = tin.begin(); it != tin.end(); ++it) {
        (*it)->reset();
    };

    mergeHeap.clear();
    initialized = false;
}

TupleSort::TupleSort( unsigned int _size, MappedTupleIn _in, ICollationTupleSerializer* _order)
    : UnaryTupleOperator(OPINFO_REF, _size, _in),
        initialized(false), order(_order), _sorted_sequence(NULL)
{

}

TupleSort::~TupleSort()
{
    delete order;
    delete _sorted_sequence;
}

void TupleSort::do_next()
{
    if (!initialized) {
        if (_sorted_sequence == NULL) {
            _sorted_sequence = new SortedSequence(order);
        };

        in->next();
        while (!in.eos()) {
            _sorted_sequence->add(in->get());
            in->next();
        };

        _sorted_sequence->sort();

        initialized = true;
    };

    if (!value().eos) {
        _sorted_sequence->next(value());
    }
}

void TupleSort::reset()
{
    phop::UnaryTupleOperator::reset();
    order->setCollationHandler(block->context->collation);
    initialized = false;
}

TupleJoinFilter::TupleJoinFilter(unsigned int _size, const phop::MappedTupleIn& _left, const phop::MappedTupleIn& _right, const TupleCellComparison& _tcc)
    : BinaryTupleOperator(OPINFO_REF, _size, _left, _right),
      initialized(false), tcc(_tcc), left_seq(NULL), right_seq(NULL),
      left_seq_pos(0), right_seq_pos(0),
      initial_left_seq_pos(0), initial_right_seq_pos(0),
      initialLeft(_left->_tsize()), initialRight(_right->_tsize()),
      leftValue(_left->_tsize()), rightValue(_right->_tsize()),
      step_state(step_both)
{
    left_seq = new sequence(_left->_tsize());
    right_seq = new sequence(_right->_tsize());
}

TupleJoinFilter::~TupleJoinFilter()
{
    delete left_seq;
    delete right_seq;
}

inline
void getValue(const MappedTupleIn & tin, sequence * seq, tuple & value, size_t pos)
{
    if ((size_t) (seq->size()) <= pos) {
        tin->next();
        value.copy(tin->get());
        seq->add(value);
    } else {
        seq->get(value, pos);
    }
};

void TupleJoinFilter::do_next()
{
    if (!initialized) {
        getValue(left, left_seq, initialLeft, initial_left_seq_pos);
        getValue(right, right_seq, initialRight, initial_right_seq_pos);

        leftValue.copy(initialLeft);
        rightValue.copy(initialRight);
        
        initialized = true;
    };

    bool found = false;

    do {
        if (initialLeft.is_eos()) {
            seteos();
            return;
        };

        if (initialRight.is_eos()) {
            seteos();
            return;
        };


        if (leftValue.eos || rightValue.eos) {
            step_state = step_both;
        } else
          if (tcc.satisfy(leftValue[left.offs], rightValue[right.offs]))
        {
            left.tupleAssignTo(value(), leftValue);
            right.tupleAssignTo(value(), rightValue);

            step_state = step_right;
            found = true;
        };

        switch (step_state) {
          case step_right:
            right_seq_pos++;
            getValue(right, right_seq, rightValue, right_seq_pos);
            step_state = step_left;
            break;
          case step_left:
            left_seq_pos++;
            getValue(left, left_seq, leftValue, left_seq_pos);
            step_state = step_both;
            break;
          case step_both:
            if (tcc.less(initialLeft[left.offs], initialRight[right.offs])) {
                initial_left_seq_pos++;
                getValue(left, left_seq, initialLeft, initial_left_seq_pos);
            } else {
                initial_right_seq_pos++;
                getValue(right, right_seq, initialRight, initial_right_seq_pos);
            };
            leftValue.copy(initialLeft);
            left_seq_pos = initial_left_seq_pos;
            rightValue.copy(initialRight);
            right_seq_pos = initial_right_seq_pos;
            break;
        };
    } while (!found);
}

void TupleJoinFilter::reset()
{
    phop::BinaryTupleOperator::reset();

    initialized = false;
    tcc.handler = block->context->collation;

    left_seq->clear();
    right_seq->clear();

    left_seq_pos = 0;
    initial_left_seq_pos = 0;
    right_seq_pos = 0;
    initial_right_seq_pos = 0;
    
    step_state = step_both;
}


TuplePredicateFilter::TuplePredicateFilter(const phop::MappedTupleIn& _in, const ValueFunction& _vcc)
    : UnaryTupleOperator(OPINFO_REF, _in.tmap.size(), _in), vcc(_vcc)
{
}

void TuplePredicateFilter::reset()
{
    phop::UnaryTupleOperator::reset();
    vcc.handler = block->context->collation;
}

void TuplePredicateFilter::do_next()
{
    do {
        in->next();

        if (in.eos()) {
            seteos();
            return;
        };

        if (vcc.evaluate(in->get()).get_xs_boolean()) {
            in.assignTo(value());
            return;
        };
    } while (!in.eos());
}

XmlConstructor & TupleJoinFilter::__toXML(XmlConstructor & producer) const
{
    return producer;
};

XmlConstructor & TuplePredicateFilter::__toXML(XmlConstructor & producer) const
{
    return producer;
};

XmlConstructor & TupleSort::__toXML(XmlConstructor & producer) const
{
    return producer;
};

XmlConstructor & DocOrderMerge::__toXML(XmlConstructor & producer) const
{
    for (TupleList::size_type i = 0; i < tin.size(); ++i) {
        tin.at(i)->toXML(producer);
    };

    return producer;
};


