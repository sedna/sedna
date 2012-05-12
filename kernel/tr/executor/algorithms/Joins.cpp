#include "Joins.h"

#include "tr/executor/base/tuple.h"
#include "tr/executor/base/ITupleSerializer.h"
#include "tr/executor/base/SortedSequence.h"
#include "tr/executor/base/sequence.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/structures/producer.h"

#include "tr/nid/nidalloc.h"
#include "tr/nid/nid.h"

#include "SequenceHelpers.h"

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

TupleSort::TupleSort(unsigned int _size, MappedTupleIn _in, ITupleSerializer* _order)
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
    initialized = false;
}

TupleJoinFilter::TupleJoinFilter(unsigned int _size, const phop::MappedTupleIn& _left, const phop::MappedTupleIn& _right, const TupleCellComparison& _tcc)
    : BinaryTupleOperator(OPINFO_REF, _size, _left, _right),
      initialized(false), tcc(_tcc), seq(NULL), seq_pos(0)
{
    seq = new sequence(_right->_tsize());
}

TupleJoinFilter::~TupleJoinFilter()
{
    delete seq;
}

void TupleJoinFilter::do_next()
{
    if (!initialized) {
        left->next();
        right->next();

        initialized = true;
    };

    if (left.eos()) {
        seteos();
        return;
    };

    if (right.eos()) {
        seteos();
        return;
    };

    bool found = false;

    // TODO: FIXME: nested loop with equality case!
    do {
/*      
        if (seq_pos == seq->size()) {
            seq->add(right->get());
            right->next();
        };
*/

        if (tcc.satisfy(left.get(), right.get())) {
            left.assignTo(value());
            right.assignTo(value());
            found = true;
        }

        if (tcc.less(left.get(), right.get())) {
            left->next();
        } else {
            right->next();
        };
    } while (!found);
}

void TupleJoinFilter::reset()
{
    phop::BinaryTupleOperator::reset();

    initialized = false;
    seq->clear();
    seq_pos = 0;
}

TuplePredicateFilter::TuplePredicateFilter(const phop::MappedTupleIn& _in, const ValueFunction& _vcc)
    : UnaryTupleOperator(OPINFO_REF, _in.tmap.size(), _in), vcc(_vcc)
{
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


IElementProducer * TupleJoinFilter::__toXML(IElementProducer * producer) const
{
    return producer;
};

IElementProducer * TuplePredicateFilter::__toXML(IElementProducer * producer) const
{
    return producer;
};

IElementProducer * TupleSort::__toXML(IElementProducer * producer) const
{
    return producer;
};

IElementProducer * DocOrderMerge::__toXML(IElementProducer * producer) const
{
    for (TupleList::size_type i = 0; i < tin.size(); ++i) {
        tin.at(i)->toXML(producer);
    };

    return producer;
};

