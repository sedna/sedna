#include "Joins.h"

#include "tr/executor/base/tuple.h"
#include "tr/executor/base/ITupleSerializer.h"
#include "tr/executor/base/SortedSequence.h"

#include "tr/nid/nidalloc.h"
#include "tr/nid/nid.h"

#include "SequenceHelpers.h"

using namespace phop;

OPINFO_DEF(DocOrderMerge)
OPINFO_DEF(ValueSort)

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

ValueSort::ValueSort(unsigned int _size, MappedTupleIn _in, ITupleSerializer* _order)
    : UnaryTupleOperator(OPINFO_REF, _size, _in),
        initialized(false), order(_order), _sorted_sequence(NULL)
{

}


ValueSort::~ValueSort()
{
    delete order;
    delete _sorted_sequence;
}

void ValueSort::do_next()
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

void ValueSort::reset()
{
    phop::UnaryTupleOperator::reset();
    initialized = false;
}




/*
struct GeneralizedHashJoin : public AbstractSequence {
//    
};

struct GeneralizedSortMergeJoin : public AbstractSequence {
    int leftCells, rightCells;
    SequenceElement leftIn, rightIn;
    SortedSequence leftSeq, rightSeq;

    scoped_ptr<GSMJMergeComparator> comparator;

    GeneralizedSortMergeJoin(
        SequenceElement left,
        SequenceElement right,
        ITupleSerializer * leftSorter,
        ITupleSerializer * rightSorter,
        GSMJMergeComparator * _comparator)

      : AbstractSequence(leftIn.seq->body.cells_number + rightIn.seq->body.cells_number),
        leftCells(leftIn.seq->body.cells_number), rightCells(rightIn.seq->body.cells_number),
        leftIn(left),
        rightIn(right),
        leftSeq(leftSorter),
        rightSeq(rightSorter),
        comparator(_comparator)
    {
    }

    ~GeneralizedSortMergeJoin() {

    }

    virtual void next() {
        // !!! TODO: If inputs are already sorted, no need to use sorted sequences

        while (leftIn.seq->next()) {
            leftSeq.add(leftIn.seq->get());
        }

        while (rightIn.seq->next()) {
            rightSeq.add(rightIn.seq->get());
        }

        leftSeq.sort();
        rightSeq.sort();

        tuple leftTuple(leftCells);
        tuple rightTuple(rightCells);

        leftSeq.next(leftTuple);
        rightSeq.next(rightTuple);

        while (!leftTuple.is_eos() || !rightTuple.is_eos()) {
            // FIXME: This is unfair!
            bool result = comparator->compare(leftTuple, rightTuple);

            if (result) {
                copy_tuple(body, leftTuple, 0);
                copy_tuple(body, rightTuple, leftCells);
            }

            if (comparator->stepLeft) {
                leftSeq.next(leftTuple);
            }

            if (comparator->stepRight) {
                rightSeq.next(rightTuple);
            }
        };
    };
};

struct ValueJoin : public AbstractSequence {
    virtual void next();
};
*/
