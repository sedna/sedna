#include "Joins.h"

#include "tr/executor/base/tuple.h"
#include "tr/executor/base/ITupleSerializer.h"
#include "tr/executor/base/SortedSequence.h"

#include "SequenceHelpers.h"

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
