#ifndef JOINS_H
#define JOINS_H

#include "SequenceModel.h"

class SortedSequence;
class ITupleSerializer;

namespace phop {

typedef std::vector<MappedTupleIn> TupleList;

typedef std::vector< std::pair<std::string, TupleList::size_type> > NumberingSchemeMergeHeap;

class DocOrderMerge : public ITupleOperator {
private:
    bool initialized;
    TupleList tin;
    NumberingSchemeMergeHeap mergeHeap;
protected:
    virtual void do_next();
public:
    DocOrderMerge(unsigned int _size, const TupleList & _tin)
        : ITupleOperator(_size), tin(_tin)
    {
        mergeHeap.reserve(_tin.size());
    };

    virtual void reset();
};

class ValueSort : public UnaryTupleOperator {
private:
    bool initialized;
protected:
    ITupleSerializer * order;
    SortedSequence * _sorted_sequence;

    virtual void do_next();
public:
    ValueSort(unsigned int _size, MappedTupleIn _in, ITupleSerializer * _order)
        : UnaryTupleOperator(_size, _in), order(_order), _sorted_sequence(NULL) {};

    virtual ~ValueSort();

    virtual void reset();
};

class TupleComparisonFilter : public ITupleOperator {
};

}

/*
class GSMJMergeComparator {
public:
    bool stepLeft;
    bool stepRight;

    virtual bool compare(tuple left, tuple right) = 0;
};

AbstractSequence * createEqualValueJoin();
*/

#endif /* PLAN_EXECUTION_H */
