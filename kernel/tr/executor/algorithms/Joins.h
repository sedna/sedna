#ifndef JOINS_H
#define JOINS_H

#include "SequenceModel.h"

#include "tr/nid/nidstring.h"
#include "tr/executor/algorithms/Comparison.h"
#include "tr/executor/algorithms/ValueFunction.h"

class SortedSequence;
class ITupleSerializer;

namespace phop {

typedef std::vector<MappedTupleIn> TupleList;
typedef std::vector< std::pair<NidString, TupleList::size_type> > NIDMergeHeap;

class DocOrderMerge : public ITupleOperator {
private:
    bool initialized;
    TupleList tin;
    NIDMergeHeap mergeHeap;
protected:
    virtual void do_next();
public:
    OPINFO_DECL(0x301)
  
    DocOrderMerge(unsigned int _size, const TupleList & _tin);
    virtual void reset();
};

class TupleSort : public UnaryTupleOperator {
private:
    bool initialized;
protected:
    ITupleSerializer * order;
    SortedSequence * _sorted_sequence;

    virtual void do_next();
public:
    OPINFO_DECL(0x302)

    TupleSort(unsigned int _size, MappedTupleIn _in, ITupleSerializer * _order);
    virtual ~TupleSort();
    virtual void reset();
};

class TupleJoinFilter : public BinaryTupleOperator {
private:
    bool initialized;
    TupleCellComparison tcc;
    sequence * seq;
    size_t seq_pos;

    virtual void do_next();
public:
    OPINFO_DECL(0x304)

    TupleJoinFilter(unsigned int _size, const phop::MappedTupleIn& _left, const phop::MappedTupleIn& _right, const TupleCellComparison & tcc);
    virtual ~TupleJoinFilter();
    virtual void reset();
};

class TuplePredicateFilter : public UnaryTupleOperator {
private:
    ValueFunction vcc;
    virtual void do_next();
public:
    OPINFO_DECL(0x305)

    TuplePredicateFilter(const phop::MappedTupleIn& _in, const ValueFunction& _vcc);
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
