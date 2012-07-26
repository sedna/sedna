#ifndef JOINS_H
#define JOINS_H

#include "tr/opt/algorithms/SequenceModel.h"
#include "tr/opt/algorithms/ComparisonOperation.h"
#include "tr/opt/algorithms/ValueFunction.h"

#include "tr/nid/nidstring.h"

class ICollationTupleSerializer;
class SortedSequence;

namespace phop {

typedef std::vector<MappedTupleIn> TupleList;
typedef std::vector< std::pair<NidString, TupleList::size_type> > NIDMergeHeap;

class DocOrderMerge : public ITupleOperator {
    RTTI_DECL(sequence_operator_DocOrderMerge, ITupleOperator)
private:
    bool initialized;
    TupleList tin;
    NIDMergeHeap mergeHeap;
protected:
    virtual void do_next();
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
public:
    DocOrderMerge(unsigned int _size, const TupleList & _tin);
    virtual void reset();
};

class TupleSort : public UnaryTupleOperator {
    RTTI_DECL(sequence_operator_TupleSort, UnaryTupleOperator)
private:
    bool initialized;
protected:
    virtual XmlConstructor & __toXML(XmlConstructor &) const;
protected:
    ICollationTupleSerializer * order;
    SortedSequence * _sorted_sequence;

    virtual void do_next();
public:
    TupleSort(unsigned int _size, MappedTupleIn _in, ICollationTupleSerializer * _order);
    virtual ~TupleSort();

    virtual void reset();
};

class TupleJoinFilter : public BinaryTupleOperator {
    RTTI_DECL(sequence_operator_TupleJoinFilter, BinaryTupleOperator)

    enum step_t {
        step_both = 0,
        step_left = 1,
        step_right = 2,
    };
protected:    
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
private:
    bool initialized;
    TupleCellComparison tcc;
    sequence * left_seq;
    sequence * right_seq;

    size_t left_seq_pos;
    size_t right_seq_pos;

    size_t initial_left_seq_pos;
    size_t initial_right_seq_pos;

    tuple initialLeft, initialRight;
    tuple leftValue, rightValue;

    step_t step_state;

    virtual void do_next();
public:
    TupleJoinFilter(unsigned int _size, const phop::MappedTupleIn& _left, const phop::MappedTupleIn& _right, const TupleCellComparison & tcc);
    virtual ~TupleJoinFilter();

    virtual void reset();
};

class TuplePredicateFilter : public UnaryTupleOperator {
    RTTI_DECL(sequence_operator_TuplePredicateFilter, UnaryTupleOperator)
protected:
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
private:
    ValueFunction vcc;
    virtual void do_next();
public:
    TuplePredicateFilter(const phop::MappedTupleIn& _in, const ValueFunction& _vcc);
    virtual void reset();
};

}

#endif /* PLAN_EXECUTION_H */
