#ifndef JOINS_H
#define JOINS_H

#include "SequenceModel.h"

#include "tr/nid/nidstring.h"

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

class ValueSort : public UnaryTupleOperator {
private:
    bool initialized;
protected:
    ITupleSerializer * order;
    SortedSequence * _sorted_sequence;

    virtual void do_next();
public:
    OPINFO_DECL(0x302)
    
    ValueSort(unsigned int _size, MappedTupleIn _in, ITupleSerializer * _order);

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
