#ifndef SCANS_H
#define SCANS_H

#include "SequenceModel.h"
#include "SequenceHelpers.h"

#include "tr/opt/OptTypes.h"
#include "tr/opt/alg/Predicates.h"

namespace phop {

class SchemaScan : public IValueOperator {
private:
    std::vector<xptr> _cache;
    std::vector<xptr>::const_iterator _cachePtr;
protected:
    schema_node_cptr snode;
    xptr currentBlock;

    void scan();
    virtual void do_next();
public:
    OPINFO_DECL(0x201)
    
    SchemaScan(schema_node_cptr _snode);

    virtual void reset();
};

class NestedEvaluation : public ITupleOperator {
protected:
    phop::TupleIn in;
    IValueOperator * nestedOperator;
    
    virtual void do_next();
public:
    OPINFO_DECL(0x204)

    NestedEvaluation(const phop::TupleIn& _in, IValueOperator * _op);
    
    virtual void reset();
    virtual void setContext(ExecutionContext* __context);
};


class TupleFilter : public UnaryTupleOperator {
protected:
    TupleComparison * tcmpop;
};

class ItemFilter : public ItemOperator {
protected:
    TupleValueComparison * tcmpop;
};

class BogusConstSequence : public IValueOperator {
protected:
    counted_ptr<MemoryTupleSequence> sequence;

    virtual void do_next();
public:
    OPINFO_DECL(0x210)

    BogusConstSequence(counted_ptr<MemoryTupleSequence> _sequence);
};

class CachedNestedLoop : public BinaryTupleOperator {
public:
    enum flags_t { strict_output = 0x01, };
protected:
    TupleValueComparison * tcmpop;
    bool cacheFilled;
    flags_t flags;

    std::vector<tuple_cell> nestedSequenceCache;
    std::vector<tuple_cell>::size_type nestedIdx;

    virtual void do_next();
public:
    OPINFO_DECL(0x212)

    CachedNestedLoop(unsigned _size, const MappedTupleIn & _left,  const MappedTupleIn & _right, TupleValueComparison * _tcmpop, flags_t _flags);

    virtual void reset();
};



}

#endif /* SCANS_H */
