#ifndef SCANS_H
#define SCANS_H

#include "tr/opt/OptTypes.h"
#include "tr/opt/SequenceModel.h"
#include "tr/opt/graphs/Predicates.h"
#include "tr/opt/algorithms/ComparisonOperation.h"

namespace phop {

struct IFunctionOpInstance;

class SchemaScan : public ITupleOperator {
private:
    std::vector<xptr> _cache;
    std::vector<xptr>::const_iterator _cachePtr;
protected:
    unsigned _idx;

    schema_node_cptr snode;
    xptr currentBlock;

    void scan();
    virtual void do_next();
public:
    OPINFO_DECL(0x201)
    
    SchemaScan(schema_node_cptr _snode, unsigned size, unsigned idx);

    virtual void reset();
};

class SchemaValueScan : public ITupleOperator {
    Node currentNode;
    schema_node_cptr snode;
    TupleCellComparison tcmpop;
    opt::MemoryTupleSequencePtr sequence;

    unsigned left;
    unsigned right;
    
    virtual void do_next();
public:
    OPINFO_DECL(0x202)

    SchemaValueScan(
        schema_node_cptr _snode,
        const TupleCellComparison & _tcmpop,
        opt::MemoryTupleSequencePtr _sequence,
        unsigned size, unsigned left, unsigned right);

    virtual void reset();
};

class NestedEvaluation : public ITupleOperator {
private:
    Operators::size_type nestedOperatorIdx;
protected:
    phop::TupleIn in;
    IValueOperator * nestedOperator;
    unsigned resultIdx;
    
    virtual void do_next();
public:
    OPINFO_DECL(0x204)

    NestedEvaluation(const phop::TupleIn& _in, IValueOperator * _op, unsigned _size, unsigned _resultIdx);
    
    virtual void reset();
};


class BogusConstSequence : public ITupleOperator {
protected:
    counted_ptr<opt::MemoryTupleSequence> sequence;
    unsigned resultIdx;
    unsigned idx;

    virtual void do_next();
public:
    OPINFO_DECL(0x210)

    BogusConstSequence(counted_ptr<opt::MemoryTupleSequence> _sequence, unsigned _size, unsigned _resultIdx);

    virtual void reset();
};

class CachedNestedLoop : public BinaryTupleOperator {
public:
    enum flags_t { strict_output = 0x01, };
protected:
    TupleCellComparison tcmpop;
    bool cacheFilled;
    flags_t flags;

    std::vector<tuple_cell> nestedSequenceCache;
    std::vector<tuple_cell>::size_type nestedIdx;

    virtual void do_next();
public:
    OPINFO_DECL(0x212)

    CachedNestedLoop(unsigned _size,
        const MappedTupleIn & _left,  const MappedTupleIn & _right,
        const TupleCellComparison & _tcmpop, flags_t _flags);

    virtual void reset();
};

/*
class FunctionOp : public ITupleOperator {
protected:
    IFunctionOpInstance * func;
    phop::MappedTupleIn in;
    unsigned resultIdx;

    virtual void do_next();
public:
    OPINFO_DECL(0x214)

    FunctionOp(const phop::MappedTupleIn& _in, unsigned _size, unsigned _resultIdx, IFunctionOpInstance * _inst);

    virtual void reset();
};
*/
}

#endif /* SCANS_H */
