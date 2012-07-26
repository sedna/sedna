#ifndef SCANS_H
#define SCANS_H

#include "tr/opt/OptTypes.h"
#include "tr/opt/algorithms/SequenceModel.h"

#include "tr/opt/graphs/Predicates.h"
#include "tr/opt/algorithms/ComparisonOperation.h"
#include "tr/opt/algorithms/VariableMap.h"

namespace phop {

// TODO swap size and idx in constructors, initialize them with 1

class SchemaScan : public ITupleOperator {
    RTTI_DECL(sequence_operator_SchemaScan, ITupleOperator)
private:
    std::vector<xptr> _cache;
    std::vector<xptr>::const_iterator _cachePtr;
protected:
    unsigned _idx;

    schema_node_cptr snode;
    xptr currentBlock;

    void scan();

    virtual void do_next();
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
public:
    SchemaScan(schema_node_cptr _snode, unsigned size, unsigned idx);

    virtual void reset();
};

class SchemaValueScan : public ITupleOperator {
    RTTI_DECL(sequence_operator_SchemaValueScan, ITupleOperator)

    Node currentNode;
    schema_node_cptr snode;
    TupleCellComparison tcmpop;
    opt::MemoryTupleSequencePtr sequence;

    unsigned left;
    unsigned right;
protected:
    virtual void do_next();
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;    
public:
    SchemaValueScan(
        schema_node_cptr _snode,
        const TupleCellComparison & _tcmpop,
        opt::MemoryTupleSequencePtr _sequence,
        unsigned size, unsigned left, unsigned right);

    virtual void reset();
};

class VariableIn : public ITupleOperator {
    RTTI_DECL(sequence_operator_VariableIn, ITupleOperator)
private:
    opt::TupleId tid;
    executor::VarIterator varIt;
    unsigned idx;
protected:
    virtual void do_next();
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
public:
    VariableIn(opt::TupleId _tid, unsigned size, unsigned idx);

    virtual void reset();
};

class NestedEvaluation : public ITupleOperator {
    RTTI_DECL(sequence_operator_NestedEvaluation, ITupleOperator)
private:
    Operators::size_type nestedOperatorIdx;
protected:
    phop::TupleIn in;
    IValueOperator * nestedOperator;
    unsigned resultIdx;
    
    virtual void do_next();
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
public:
    NestedEvaluation(const phop::TupleIn& _in, IValueOperator * _op, unsigned _size, unsigned _resultIdx);
    
    virtual void reset();
};


class BogusConstSequence : public ITupleOperator {
    RTTI_DECL(sequence_operator_BogusConstSequence, ITupleOperator)
protected:
    counted_ptr<opt::MemoryTupleSequence> sequence;
    unsigned resultIdx;
    unsigned idx;

    virtual void do_next();
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
public:
    BogusConstSequence(counted_ptr<opt::MemoryTupleSequence> _sequence, unsigned _size, unsigned _resultIdx);

    virtual void reset();
};

class CachedNestedLoop : public BinaryTupleOperator {
    RTTI_DECL(sequence_operator_CachedNestedLoop, BinaryTupleOperator)
public:
    enum flags_t { strict_output = 0x01, };
protected:
    TupleCellComparison tcmpop;
    bool cacheFilled;
    flags_t flags;

    std::vector<tuple_cell> nestedSequenceCache;
    std::vector<tuple_cell>::size_type nestedIdx;

    virtual void do_next();
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
public:
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
