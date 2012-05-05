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
    SchemaScan(schema_node_cptr _snode)
      : IValueOperator(), _cachePtr(_cache.begin()), snode(_snode), currentBlock(XNULL)
    {  };

    virtual void reset();
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
    BogusConstSequence(counted_ptr<MemoryTupleSequence> _sequence);
};

class CachedNestedLoop : public BinaryTupleOperator {
public:
    enum flags_t { strict_output = 0x01, };
protected:
    TupleComparison * tcmpop;
    bool cacheFilled;
    flags_t flags;

    std::vector<tuple_cell> nestedSequenceCache;
    std::vector<tuple_cell>::size_type nestedIdx;

    virtual void do_next();
public:
    CachedNestedLoop(TupleIn _left, TupleIn _right, TupleComparison * _tcmpop, flags_t _flags)
        : BinaryTupleOperator(_left, _right), tcmpop(_tcmpop), flags(_flags) { };

    virtual void reset();
};

class VPath : public UnaryTupleOperator {
protected:
    pe::AtomizedPath path;

    virtual void do_next();
public:
    VPath(unsigned int _size, MappedTupleIn _in, const pe::AtomizedPath & _path)
        : UnaryTupleOperator(_size, _in), path(_path) {};

    virtual void reset();
};

}

/*
struct NodeTraverse : public ITupleOperator {
};

*/


/*

struct AbsPath : public ITupleOperator {
    pe::NodeIterator iterator;
    bool docOrder;

    AbsPath(const pe::Path& path, bool _docOrder = false) : ITupleOperator(1), path(pe), docOrder(_docOrder) {};

    virtual bool open() {
        iterator = path.execute(XNULL);
    }

    virtual bool next() {
        Node result;

        result = iterator.next();

        if (result.isNull()) {
            body.set_eos();
        }
    };
};
*/

#endif /* SCANS_H */
