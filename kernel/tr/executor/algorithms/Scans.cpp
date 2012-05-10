#include "Scans.h"
#include "SequenceHelpers.h"

#include "tr/executor/xpath/XPathLookup.h"

#include "tr/executor/base/XPath.h"
#include "tr/executor/base/XPathOnSchema.h"
#include "tr/executor/base/PPUtils.h"

using namespace phop;

OPINFO_DEF(SchemaScan)
OPINFO_DEF(BogusConstSequence)
OPINFO_DEF(CachedNestedLoop)
OPINFO_DEF(NestedEvaluation)

SchemaScan::SchemaScan(schema_node_cptr _snode)
    : IValueOperator(OPINFO_REF),
        _cachePtr(_cache.begin()), snode(_snode), currentBlock(XNULL)
{

}

void SchemaScan::reset()
{
    currentBlock = snode->bblk;
    _cache.clear();
    _cachePtr = _cache.end();
}

void SchemaScan::scan()
{
    if (currentBlock == XNULL) {
        _cache.clear();
        seteos();
    } else {
        NodeBlockHeader hdr(checkp(currentBlock));
        Node node = hdr.getFirstDescriptor();

        _cache.clear();
        _cache.reserve(hdr.getNodeCount());
        while (!node.isNull()) {
            _cache.push_back(node.getPtr());
            node = node.getNext();
        }

        currentBlock = hdr.getNextBlock();
    };

    _cachePtr = _cache.begin();
}

void SchemaScan::do_next()
{
    if (_cache.empty() || _cachePtr == _cache.end()) {
        scan();
    }

    while (_cachePtr != _cache.end()) {
        push(tuple_cell::node(*(_cachePtr++)));
    }
}

BogusConstSequence::BogusConstSequence(counted_ptr< MemoryTupleSequence > _sequence)
  : IValueOperator(OPINFO_REF), sequence(_sequence)
{
  
}

void BogusConstSequence::do_next()
{
    for (MemoryTupleSequence::const_iterator it = sequence->begin(); it != sequence->end(); ++it) {
        push(*it);
    };

    push(tuple_cell());
}

CachedNestedLoop::CachedNestedLoop(unsigned _size, const MappedTupleIn & _left, const MappedTupleIn & _right, TupleValueComparison* _tcmpop, CachedNestedLoop::flags_t _flags)
  : BinaryTupleOperator(OPINFO_REF, _size, _left, _right),
    tcmpop(_tcmpop), cacheFilled(false), flags(_flags)
{
  
}


void CachedNestedLoop::do_next()
{
    if (!cacheFilled) {
        while (!right.eos()) {
            nestedSequenceCache.push_back(right.get());
            right.op->next();
        };

        nestedIdx = nestedSequenceCache.size();
        cacheFilled = true;

        if (nestedSequenceCache.size() == 0) {
            seteos();
        };
    };

    do {
        if (nestedIdx >= nestedSequenceCache.size()) {
            left->next();

            if (left.eos()) {
                seteos();
                return;
            };

            nestedIdx = 0;
        };

        tuple_cell lTuple = left.get();

        while (nestedIdx < nestedSequenceCache.size()) {
            nestedIdx++;

            if ((*tcmpop)(lTuple, nestedSequenceCache.at(nestedIdx-1))) {
                left.assignTo(value());
                right.assignTo(value());

                if ((flags & strict_output) == 0) {
                    nestedIdx = nestedSequenceCache.size();
                };
                
                return;
            }
        };
    } while (!left.eos());
}

void CachedNestedLoop::reset()
{
    BinaryTupleOperator::reset();
    cacheFilled = false;
    nestedIdx = nestedSequenceCache.size();
}


NestedEvaluation::NestedEvaluation(const phop::TupleIn& _in, IValueOperator* _op)
  : ITupleOperator(OPINFO_REF, _in.op->_tsize()), in(_in), nestedOperator(_op)
{
}

void NestedEvaluation::do_next()
{
    nestedOperator->next();

    if (nestedOperator->get().is_eos()) {
        seteos();
        return;
    };

    in.copyTo(value());
    value().cells[in.offs] = nestedOperator->get();
}

void NestedEvaluation::reset()
{
    phop::ITupleOperator::reset();
    nestedOperator->reset();
}

void NestedEvaluation::setContext(ExecutionContext* __context)
{
    phop::IOperator::setContext(__context);
    nestedOperator->setContext(__context);
}
