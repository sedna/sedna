#include "Scans.h"
#include "SequenceHelpers.h"

#include "tr/executor/xpath/XPathLookup.h"

#include "tr/executor/base/XPath.h"
#include "tr/executor/base/XPathOnSchema.h"
#include "tr/executor/base/PPUtils.h"

using namespace phop;

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

        currentBlock = hdr.;
    };

    _cachePtr = _cache.begin();
}

void SchemaScan::do_next()
{
    if (_cache.empty() || _cachePtr == _cache.end()) {
        scan();
    }

    while (_cachePtr != _cache.end()) {
        push(*(_cachePtr++));
    }
}

BogusConstSequence::BogusConstSequence(counted_ptr< MemoryTupleSequence > _sequence)
  : IValueOperator(), sequence(_sequence)
{ }

void BogusConstSequence::do_next() {
    for (MemoryTupleSequence::const_iterator it = sequence->begin(); it != sequence->end(); ++it) {
        push(*it);
    };

    push(tuple_cell());
}

void CachedNestedLoop::do_next()
{
    if (!cacheFilled) {
        while (!right.eos()) {
            nestedSequenceCache.push_back(right.get());
            right.op->next(_context);
        };

        nestedIdx = nestedSequenceCache.size();
        cacheFilled = true;

        if (nestedSequenceCache.size() == 0) {
            seteos();
        };
    };

    do {
        if (nestedIdx >= nestedSequenceCache.size()) {
            left->next(_context);

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

void VPath::do_next()
{
    path.begin();
}

void VPath::reset()
{
    phop::UnaryTupleOperator::reset();
}


/*
struct PathStep : public AbstractSequence {
    SequenceElement inSequence;
    int outTuple;
    xpe::XPathLookup path;
    int state;
    xpe::NodeIterator iterator;

    PathStep(SequenceElement in, const xpe::Path * pe)
    : AbstractSequence(in.seq->body.cells_number + 1), inSequence(in), path(pe), state(0)
    {
        outTuple = in.seq->body.cells_number;
        path.compile();
    };

    virtual bool open() {
        state = 0;
    }

    virtual bool next() {
        tuple_cell node;
        Node result;

        do {
            switch(state) {
                case 0:
                    inSequence.seq->next();
                    copy_tuple(body, inSequence.seq->get(), 0);

                    if (body.is_eos()) {
                        return false;
                    }

                    node = body.cells[inSequence.pos];

                    if (!node.is_node()) {
                        U_ASSERT(false);
                        // throw error
                    }

                    iterator = path.execute(node.get_node());

                    ++state;
                case 1:
                    result = iterator.next();

                    if (result.isNull()) {
                        state = 0;
                        continue;
                    }

                    body.cells[outTuple].set_node(result.getPtr());

                    return true;
                default:
                    U_ASSERT(false);
                    state = 0;
            };
        } while (true);
    };
};
*/
