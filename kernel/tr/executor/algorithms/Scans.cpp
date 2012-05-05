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

    unsigned i = _cacheSize;
    while (i > 0 && _cachePtr != _cache.end()) {
        push(); value() = *_cachePtr++; --i;
    }
}

void BogusConstSequence::do_next() {
    unsigned i = _cacheSize;
    while (i > 0 && _seq_ptr != sequence->end()) {
        push(); value() = *_seq_ptr++; --i;
    }
}

void BogusConstSequence::reset()
{
    _seq_ptr = sequence->begin();
}

void CachedNestedLoop::do_next()
{
    if (!cacheFilled) {
        while (!right.eos()) {
            nestedSequenceCache.push_back(right.get());
            right.op->next(_context);
        };

        cacheFilled = true;
    };

    left->next(_context);
    tuple_cell lTuple = left.get();

    for (std::vector<tuple_cell>::const_iterator it = nestedSequenceCache.begin(); it != nestedSequenceCache.end(); it++) {
        if ((*tcmpop)(lTuple, *it)) {
            push();
            left.assignTo(value());
            right.assignTo(value());

            if (flags & strict_output == 0) {
                break;
            };
        }
    };
}

void CachedNestedLoop::reset()
{
    BinaryTupleOperator::reset();
    cacheFilled = false;
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
