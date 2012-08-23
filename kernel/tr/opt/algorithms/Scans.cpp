#include "Scans.h"

#include "tr/opt/path/XPathLookup.h"

#include "tr/executor/base/XPath.h"
#include "tr/executor/base/XPathOnSchema.h"
#include "tr/executor/base/PPUtils.h"

#include "tr/models/XmlConstructor.h"

#include "tr/opt/functions/Functions.h"
#include "tr/opt/evaluation/DynamicContext.h"

using namespace phop;
using namespace opt;

RTTI_DEF(SchemaScan)
RTTI_DEF(SchemaValueScan)
RTTI_DEF(VariableIn)

RTTI_DEF(BogusConstSequence)
RTTI_DEF(CachedNestedLoop)

SchemaScan::SchemaScan(schema_node_cptr _snode, unsigned int size, unsigned int idx)
    : ITupleOperator(SELF_RTTI_REF, size),
        _cachePtr(_cache.begin()), _idx(idx), snode(_snode), currentBlock(XNULL)
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
    do {
        if (_cache.empty() || _cachePtr == _cache.end()) {
            scan();
        } else {
            value().cells[_idx] = tuple_cell::node(*(_cachePtr++));
            return;
        }
    } while (_cachePtr != _cache.end());

    seteos();
}

SchemaValueScan::SchemaValueScan(
    schema_node_cptr _snode,
    const TupleCellComparison& _tcmpop,
    MemoryTupleSequencePtr _sequence,
    unsigned int _size, unsigned int _left, unsigned int _right)

    : ITupleOperator(SELF_RTTI_REF, _size),
      currentNode(XNULL), snode(_snode),
      tcmpop(_tcmpop), sequence(_sequence),
      left(_left), right(_right)
{

}


void SchemaValueScan::do_next()
{
    if (currentNode == XNULL) {
        NodeBlockHeader header(checkp(snode->bblk));
        currentNode = header.getFirstDescriptor();
    } else {
        currentNode = NodeIteratorForeward::nextNode(currentNode.getPtr());
    };

    while (!currentNode.isNull()) {
        for (MemoryTupleSequence::const_iterator it = sequence->begin(); it != sequence->end(); ++it) {
            tuple_cell leftNode = tuple_cell::node(currentNode.getPtr());

            if (tcmpop.satisfy(leftNode, *it)) {
                value().cells[left] = leftNode;

                if (right < _tsize()) {
                    value().cells[right] = *it;
                }

                return;
            }
        };

        currentNode = NodeIteratorForeward::nextNode(currentNode.getPtr());
    };

    seteos();
}

void SchemaValueScan::reset()
{
    phop::ITupleOperator::reset();
    currentNode = XNULL;
}

phop::VariableIn::VariableIn(TupleId _tid, unsigned int size, unsigned int _idx)
  : ITupleOperator(SELF_RTTI_REF, size), tid(_tid), varIt(NULL), idx(_idx)
{
    
}

void phop::VariableIn::do_next()
{
   // TODO : remove;
    if (varIt.info() == NULL) {
        varIt = block->dynamicContext->variables->getIterator(tid);
    };

    tuple_cell tc = varIt.next();
    
    if (tc.is_eos()) {
        seteos();
    };

    value().cells[idx] = tc;
};

void phop::VariableIn::reset()
{
    phop::ITupleOperator::reset();
    varIt = block->dynamicContext->variables->getIterator(tid);
};


BogusConstSequence::BogusConstSequence(MemoryTupleSequencePtr _sequence, unsigned _size, unsigned _resultIdx)
  : ITupleOperator(SELF_RTTI_REF, _size), sequence(_sequence), resultIdx(_resultIdx), idx(0)
{
  
}

void BogusConstSequence::reset()
{
    phop::ITupleOperator::reset();
    idx = 0;
}


void BogusConstSequence::do_next()
{
    if (idx < sequence->size()) {
        value().cells[resultIdx] = sequence->at(idx++);
    } else {
        seteos();
    }
}

CachedNestedLoop::CachedNestedLoop(
    unsigned _size,
    const MappedTupleIn & _left,
    const MappedTupleIn & _right,
    const TupleCellComparison & _tcmpop,
    CachedNestedLoop::flags_t _flags)

  : BinaryTupleOperator(SELF_RTTI_REF, _size, _left, _right),
    tcmpop(_tcmpop), cacheFilled(false), flags(_flags)
{
  
}


void CachedNestedLoop::do_next()
{
    if (!cacheFilled) {
        right.op->next();
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

            if (tcmpop.satisfy(lTuple, nestedSequenceCache.at(nestedIdx-1))) {
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

#include <sstream>

static
std::string schemaPath(schema_node_cptr snode) {
    std::stringstream path;
    std::stack<schema_node_cptr> path_sn;

    while (snode.found()) {
        path_sn.push(snode);
        snode = snode->parent;
    };

    while (!path_sn.empty()) {
        path << path_sn.top()->get_qname().getColonizedName().c_str() << "/";
        path_sn.pop();
    }

    return path.str();
};

XmlConstructor & SchemaScan::__toXML(XmlConstructor & producer) const
{
    producer.addElementValue(SE_EL_NAME("path"), schemaPath(snode));
    return  producer;
};

XmlConstructor & SchemaValueScan::__toXML(XmlConstructor & producer) const
{
    producer.addElementValue(SE_EL_NAME("path"), schemaPath(snode));

    for (MemoryTupleSequence::const_iterator it = sequence->begin(); it != sequence->end(); ++it) {
        producer.addElementValue(SE_EL_NAME("value"), *it);
    };
    
    return producer;
};

XmlConstructor & VariableIn::__toXML(XmlConstructor & producer) const
{
    producer.openElement(SE_EL_NAME("variable"));
    producer.addAttributeValue(SE_EL_NAME("variable"), tuple_cell::atomic_int(tid));
    producer.closeElement();
    
    return  producer;
};

XmlConstructor & BogusConstSequence::__toXML(XmlConstructor & producer) const
{
    for (MemoryTupleSequence::const_iterator it = sequence->begin(); it != sequence->end(); ++it) {
        producer.addElementValue(SE_EL_NAME("value"), *it);
    };
    
    return  producer;
};

XmlConstructor & CachedNestedLoop::__toXML(XmlConstructor & producer) const
{
    return BinaryTupleOperator::__toXML(producer);
};


