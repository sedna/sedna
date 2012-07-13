#include "tr/structures/portal.h"
#include "tr/executor/base/tuple.h"
#include "tr/executor/base/dynamic_context.h"
#include "tr/crmutils/serialization.h"
#include "tr/structures/schema.h"
#include "common/errdbg/d_printf.h"
#include "tr/models/SCElementProducer.h"
#include "tr/executor/base/PPBase.h"

using namespace std;
using namespace portal;

schema_node_xptr get_schema_node(const tuple_cell tc) {
    U_ASSERT(tc.is_portal());
    sequence_ptr p = tc.get_portal();
    U_ASSERT(p.pos >= 0);

    /* The following code means: take the virtual first element's tuple from sequence
    * and take the first (the only) cell from that tuple, and take xptr stored there
    */
    return ((*(p.p))[p.pos]).cells[0].get_xptr();
}


VirtualNodeIterator::VirtualNodeIterator(const tuple_cell tc)
    : atEnd(false), reader(NULL), nodeValue(tuple_cell::eos()), nodeSedna(NULL), nodeVirtual(NULL), currentNode(NULL)
{
    U_ASSERT(tc.get_atomic_type() == se_sequence_element);
    setNode(new SequenceReader(tc), XNULL /* here should be virtual root schema node */ );
}

VirtualNodeIterator::VirtualNodeIterator()
    : atEnd(true), reader(NULL), nodeValue(tuple_cell::eos()), nodeSedna(NULL), nodeVirtual(NULL), currentNode(NULL) { }

void VirtualNodeIterator::setNode(counted_ptr<SequenceReader> newReader, schema_node_cptr parent_snode)
{
    currentNode = NULL;
    reader = newReader;
    const tuple_cell tc = reader->getCell();

    if (tc.is_atomic_type(xs_string)) {
        // Text element representation
        snode = parent_snode->get_first_child(text);
    } else {
        // portal
        snode = reader->getCell().get_xptr();
        reader->next();
    }

    U_ASSERT(snode.found());

    if (snode->has_text()) {
        nodeValue = reader->getCell();
        U_ASSERT(nodeValue.is_atomic_type(xs_string));
        reader->next();
    } else {
        nodeValue = tuple_cell::eos();
    }

    if (snode->has_children()) {
        atEnd = reader->getCell().is_atomic_type(se_separator);

        if (atEnd) {
            reader->next();
        }
    } else {
        atEnd = true;
    }
};

bool VirtualNodeIterator::end()
{
    return atEnd;
}

IXDMNodeList* VirtualNodeIterator::getAllChildren()
{
    return this;
}

xmlns_ptr VirtualNodeIterator::getNamespaceValue() const
{
    return NULL_XMLNS;
}

t_item VirtualNodeIterator::getNodeKind() const
{
    return snode->type;
}

xsd::QName VirtualNodeIterator::getQName() const
{
    return snode->get_qname();
}

bool VirtualNodeIterator::hasText() const
{
    return false;
}

const text_source_t VirtualNodeIterator::getValue() const
{
    return text_source_tuple_cell(nodeValue);
}

VirtualNodeIterator::~VirtualNodeIterator() { };

IXDMNode * VirtualNodeIterator::getNode() {
    if (currentNode == NULL && !atEnd) {
        const tuple_cell tc = reader->getCell();

        if (tc.is_portal() || tc.is_atomic_type(xs_string)) {
            if (nodeVirtual.isnull()) {
                nodeVirtual = new VirtualNodeIterator();
            }

            if (tc.get_atomic_type() == se_sequence_element && !reader->sameSequence(tc.get_portal().p)) {
                U_ASSERT(false); // This is currently not implemented, so that is very unlikely going to happen
                nodeVirtual->setNode(new SequenceReader(tc), snode);
            } else {
                nodeVirtual->setNode(reader, snode);
            }

            currentNode = nodeVirtual.get();
        } else if (tc.is_node()) {
            if (nodeSedna.isnull()) {
                nodeSedna = new SednaNode(tc.get_node());
            } else {
                nodeSedna->setNode(tc.get_node());
            }

            reader->next();

            currentNode = nodeSedna.get();
        } else {
            U_ASSERT(false);
            return NULL;
        }
    };

    return currentNode;
}

bool VirtualNodeIterator::next() {
    getNode(); // This currently makes no sense. In future, this must be made to actually farward to the next node

    currentNode = NULL;

    if (atEnd) {
        return false;
    }

    atEnd = reader->getCell().is_atomic_type(se_separator);
    if (atEnd) {
        reader->next();
    }

    return !atEnd;
}

static inline
tuple create_tuple(const tuple_cell tc) { tuple t(1); t.copy(tc); return t; }

static
tuple create_schema_tuple(const schema_node_xptr snode) {
    return tuple(tuple_cell::atomic_portal(snode));
};


tuple_cell VirtualElementProducer::close()
{
    U_ASSERT(opened);
    processAtomics();

    seq->add(create_tuple(tuple_cell::atomic_se_separator()));
    el_debug_printf("[%s] insert end to %x\n", __FUNCTION__, seq);

    tuple_cell result = tuple_cell::atomic_portal(seq, index);

    if (sequenceReturnStack != NULL) {
        sequenceReturnStack->returnSequence(seq);
        seq = NULL;
        sequenceReturnStack = NULL;
    }

    closed = true;
    opened = false;

    return result;
}

VirtualElementProducer::~VirtualElementProducer()
{
    if (!closed && seq != NULL) {
        delete seq;
    }
}

VirtualElementProducer::VirtualElementProducer(const schema_node_xptr _snode, sequence * _seq, VirtualElementGenerator * owner) :
    noMoreAttributes(false), sequenceReturnStack(owner), index(0), opened(true), closed(false), seq(_seq), textAccum(1), snode(_snode)
{
    U_ASSERT(_seq != NULL);
    index = seq->size();
    seq->add(create_schema_tuple(snode.ptr()));
}

static inline
schema_node_xptr force_get_child(schema_node_cptr snode, const xsd::QName& qname, t_item t) {
    schema_node_xptr child = snode->get_first_child(qname, t);

    if (child == XNULL) {
        child = snode->add_child(qname, t);
    };

    return child;
};

IElementProducer* VirtualElementProducer::addElement(const xsd::QName& qname, xmlscm_type type)
{
    processAtomics();

    return new VirtualElementProducer(force_get_child(snode, qname, element), seq, NULL);
}

void VirtualElementProducer::processAtomics()
{
    text_source_t ts = concatTextSequence(&textAccum);
    textAccum.clear();

    if (!text_is_null(ts)) {
        seq->add(tuple(tuple_cell::atomic_any_text(xs_string, ts)));
        force_get_child(snode, xsd::QName(), text);
    }
}

tuple_cell VirtualElementProducer::addAtomic(const tuple_cell& node)
{
    textAccum.add(tuple(node));

    return tuple_cell::eos();
}

inline static
tuple string_tuple_from_text(const text_source_t value) {
    tuple_cell t(tuple_cell::atomic_any_text(xs_string, value));

    if (t.is_eos()) {
        return tuple(tuple_cell::atomic_deep(xs_string, ""));
    } else {
        return tuple(t);
    }
}

tuple_cell VirtualElementProducer::addAttribute(const xsd::QName& qname, const text_source_t value, xmlscm_type type)
{
    if (noMoreAttributes) {
        throw XQUERY_EXCEPTION(XQTY0024);
    }

    seq->add(create_schema_tuple(force_get_child(snode, qname, attribute)));
    seq->add(string_tuple_from_text(value));

    return tuple_cell::atomic_portal(seq, seq->size() - 2);
}

tuple_cell VirtualElementProducer::addComment(const text_source_t value)
{
    processAtomics();

    seq->add(create_schema_tuple(force_get_child(snode, xsd::QName(), comment)));
    seq->add(string_tuple_from_text(value));

    U_ASSERT(false);
    return tuple_cell::atomic_portal(seq, seq->size() - 2);
}

tuple_cell VirtualElementProducer::addTupleCell(const tuple_cell& tc)
{
    U_ASSERT(opened);

    if (!tc.is_portal() || seq != tc.get_portal().p) {
        seq->add(create_tuple(tc));
        if (tc.is_node()) {
            el_debug_printf("[%s] insert node %llx to %x at %d\n", __FUNCTION__, tc.get_node().to_logical_int(), seq, seq->size() - 1);
        } else {
            el_debug_printf("[%s] insert %x to %x at %d\n", __FUNCTION__, tc.get_type(), seq, seq->size() - 1);
        }

        return tuple_cell::atomic_portal(seq, seq->size() - 1);
    }

    return tuple_cell::eos();
}

tuple_cell VirtualElementProducer::addNode(const tuple_cell& node, bool preserveType)
{
    U_ASSERT(node.is_node());
    processAtomics();
    return addTupleCell(node);
}

tuple_cell VirtualElementProducer::addNS(const xmlns_ptr ns)
{
    processAtomics();

    if (noMoreAttributes) {
        throw XQUERY_EXCEPTION(XQTY0024);
    }

//    seq->add(tuple_cell(ns));
    U_ASSERT(false);

    return tuple_cell::eos();
}

tuple_cell VirtualElementProducer::addPI(const xsd::NCName& name, const text_source_t value)
{
    processAtomics();

    seq->add(create_schema_tuple(force_get_child(snode, xsd::QName(), pr_ins)));
    seq->add(string_tuple_from_text(value));

    U_ASSERT(false);
    return tuple_cell::atomic_portal(seq, seq->size() - 2);
}

tuple_cell VirtualElementProducer::addText(const text_source_t value)
{
    if (!text_is_null(value)) {
        textAccum.add(tuple(tuple_cell::atomic_any_text(xs_string, value)));
    }

    return tuple_cell::eos();
}

bool VirtualElementProducer::hasNode(const tuple_cell& node)
{
    return node.is_portal() && node.get_portal().p == seq;
}

#define SHOULD_NOT_HAPPEN do { U_ASSERT(false); return tuple_cell::eos(); } while (0)

VirtualElementGenerator::VirtualElementGenerator(schema_node_cptr _virtualRoot): virtualRoot(_virtualRoot) {}

IElementProducer* VirtualElementGenerator::addElement(const xsd::QName& qname, xmlscm_type type)
{
    schema_node_xptr child = force_get_child(virtualRoot, qname, element);
    sequence * seq;

    if (tmp_sequence.empty()) {
        seq = new sequence(1, 1024, -1, true);
    } else {
        seq = tmp_sequence.top();
        tmp_sequence.pop();
    }

    return new VirtualElementProducer(child, seq, this);
}

VirtualElementGenerator::~VirtualElementGenerator()
{
    while (!tmp_sequence.empty()) {
        delete tmp_sequence.top();
        tmp_sequence.pop();
    }
}

tuple_cell VirtualElementGenerator::addAtomic(const tuple_cell& node)
{  SHOULD_NOT_HAPPEN; }
tuple_cell VirtualElementGenerator::addAttribute(const xsd::QName& qname, const text_source_t value, xmlscm_type type)
{  SHOULD_NOT_HAPPEN; }
tuple_cell VirtualElementGenerator::addComment(const text_source_t value)
{  SHOULD_NOT_HAPPEN; }
tuple_cell VirtualElementGenerator::addNode(const tuple_cell& node, bool preserveType)
{  SHOULD_NOT_HAPPEN; }
tuple_cell VirtualElementGenerator::addNS(const xmlns_ptr ns)
{  SHOULD_NOT_HAPPEN; }
tuple_cell VirtualElementGenerator::addPI(const xsd::NCName& name, const text_source_t value)
{  SHOULD_NOT_HAPPEN; }
tuple_cell VirtualElementGenerator::addText(const text_source_t value)
{  SHOULD_NOT_HAPPEN; }
tuple_cell VirtualElementGenerator::close()
{  SHOULD_NOT_HAPPEN; }

bool VirtualElementGenerator::hasNode(const tuple_cell& node)
{    return false;  }
