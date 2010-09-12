#include "tr/structures/portal.h"
#include "tr/executor/base/tuple.h"
#include "tr/crmutils/serialization.h"
#include "tr/structures/schema.h"
#include "common/errdbg/d_printf.h"

using namespace std;
using namespace portal;

namespace portal {
    schema_node_xptr get_schema_node(const tuple_cell tc) {
        U_ASSERT(tc.is_portal());
        sequence_ptr p = tc.get_portal();
        U_ASSERT(p.pos >= 0);

      /* The following code means: take the virtual first element's tuple from sequence
       * and take the first (the only) cell from that tuple, and take xptr stored there
       */
        return ((*(p.p))[p.pos]).cells[0].get_xptr();
    }

    VirtualElementIterator::VirtualElementIterator(const tuple_cell tc) : reader(new SequenceReader(tc)) {
        U_ASSERT(tc.get_atomic_type() == se_sequence_element);
        tuple t(1);
        reader->get(t);
        snode = t.cells[0].get_xptr();
    }

    VirtualElementIterator::VirtualElementIterator(const VirtualElementIterator * parent) : snode(XNULL), reader(NULL) {
        const tuple_cell tc = parent->reader->getCell();

        U_ASSERT(tc.is_portal());
        if (tc.get_atomic_type() == se_sequence_element) {
            if (parent->reader->sameSequence(tc.get_portal().p.get())) {
                reader = parent->reader;
            } else {
                reader = new SequenceReader(tc);
            }
        } else {
            reader = parent->reader;
        }
        snode = reader->getCell().get_xptr();
    }

    void VirtualElementIterator::close() {
        while (!do_get().is_eos()) { do_next(); }
    }

    void VirtualElementIterator::next(tuple &t) {
        do_next();
        t = do_get();
    };

    void VirtualElementIterator::do_next() { reader->next(); };

    tuple VirtualElementIterator::do_get() {
        tuple t(1);
        reader->get(t);
        const tuple_cell tc = t.cells[0];
        if (tc.is_atomic() && tc.get_atomic_type() == se_separator) {
            t.set_eos();
        } else {
            t.copy(tc);
        };
        return t;
    };

    static inline
    tuple create_tuple(const tuple_cell tc) { tuple t(1); t.copy(tc); return t; }

    static
    tuple create_schema_tuple(const schema_node_xptr snode) {
        tuple t(1);
        t.copy(tuple_cell::atomic_portal(snode));
        return t;
    };

    tuple_cell VirtualElementWriter::get() {
        return tuple_cell::atomic_portal(seq.get(), index);
    }

    void VirtualElementWriter::create(const schema_node_xptr snode, const tuple_cell& parent) {
        if (parent.is_eos()) {
            ownsSequence = true;
            if (cxt->tmp_sequence.empty()) {
                seq = new sequence(1);
            } else {
                seq = cxt->tmp_sequence.top();
                cxt->tmp_sequence.pop();
            }
        } else {
            U_ASSERT(parent.is_portal());
            ownsSequence = false;
            seq = parent.get_portal().p;
        }
        index = seq->size();
        seq->add(create_schema_tuple(snode));
        el_debug_printf("[%s] open %x at %d\n", __FUNCTION__, seq.get(), index);
        opened = true;
    };


    void VirtualElementWriter::add(const tuple_cell& tc) {
        U_ASSERT(opened);
        if (!tc.is_portal() || seq.get() != tc.get_portal().p.get()) {
            seq->add(create_tuple(tc));
            if (tc.is_node()) {
                el_debug_printf("[%s] insert node %llx to %x at %d\n", __FUNCTION__, tc.get_node().to_logical_int(), seq.get(), seq->size() - 1);
            } else {
                el_debug_printf("[%s] insert %x to %x at %d\n", __FUNCTION__, tc.get_type(), seq.get(), seq->size() - 1);
            }
        }
    };

    void VirtualElementWriter::close() {
        U_ASSERT(opened);
        seq->add(create_tuple(tuple_cell::atomic_se_separator()));
        el_debug_printf("[%s] insert end to %x\n", __FUNCTION__, seq.get());
        if (ownsSequence) {
            cxt->tmp_sequence.push(seq);
            seq = NULL;
        }
        closed = true;
        opened = false;
    }
};
