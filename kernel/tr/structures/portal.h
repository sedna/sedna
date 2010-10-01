#ifndef PORTAL_H_
#define PORTAL_H_

#include "common/sedna.h"
#include "common/base.h"

#include "tr/structures/schema.h"
#include "tr/executor/base/sequence.h"
#include "tr/crmutils/xmlserializer.h"

#include <vector>

namespace portal {
    class SequenceReader {
    private:
        counted_ptr<sequence> s;
        int pos;
    public:
        SequenceReader(tuple_cell t) : s(t.get_portal().p), pos(t.get_index()) { U_ASSERT(t.is_portal()); };
        inline void next() { pos++; };
        inline void get(tuple &t) { t.copy((*s)[pos]); };
        inline tuple_cell getCell() { return (*s)[pos].cells[0]; };
        inline bool sameSequence(const sequence * t) { return t == s.get(); };
    };

    struct VirtualElementIterator : public XDMElement {
    private:
        xptr snode;
        counted_ptr<SequenceReader> reader;
    public:
        explicit VirtualElementIterator(const tuple_cell t);
        explicit VirtualElementIterator(const VirtualElementIterator * parent);

        void do_next();
        tuple do_get();

        virtual schema_node_cptr getSchemaNode() const { return snode; };
        virtual void next(tuple &t);

        void close();
    };

    class VirtualElementWriter {
    private:
        bool noMoreAttributes;
        bool ownsSequence;
        int index;
        bool opened, closed;

        dynamic_context * cxt;
        counted_ptr<sequence> seq;
    public:
        VirtualElementWriter(dynamic_context * a_cxt) :
            noMoreAttributes(false), ownsSequence(false), index(0),
            opened(false), closed(false), cxt(a_cxt), seq(NULL) {};

        void create(const schema_node_xptr snode, const tuple_cell& parent);
        void add(const tuple_cell& tc);
        void close();

        tuple_cell get();
    };
}

#endif /* PORTAL_H_ */
