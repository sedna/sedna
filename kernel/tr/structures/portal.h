#ifndef PORTAL_H_
#define PORTAL_H_

#include "common/sedna.h"
#include "common/base.h"

#include "tr/structures/schema.h"
#include "tr/executor/base/sequence.h"
#include "tr/crmutils/xdm.h"

#include <vector>
#include <tr/strings/strings.h>

namespace portal {
    class SequenceReader {
    private:
        sequence * s;
        int pos;
    public:
        SequenceReader(tuple_cell t) : s(t.get_portal().p), pos(t.get_index()) { U_ASSERT(t.is_portal()); };
        inline void next() { pos++; };
        inline void get(tuple &t) { t.copy((*s)[pos]); };
        inline tuple_cell getCell() { return (*s)[pos].cells[0]; };
        inline bool sameSequence(const sequence * t) { return t == s; };
    };


    /* Virtual node can be iterated only once, so its instance is it's own iteratator */
    class VirtualNode : public IXDMNode, public IXDMNodeList  {
      private:
        bool atend;
        schema_node_cptr snode;
        counted_ptr<SequenceReader> reader;

        mutable SednaNode * nodeSedna;
        mutable VirtualNode * nodeVirtual;
      public:
        explicit VirtualNode(const tuple_cell t);
        explicit VirtualNode(const VirtualNode * parent);
        ~VirtualNode();

        void setParent(const VirtualNode * parent);

        t_item getNodeKind() const { return element; };
        const text_source_t getValue() const { return text_source_mem(NULL, 0); };
        const char * getLocalName() const { return snode->get_name(); };
        xsd::QName getQName() const { return xsd::QName::createNsN(snode->get_xmlns(), snode->get_name()); };
        xmlns_ptr getNamespace() const { return snode->get_xmlns(); };

        void printNodeName(se_ostream & out) const;

        IXDMNodeList * getAllChildren() { return this; };

        bool next();
        bool end() { return atend; };

        IXDMNode * getNode();
    };

    class VirtualElementWriter {
    private:
        bool noMoreAttributes;
        bool ownsSequence;
        int index;
        bool opened, closed;

        dynamic_context * cxt;
        sequence* seq;
    public:
        VirtualElementWriter(dynamic_context * a_cxt) :
            noMoreAttributes(false), ownsSequence(false), index(0),
            opened(false), closed(false), cxt(a_cxt), seq(NULL) {};

        ~VirtualElementWriter();

        void create(const schema_node_xptr snode, const tuple_cell& parent);
        void add(const tuple_cell& tc);
        void close();

        tuple_cell get();
    };
}

#endif /* PORTAL_H_ */
