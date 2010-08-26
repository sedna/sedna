#ifndef PORTAL_H_
#define PORTAL_H_

#include "common/sedna.h"
#include "common/base.h"

#include "tr/structures/schema.h"
#include "tr/executor/base/xptr_sequence.h"

#include <vector>

struct tuple_cell;
class Serializer;

namespace portal {
    class VirtualNode {
    protected:
        schema_node_xptr snode;
    public:
        virtual void print(Serializer * out) const = 0;
    };

    class VirtualAttribute : public VirtualNode {
        text_source_t value;
    };

    class VirtualElement : public VirtualNode {
        bool noMoreAttributes;
        int count;
        int * shared_counter;

        typedef std::pair<int, VirtualElement *> PositionedVirtualElement;

        std::vector<PositionedVirtualElement> vitual_nodes;
        std::vector<xmlns_ptr> namespaces;
        xptr_sequence portal;
    public:
        virtual void print(Serializer * out) const;

        void setSchemaNode(schema_node_cptr a_snode) { snode = a_snode.ptr(); };
        void addNode(const xptr node);
        void addVirtual(const tuple_cell * t);
        void addNamespace(const xmlns_ptr &ns);
    };

    class VirtualStorage {
    public:
        VirtualNode * createElement();
        void releaseNode(VirtualNode * node);
    };

    void portalOnTransactionBegin();
    void portalOnTransactionEnd(bool commit);

    extern VirtualStorage * virtualNodeStorage;
};

#endif /* PORTAL_H_ */
