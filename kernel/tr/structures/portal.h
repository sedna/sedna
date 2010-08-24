#ifndef PORTAL_H_
#define PORTAL_H_

namespace portal {
    class VirtualStorage {
        VirtualElement * createElement();
    };

    void portalOnTransactionBegin();
    void portalOnTransactionEnd(bool commit);

    extern VirtualStorage * virtualNodeStorage;

    class VirtualNode {
        schema_node_xptr snode;
        xptr ptr;
    public:
        xptr getPtr() const { return ptr; };
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
        virtual se_ostream & print(se_ostream & crmout) const;

        void addNode(const xptr node);
        void addNode(const tuple_cell &t);
        void addNamespace(const xmlns_ptr &ns);
    };
}

#endif /* PORTAL_H_ */
