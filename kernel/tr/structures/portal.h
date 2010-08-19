#ifndef PORTAL_H_
#define PORTAL_H_

namespace portal {
    class VirtualNode {
        schema_node_xptr snode;
    };

    class VirtualAttribute : public VirtualNode {
        text_source_t value;
    };

    class VirtualElement : public VirtualNode {
        typedef std::pair<int, VirtualElement> PositionedElement;

        std::vector<xmlns_ptr> namespaces;
        std::vector<VirtualAttribute> attributes;
        std::vector<PositionedElement> virtual_children;
        xptr_sequence portal;

    public:
        virtual se_ostream & print(se_ostream & crmout) const;

        void addNode(const xptr node);
        void addNode(const tuple_cell &t);
        void addNamespace(const xmlns_ptr &ns);
    };
}

#endif /* PORTAL_H_ */
