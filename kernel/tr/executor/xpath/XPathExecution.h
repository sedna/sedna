#ifndef _XPATHEXECUTION_H_
#define _XPATHEXECUTION_H_

#include "tr/structures/nodeinterface.h"
#include "tr/nid/nidstring.h"

#include "SchemaTests.h"
#include "AtomizedPath.h"

#include <algorithm>
#include <queue>
#include <map>

typedef std::vector<int> SchemaPath;
typedef std::vector<SchemaPath> SchemaPathList;

struct PathSchemaMerge;

struct NodeIterator {
public:
typedef void (NodeIterator::*NextNodeProc)();
protected:
    counted_ptr<NidString> baseNID;
    Node node;
    NextNodeProc _next;
public:
    explicit NodeIterator(Node _node, NextNodeProc __next = NULL, counted_ptr<NidString> _baseNID = NULL)
      : baseNID(_baseNID), node(_node), _next(__next) {};

    Node get() const { return node; };

    inline
    bool next()
    {
        if (_next != NULL) {
            (this ->* _next)();
            return !node.isNull();
        } else {
            node = XNULL;
            return false;
        }
    }

    void nextNodeCommonAncestor();
    void nextNodeSibling();
    void nextNodeParent();
};

struct NextChildNode {
    Node _node;
    schema_node_cptr _snode;
    const pe::AtomizedPath &_path;
    typemask_t _mask;

    NextChildNode(const Node & node, const pe::AtomizedPath & path, typemask_t mask)
        : _node(node), _path(path), _mask(mask) { _snode = node.checkp().getSchemaNode(); };
};

struct PathStepIterator : NodeIterator {
    schema_node_cptr snode;
    pe::AtomizedPath path;
    typemask_t mask;

    void nextSiblingSchema();

    PathStepIterator(const NodeIterator & nit, const pe::AtomizedPath &_path, schema_node_cptr _snode)
        : NodeIterator(nit), snode(_snode), path(_path), mask(ti_all_valid) { };

    PathStepIterator(const NextChildNode & ctr)
        : NodeIterator(ctr._node, (NextNodeProc) &PathStepIterator::nextSiblingSchema),
            snode(ctr._snode), path(ctr._path), mask(ctr._mask) { };

    PathStepIterator passStep() const { PathStepIterator result(*this); result.path = path + 1; return result; };
};

typedef std::vector< std::pair<NidString, NodeIterator> > NIDMergeHeap;

struct NIDMergeHeapCompare {
    bool operator()(
        const NIDMergeHeap::value_type & a,
        const NIDMergeHeap::value_type & b)
    {
        return a.first.compare(b.first) > 0;
    };
};

typedef std::deque< PathStepIterator > PathStepList;

struct PathTraverse {
private:
    PathStepList mergelist;
    pe::AtomizedPath path;
    bool _eos;
public:
    PathTraverse(const pe::AtomizedPath & _path);

    bool eos() const { return _eos; };
    void clear();

    void push(const Node & node) {
        U_ASSERT(!node.isNull());
        mergelist.push_back(PathStepIterator(NodeIterator(node, NULL, NULL), path, node.checkp().getSchemaNode()));
        _eos = false;
    };

    Node next();
};

struct PathSchemaMerge {
private:
    bool _eos;
    NIDMergeHeap mergeheap;
public:
    PathSchemaMerge() : _eos(true) {};
  
    void pushAll(Node, schema_node_cptr, const SchemaNodeList &);

    void push(const NodeIterator & node) {
        U_ASSERT(!node.get().isNull());
        mergeheap.push_back(NIDMergeHeap::value_type(node.get().getPtr(), node));
        std::push_heap(mergeheap.begin(), mergeheap.end(), NIDMergeHeapCompare());
        _eos = false;
    };

    bool eos() const { return _eos; };

    void clear();

    Node next() {
        if (mergeheap.empty()) {
            return Node();
        };

        Node result = mergeheap.front().second.get();

        std::pop_heap(mergeheap.begin(), mergeheap.end(), NIDMergeHeapCompare());

        if (!mergeheap.back().second.next()) {
            mergeheap.pop_back();
            _eos = mergeheap.empty();
        } else {
            mergeheap.back().first = NidString(mergeheap.back().second.get().getPtr());
            std::push_heap(mergeheap.begin(), mergeheap.end(), NIDMergeHeapCompare());
        }

        return result;
    };
};

bool executeSchemaPathTest(schema_node_cptr base, const pe::AtomizedPath & path, SchemaNodePtrSet * output, bool _fast = false);

#endif /* _XPATHEXECUTION_H_ */
