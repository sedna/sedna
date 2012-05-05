#ifndef _XPATHEXECUTION_H_
#define _XPATHEXECUTION_H_

#include "tr/structures/nodeinterface.h"
#include "tr/nid/nidstring.h"

#include "SchemaTests.h"

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
    scoped_ptr<PathSchemaMerge> merge;

    explicit NodeIterator(Node _node, NextNodeProc __next = NULL, counted_ptr<NidString> _baseNID = NULL)
      : baseNID(_baseNID), node(_node), _next(__next), merge(NULL) {};

    Node get() const { return node; };

    inline
    bool next() {
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
    void nextMerge();

    explicit NodeIterator(PathSchemaMerge * ms)
      : baseNID(NULL), node(), _next(&NodeIterator::nextMerge), merge(ms) {};
};

struct PathStepIterator : NodeIterator {
    schema_node_cptr snode;
    pe::AtomizedPath path;

    PathStepIterator(const NodeIterator & nit, const pe::AtomizedPath &_path)
      : NodeIterator(nit), path(_path) {};
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
    bool eos() const { return _eos; };
    void clear();

    void push(const Node & node) {
        U_ASSERT(!node.get().isNull());
        mergelist.push_back(PathStepIterator(NodeIterator(node, NULL, NULL), path));
    };
    
    Node next();
};

struct PathSchemaMerge {
private:
    bool _eos;
    NIDMergeHeap mergeheap;
public:  
    void pushAll(Node, schema_node_cptr, const SchemaNodeList &);

    void push(const NodeIterator & node) {
        U_ASSERT(!node.get().isNull());
        mergeheap.push_back(NIDMergeHeap::value_type(node.get().getPtr(), node));
        std::push_heap(mergeheap.begin(), mergeheap.end(), NIDMergeHeapCompare());
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


/*

typedef Node (*NextNodeProc)(Node node, AxisHints * hint);
typedef bool (*TestNodeProc)(Node node, AxisHints * hint);

typedef std::map<schema_node_xptr, SchemaPathList> DescendantMap;


/**
 *  AxisHints stores data, needed to effectively evaluate path expressions
 */
/*
struct AxisHints {
    xpath::NodeTest nt;
    t_item childTypeMask;
    ISchemaTest * schemaTest;
    Node baseNode;
    u_timeb tx;
    
    DescendantMap descendantPathIndex;
    
    std::vector<Node> nodeHeapStorage; // It turns out, that priority_queue sucks: it have no clear() method.
    std::list<Node> nodeStackStorage; // Same thing with stack
    
    ISchemaTest * selfSchemaTest;
    
    AxisHints() : nt(), childTypeMask(element), schemaTest(NULL), nodeHeapStorage() {};
    
    Node nodeHeapPop() {
        if (nodeHeapStorage.empty()) {
            return Node();
        } else {
            Node result = nodeHeapStorage.front();
            std::pop_heap(nodeHeapStorage.begin(), nodeHeapStorage.end(), NodeDocumentOrderCmp());
            nodeHeapStorage.pop_back();
            return result;
        }
    };
    
    void nodeHeapPush(const Node & node) {
        nodeHeapStorage.push_back(node);
        std::push_heap(nodeHeapStorage.begin(), nodeHeapStorage.end(), NodeDocumentOrderCmp());
    };
    
    Node nodeStackPop() {
        if (nodeStackStorage.empty()) {
            return Node();
        } else {
            Node result = nodeStackStorage.front();
            nodeStackStorage.pop_front();
            return result;
        }
    }
    
    void nodeStackPush(const Node & node) {
        nodeStackStorage.push_front(node);
    }
};

Node resolveAxis_Descendant(Node node, AxisHints * hint);
Node resolveAxis_ChildType(Node node, AxisHints * hint);
Node resolveAxis_ChildAny(Node node, AxisHints * hint);
Node resolveAxis_ChildFirst(Node node, AxisHints * hint);
Node resolveAxis_Self(Node node, AxisHints * hint);
Node resolveAxis_NULL(Node node, AxisHints * hint);
Node resolveNode_RightSiblingQName(Node node, AxisHints * hint);
Node resolveNode_LeftSiblingQName(Node node, AxisHints * hint);
Node resolveAxis_following(Node node, AxisHints * hint);

Node nextNode_RightSiblingMerge(Node node, AxisHints * hint);
Node nextNode_RightDescendantMerge(Node node, AxisHints * hint);

Node nextNode_traverseAll(Node node, AxisHints * hint);
Node nextNode_Parent(Node node, AxisHints * hint);
Node nextNode_Null(Node node, AxisHints * hint);

Node nextNode_RightSiblingSame(Node node, AxisHints * hint);
Node nextNode_RightSiblingAny(Node node, AxisHints * hint);
Node nextNode_LeftSiblingSame(Node node, AxisHints * hint);
Node nextNode_LeftSiblingAny(Node node, AxisHints * hint);
Node nextNode_RightSiblingType(Node node, AxisHints * hint);

Node nextNode_following(Node node, AxisHints * hint);
Node nextNode_preceding(Node node, AxisHints * hint);

bool testNode_PIName(Node node, AxisHints * hint);
bool schemaTest(Node node, AxisHints * hint);

static const struct {
    Axis axis;
    NextNodeProc nextProc;
    EvaluateAxisProc evalProc;
} axisResolutionProcs[] = {
    {pe::axis_child,              nextNode_RightSiblingMerge,     resolveAxis_Descendant},
    {pe::axis_descendant,         nextNode_RightDescendantMerge,  resolveAxis_Descendant},
    {pe::axis_attribute,          nextNode_RightSiblingMerge,     resolveAxis_Descendant},
    {pe::axis_self,               nextNode_Null,                  resolveAxis_Self},
    {pe::axis_descendant_or_self, nextNode_RightDescendantMerge,  resolveAxis_Descendant},
    {pe::axis_parent,             nextNode_Null,                  nextNode_Parent},
    
    {pe::axis_ancestor,           nextNode_Parent,            nextNode_Parent},
    {pe::axis_ancestor_or_self,   nextNode_Parent,            resolveAxis_Self},
    
    {pe::axis_following,          nextNode_following,         resolveAxis_following},
    {pe::axis_following_sibling,  nextNode_RightSiblingAny,   nextNode_RightSiblingAny},
    {pe::axis_preceding,          nextNode_preceding,         nextNode_preceding},
    {pe::axis_preceding_sibling,  nextNode_LeftSiblingAny,    nextNode_LeftSiblingAny},
    
    {__axis_last, },
};
*/

#endif /* _XPATHEXECUTION_H_ */
