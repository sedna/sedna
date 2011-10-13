/*
 * File:  PPAxisChild.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPAxisStep.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/xs_names.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/structures/nodeutils.h"
#include "tr/executor/base/XPathOnSchema.h"

#include <algorithm>
#include <queue>

using namespace xpath;

struct NodeDocumentOrderCmp {
  bool operator()(const Node & __x, const Node& __y) const
  { return !(__x < __y); }
};

// NodeDocumentOrderCmp nodeDocumentOrderCmp;

typedef std::map<schema_node_xptr, SchemaPathList> DescendantMap;

/**
  AxisHints stores data, needed to effectively evaluate path expressions
*/
struct AxisHints {
    xpath::NodeTest nt;
    t_item childTypeMask;
    ISchemaTest * schemaTest;
    Node baseNode;
    u_timeb tx;

    DescendantMap descendantPathIndex;

    std::vector<Node> nodeHeapStorage; // It turns out, that priority_queue sucks: it have no clear() method.
    std::list<Node> nodeStackStorage; // Same thing with stack

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

/**
  Evaluates node path, following path from \node to \goal
*/

static
void getNodePath(schema_node_cptr from, schema_node_cptr to, SchemaPath& path) {
    schema_node_cptr i = to;

    while (i.ptr() != from.ptr()) {
        path.push_back(i->getIndex());

        if (i->parent == XNULL) {
            U_ASSERT(false);
        }

        i = i->parent;
    }
}

static
Node getRightBrother(Node node, Node commonAncestor) {
    Node n = getNextDescriptorOfSameSort(node.getPtr());

    /* Check if this node still is a descendant of the base one */
    if (!n.isNull()) {
        int relation = nid_cmp_effective(n.getPtr(), commonAncestor.getPtr());

        if (relation != 2 && relation != 0) {
            return XNULL;
        }
    }

    return n;
};

static
xptr getFirstNodeByPath(Node node, const SchemaPath & path, Node commonAncestor) {
    xptr nodeI = node.getPtr();

    for (std::vector<int>::const_reverse_iterator i = path.rbegin(); i != path.rend(); ++i) {
        xptr nodeJ = getChildAt(nodeI, *i);

  /* If we found the first child node on the step of path traversing,
    * we should scan through all children of this type to find one, that have
    * requested grandchild for the next step */

        while (nodeJ == XNULL) {
            nodeI = getRightBrother(nodeI, commonAncestor).getPtr();

            if (nodeI == XNULL) {
                return XNULL;
            }

            nodeJ = getChildAt(nodeI, *i);
        }

        if (nodeJ == XNULL) {
            return XNULL;
        }

        nodeI = nodeJ;
    }

    return nodeI;
}

static
void traverseSchemaPathList(Node node, const SchemaPathList * list, AxisHints * hint) {
    for (SchemaPathList::const_iterator i = list->begin(); i != list->end(); ++i) {
        xptr result = getFirstNodeByPath(node, *i, hint->baseNode);
        if (result != XNULL) { hint->nodeHeapPush(result); }
    }
}

Node nextNode_RightSiblingMerge(Node node, AxisHints * hint) {
    U_ASSERT(!node.isNull());

    /* Push the next node of the previously evaluated to node sorting heap.
       Push it if it is not XNULL. */
    Node n = getRightSiblingOfSameSort(node.getPtr());

    if (!n.isNull()) {
        hint->nodeHeapPush(n);
    }

    /* If nothing left to merge with, return EMPTY node. */
    return hint->nodeHeapPop();
}

Node nextNode_RightDescendantMerge(Node node, AxisHints * hint) {
    U_ASSERT(!node.isNull());

    /* Push the next node of the previously evaluated to node sorting heap.
       Push it if it is not XNULL. */
    Node n = getRightBrother(node, hint->baseNode);

    if (!n.isNull()) {
        hint->nodeHeapPush(n);
    }

    /* If nothing left to merge with, return EMPTY node. */
    return hint->nodeHeapPop();
}

#define TIME_START(t) if (executor_globals::profiler_mode) { u_ftime(&(t)); };
#define TIME_END(t) if (executor_globals::profiler_mode) { \
    u_timeb x; \
    u_ftime(&x); \
    (t) = x - (t); \
}


/*
  TODO: Descendant axis is evaluated as follows: HOOOWWWWW!
*/

Node resolveAxis_Descendant(Node node, AxisHints * hint) {
    schema_node_cptr scn = getSchemaNode(node.getPtr());

    /* Find ready (cached) node extraction strategy for given schema node */
    DescendantMap::iterator descMap = hint->descendantPathIndex.find(scn.ptr());

    /* If nothing found, evaluate all available paths from given node, to axis-evaluatable */
    if (descMap == hint->descendantPathIndex.end()) {
        t_scmnodes schemaNodes;
        SchemaPathList pathList;

        TIME_START(hint->tx);
        /* Build path list for every resolved node */
        executeNodeTestPath(scn, hint->nt, &schemaNodes, &pathList);
        TIME_END(hint->tx);

        descMap = hint->descendantPathIndex.insert(DescendantMap::value_type(scn.ptr(), pathList)).first;
    }

    U_ASSERT(descMap != hint->descendantPathIndex.end());

    /* Find all nodes for all found pathes for given node.
       We need all nodes to maintain document order.
       MAYBE if document order is not required, we can find just the very first one. */

    traverseSchemaPathList(node, &(descMap->second), hint);

    /* Get the first node from heap */
    return hint->nodeHeapPop();
}

Node resolveAxis_ChildType(Node node, AxisHints * hint) {
    return getFirstChildByType(node.getPtr(), hint->childTypeMask);
}

Node resolveAxis_ChildAny(Node node, AxisHints * hint) {
    return getFirstChild(node.getPtr());
}

Node resolveAxis_Self(Node node, AxisHints * hint) {
    return node;
}

Node resolveAxis_NULL(Node node, AxisHints * hint) {
    return XNULL;
}

Node nextNode_Parent(Node node, AxisHints * hint) {
    return node.checkp().getActualParent();
}

Node nextNode_Null(Node node, AxisHints * hint) {
    return XNULL;
}

Node resolveNode_RightSiblingQName(Node node, AxisHints * hint) {
    schema_node_cptr scn = node.checkp().getSchemaNode();

    if (scn->type == attribute) {
        return XNULL;
    }

    schema_node_cptr child = scn->parent->get_first_child(hint->nt.getQName(), element);

    if (!child.found()) {
        return XNULL;
    }

    return getRightSiblingBySchema(node.getPtr(), child);
}

Node resolveNode_LeftSiblingQName(Node node, AxisHints * hint) {
    schema_node_cptr scn = node.checkp().getSchemaNode();

    if (scn->type == attribute) {
        return XNULL;
    }

    schema_node_cptr child = scn->parent->get_first_child(hint->nt.getQName(), element);

    if (!child.found()) {
        return XNULL;
    }

    return getLeftSiblingBySchema(node.getPtr(), child);
}

Node nextNode_RightSiblingSame(Node node, AxisHints * hint) {
    if (node.checkp().getSchemaNode()->type == attribute) {
        return XNULL;
    }

    return getRightSiblingOfSameSort(node.getPtr());
}

Node nextNode_RightSiblingAny(Node node, AxisHints * hint) {
    if (node.checkp().getSchemaNode()->type == attribute) {
        return XNULL;
    }

    return getRightSiblingByTypeMask(node.getPtr(), hint->childTypeMask);
}

Node nextNode_LeftSiblingSame(Node node, AxisHints * hint) {
    if (node.checkp().getSchemaNode()->type == attribute) {
        return XNULL;
    }

    return getLeftSiblingOfSameSort(node.getPtr());
}

Node nextNode_LeftSiblingAny(Node node, AxisHints * hint) {
    if (node.checkp().getSchemaNode()->type == attribute) {
        return XNULL;
    }

    return getLeftSiblingByType(node.getPtr(), hint->childTypeMask);
}

Node nextNode_RightSiblingType(Node node, AxisHints * hint) {
    if (node.checkp().getSchemaNode()->type == attribute) {
        return XNULL;
    }

    return getRightSiblingByTypeMask(node.getPtr(), hint->childTypeMask);
}

Node resolveAxis_following(Node node, AxisHints * hint) {
    Node x = node.checkp().getRight();;

    while (x.isNull()) {
        node = node.checkp().getActualParent();

        if (node.isNull()) {
            return XNULL;
        }

        x = node.checkp().getRight();
    }

    return x;
}

Node nextNode_following(Node node, AxisHints * hint) {
    hint->nodeStackPush(node);

    Node x = getFirstChild(node.checkp().getPtr());

    while (x.isNull()) {
        /* Pop node from stack */
        if (hint->nodeStackStorage.empty()) {
            node = node.checkp().getActualParent();
            if (node.isNull()) {
                return XNULL;
            }
        } else {
            node = hint->nodeStackPop();
        }
        x = node.checkp().getRight();
    }

    return x;
}

Node nextNode_preceding(Node node, AxisHints * hint) {
    Node x = node.checkp().getLeft();

    while (x.isNull()) {
        node = node.checkp().getActualParent();

        if (node.isNull()) {
            return XNULL;
        } else if (nid_cmp_effective(node.getPtr(), hint->baseNode.getPtr()) != -2) {
            return node;
        } else {
            x = node.checkp().getLeft();
            continue;
        }
    }

    Node y;

    while (!(y = getLastChild(x.getPtr())).isNull()) {
        x = y;
    }

    return x;
}


bool testNode_PIName(Node node, AxisHints * hint) {
    U_ASSERT(node.checkp().getNodeType() == pr_ins);

    if (!hint->nt.getLocal().valid()) {
        return true;
    } else {
        return PINode(node.checkp()).compareTarget(hint->nt.getLocal().getValue()) == 0;
    }
}

bool schemaTest(Node node, AxisHints * hint) {
    U_ASSERT(hint->schemaTest != NULL);

    if (hint->schemaTest == NULL || !hint->schemaTest->test(node.checkp().getSchemaNode())) {
        return false;
    } else {
        if (node.getNodeType() == pr_ins && hint->nt.getLocal().valid()) {
            return PINode(node).compareTarget(hint->nt.getLocal().getValue()) == 0;
        } else {
            return true;
        }
    }
}

static const struct {
    Axis axis;
    NextNodeProc nextProc;
    EvaluateAxisProc evalProc;
} axisResolutionProcs[] = {
  {axis_child,              nextNode_RightSiblingMerge,     resolveAxis_Descendant},
  {axis_descendant,         nextNode_RightDescendantMerge,  resolveAxis_Descendant},
  {axis_attribute,          nextNode_RightSiblingMerge,     resolveAxis_Descendant},
  {axis_self,               nextNode_Null,                  resolveAxis_Self},
  {axis_descendant_or_self, nextNode_RightDescendantMerge,  resolveAxis_Descendant},
  {axis_descendant_attr,    nextNode_RightDescendantMerge,  resolveAxis_Descendant /* Soon to be depricated */ },
  {axis_parent,             nextNode_Null,                  nextNode_Parent},

  {axis_any, /* gap */ },

  {axis_ancestor,           nextNode_Parent,            nextNode_Parent},
  {axis_ancestor_or_self,   nextNode_Parent,            resolveAxis_Self},

 //
  {axis_following,          nextNode_following,         resolveAxis_following},
  {axis_following_sibling,  nextNode_RightSiblingAny,   nextNode_RightSiblingAny},
  {axis_preceding,          nextNode_preceding,         nextNode_preceding},
  {axis_preceding_sibling,  nextNode_LeftSiblingAny,    nextNode_LeftSiblingAny},

  {__axis_last, },
};


PPAxisStep::PPAxisStep(dynamic_context* _cxt_, operation_info _info_, PPOpIn _child_, NodeTest _nt_)
  : PPIterator(_cxt_, _info_, "PPAxisStep"), child(_child_), nt(_nt_), currentNodeIndir(XNULL), hint(new AxisHints)
{
    U_ASSERT((nt.axis > axis_any && nt.axis < __axis_last) || (nt.type > node_test_invalid && nt.type < __node_test_last));

    int axisIndex = nt.axis - axis_child;

    U_ASSERT(axisResolutionProcs[axisIndex].axis == nt.axis);

    hint->nt = nt;
    hint->schemaTest = createSchemaTest(nt);

    if (hint->schemaTest == NULL) {
        evaluateAxisProc = resolveAxis_NULL;
        nextNodeProc = NULL;
        testNodeProc = NULL;

        return;
    }

    hint->childTypeMask = hint->schemaTest->getTypeMask();

    evaluateAxisProc = axisResolutionProcs[axisIndex].evalProc;
    nextNodeProc = axisResolutionProcs[axisIndex].nextProc;
    testNodeProc = NULL;

    /* All optimizations here v v v */

    /* Kinda optimization =) */
    if ((nt.axis == axis_child) && ((hint->childTypeMask & ~ti_singleton_element) == 0)) {
        evaluateAxisProc = resolveAxis_ChildType;
        nextNodeProc = nextNode_RightSiblingType;
    }

    if ((nt.axis == axis_attribute) && !hint->nt.isAnyQName()) {
        nextNodeProc = nextNode_Null;
    }

    if (hint->childTypeMask == pr_ins) {
        testNodeProc = testNode_PIName;
    }

    if ((nt.axis == axis_self) || (nt.axis >= axis_ancestor) || (nt.axis == axis_parent)) {
        testNodeProc = schemaTest;
    }

    if (nt.axis == axis_following_sibling || nt.axis == axis_preceding_sibling) {
        testNodeProc = schemaTest;

        if (!hint->nt.isAnyQName()) {
            evaluateAxisProc = (nt.axis == axis_following_sibling) ? resolveNode_RightSiblingQName : resolveNode_LeftSiblingQName;
            nextNodeProc = (nt.axis == axis_following_sibling) ? nextNode_RightSiblingSame : nextNode_LeftSiblingSame;
        };
    }

    u_timeb_init(timer+0);
    u_timeb_init(timer+1);
    u_timeb_init(timer+2);
    u_timeb_init(timer+3);
}

PPAxisStep::~PPAxisStep()
{
    delete hint->schemaTest;
    delete hint;
    delete child.op;
    child.op = NULL;
}

void PPAxisStep::do_open ()
{
    child.op->open();
    hint->baseNode = XNULL;
    currentNodeIndir = XNULL;
}

void PPAxisStep::do_reopen()
{
    child.op->reopen();
    hint->baseNode = XNULL;
    currentNodeIndir = XNULL;
    hint->nodeHeapStorage.clear();
    hint->nodeStackStorage.clear();
}

void PPAxisStep::do_close()
{
    child.op->close();
    hint->nodeHeapStorage.clear();
    hint->nodeStackStorage.clear();
    hint->descendantPathIndex.clear();
}

void PPAxisStep::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}

void PPAxisStep::do_next(tuple& t)
{
    Node currentNode = indirectionDereferenceCP(currentNodeIndir);

    while (true) {
        if (!currentNode.isNull()) {
            u_timeb z;
            TIME_START(z);
            currentNode = nextNodeProc(currentNode, hint);
            TIME_END(z);
            if (executor_globals::profiler_mode) { timer[2] = timer[2] + z; }
        }

        while (currentNode.isNull()) {
            child.op->next(t);

            if (t.is_eos()) {
                currentNodeIndir = XNULL;
                return ;
            }

            if (!(child.get(t).is_node())) {
                throw XQUERY_EXCEPTION(XPTY0020);
            }

            hint->baseNode = child.get(t).get_node();

            u_timeb z;
            TIME_START(z);
            u_timeb_init(&hint->tx);
            currentNode = evaluateAxisProc(hint->baseNode, hint);
            TIME_END(z);
            if (executor_globals::profiler_mode) { timer[0] = timer[0] + hint->tx; }
            if (executor_globals::profiler_mode) { timer[1] = timer[1] + z; }
        }

        U_ASSERT(!currentNode.isNull());

        if (testNodeProc == NULL || testNodeProc(currentNode, hint)) {
            currentNodeIndir = currentNode.checkp().getIndirection();
            t.copy(tuple_cell::node_indir(currentNodeIndir));
            return ;
        }
    }
}

PPIterator* PPAxisStep::do_copy(dynamic_context *_cxt_)
{
    PPAxisStep *res = se_new PPAxisStep(_cxt_, info, child, nt);
    res->child.op = child.op->copy(_cxt_);
    return res;
}
