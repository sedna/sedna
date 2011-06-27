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
  { return __x < __y; }
};

typedef std::priority_queue<Node, std::vector<Node>, NodeDocumentOrderCmp> NodeSequenceHeap;
typedef std::vector<int> SchemaPath;
typedef std::vector<SchemaPath> SchemaPathList;
typedef std::map<schema_node_xptr, SchemaPathList> DescendantMap;

/**
  AxisHints stores data, needed to effectively evaluate path expressions
*/
struct AxisHints {
    xpath::NodeTest nt;
    t_item childTypeMask;
    ISchemaTest * schemaTest;

    DescendantMap descendantPathIndex;
    NodeSequenceHeap nodeHeap;

    std::stack<Node> nodeStack;

    AxisHints() : nt(), childTypeMask(element) {};

    Node nodeHeapPop() {
        if (nodeHeap.empty()) {
            return Node();
        } else {
            Node result = nodeHeap.top();
            nodeHeap.pop();
            return result;
        }
    };
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
xptr getFirstNodeByPath(Node node, const SchemaPath & path) {
    xptr nodeI = node.getPtr();

    for (std::vector<int>::const_reverse_iterator i = path.rbegin(); i != path.rend(); ++i) {
        nodeI = getChildAt(nodeI, *i);
        if (nodeI == XNULL) { break; }
    }

    return nodeI;
}

static
void traverseSchemaPathList(Node node, const SchemaPathList * list, NodeSequenceHeap * nodes) {
    for (SchemaPathList::const_iterator i = list->begin(); i != list->end(); ++i) {
        xptr result = getFirstNodeByPath(node, *i);
        if (result != XNULL) { nodes->push(result); }
    }
}

Node nextNode_RightSiblingMerge(Node node, AxisHints * hint) {
    U_ASSERT(!node.isNull());

    /* Push the next node of the previously evaluated to node sorting heap.
       Push it if it is not XNULL. */
    Node n = getRightSiblingOfSameSort(node.getPtr());

    if (!n.isNull()) {
        hint->nodeHeap.push(n);
    }

    /* If nothing left to merge with, return EMPTY node. */
    return hint->nodeHeapPop();
}

Node resolveAxis_XPathStep(Node node, AxisHints * hint) {
    schema_node_cptr scn = getSchemaNode(node.getPtr());

    /* Find ready (cached) node extraction strategy for given schema node */
    DescendantMap::iterator descMap = hint->descendantPathIndex.find(scn.ptr());

    /* If nothing found, evaluate all available paths from given node, to axis-evaluatable */
    if (descMap == hint->descendantPathIndex.end()) {
        t_scmnodes schemaNodes;
        SchemaPathList pathList;

        /* Build path list for every resolved node */
        executeNodeTest(scn, hint->nt, &schemaNodes, NULL, NULL);

        /* Just copy cached result array to our array */
        for (std::vector<schema_node_xptr>::iterator i = schemaNodes.begin(); i != schemaNodes.end(); ++i) {
            pathList.push_back(SchemaPath());
            getNodePath(scn, *i, pathList.at(pathList.size() - 1));
        };

        descMap = hint->descendantPathIndex.insert(DescendantMap::value_type(scn.ptr(), pathList)).first;
    }

    U_ASSERT(descMap != hint->descendantPathIndex.end());

    /* Find all nodes for all found pathes for given node.
       We need all nodes to maintain document order.
       MAYBE if document order is not required, we can find just the very first one. */

    traverseSchemaPathList(node, &(descMap->second), &(hint->nodeHeap));

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

Node resolveAxis_Parent(Node node, AxisHints * hint) {
    return getActualParentNode(node.getPtr());
}


Node nextNode_Parent(Node node, AxisHints * hint) {
    return getActualParentNode(node.getPtr());
}

Node nextNode_Null(Node node, AxisHints * hint) {
    return XNULL;
}

Node nextNode_RightSiblingSame(Node node, AxisHints * hint) {
    return getRightSiblingOfSameSort(node.getPtr());
}

Node nextNode_RightSiblingAny(Node node, AxisHints * hint) {
    return getRightSiblingByTypeMask(node.getPtr(), hint->childTypeMask);
}

Node nextNode_LeftSiblingAny(Node node, AxisHints * hint) {
    return getLeftSiblingByType(node.getPtr(), hint->childTypeMask);
}

Node nextNode_RightSiblingType(Node node, AxisHints * hint) {
    return getRightSiblingByTypeMask(node.getPtr(), hint->childTypeMask);
}

Node nextNode_following(Node node, AxisHints * hint) {
    hint->nodeStack.push(node);

    Node x = getFirstChild(node.checkp().getPtr());
    while (x.isNull()) {
        /* Pop node from stack */
        if (hint->nodeStack.empty()) {
            node = node.getActualParent();
            if (node.isNull()) {
                return XNULL;
            }
        } else {
            node = hint->nodeStack.top();
            hint->nodeStack.pop();
        }
        x = node.getRight();
    }

    return x;
}

Node nextNode_preceding(Node node, AxisHints * hint) {
    hint->nodeStack.push(node);

    Node x = node.getLeft();

    if (x.isNull()) {
        return node.getActualParent();
    }

    Node y;

    while (!(y = getLastChild(x.getPtr())).isNull()) {
        x = y;
    }

    return x;
}


bool testNode_PIName(Node node, AxisHints * hint) {
    U_ASSERT(node.checkp().getNodeType() != pr_ins);

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
  {axis_child,              nextNode_RightSiblingMerge, resolveAxis_XPathStep},
  {axis_descendant,         nextNode_RightSiblingMerge, resolveAxis_XPathStep},
  {axis_attribute,          nextNode_Null,              resolveAxis_XPathStep},
  {axis_self,               nextNode_Null,              resolveAxis_Self},
  {axis_descendant_or_self, nextNode_RightSiblingMerge, resolveAxis_XPathStep},
  {axis_descendant_attr,    /* Depricated */ },
  {axis_parent,             nextNode_Null,              resolveAxis_Parent},

  {axis_any, /* gap */ },

  {axis_ancestor,           nextNode_Parent,            resolveAxis_Parent},
  {axis_ancestor_or_self,   nextNode_Parent,            resolveAxis_Self},
  //
  {axis_following,          nextNode_following,         nextNode_following},
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

    /* Kinda optimization =) */
    if ((nt.axis == axis_child) && ((hint->childTypeMask & ~ti_singleton_element) == 0)) {
        evaluateAxisProc = resolveAxis_ChildType;
        nextNodeProc = nextNode_RightSiblingType;
    }

    if (hint->childTypeMask == pr_ins) {
        testNodeProc = testNode_PIName;
    }

    if ((nt.axis == axis_self) || (nt.axis >= axis_ancestor)) {
        testNodeProc = schemaTest;
    }
}

PPAxisStep::~PPAxisStep()
{
    delete hint;
    delete child.op;
    child.op = NULL;
}

void PPAxisStep::do_open ()
{
    child.op->open();
    currentNodeIndir = XNULL;
}

void PPAxisStep::do_reopen()
{
    child.op->reopen();
    currentNodeIndir = XNULL;
}

void PPAxisStep::do_close()
{
    child.op->close();
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
            currentNode = nextNodeProc(currentNode, hint);
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

            currentNode = evaluateAxisProc(child.get(t).get_node(), hint);
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
