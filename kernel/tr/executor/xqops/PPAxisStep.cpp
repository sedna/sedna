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

#include <algorithm>
#include <queue>

using namespace xpath;

struct NodeDocumentOrderCmp {
  bool operator()(const Node & __x, const Node& __y) const
  { return __x < __y; }
};

typedef std::priority_queue<Node, std::vector<Node>, NodeDocumentOrderCmp> NodeSequenceHeap;
typedef std::vector<schema_node_cptr> SchemaPathList;
typedef std::map<schema_node_xptr, SchemaPathList> DescendantMap;

/**
  AxisHints stores data, needed to effectively evaluate path expressions
*/
struct AxisHints {
    xpath::NodeTest nt;
    t_item childTypeMask;

    DescendantMap descendantPathIndex;
    NodeSequenceHeap nodeHeap;

    AxisHints() : nt(), childTypeMask(element) {};
};

/**
  Evaluates node, following path from \node to \goal
*/
static
xptr getFirstNodeByPath(Node node, schema_node_cptr goal) {
    schema_node_cptr i = goal;
    std::vector<int> path(64);
    xptr nodeI = node.getPtr();
    schema_node_cptr base = node.getSchemaNode();

    while (i.ptr() != base.ptr()) {
        path.push_back(i->getIndex());

        if (i->parent == XNULL) {
            U_ASSERT(false);
        }

        i = i->parent;
    }

    for (std::vector<int>::reverse_iterator i = path.rbegin(); i != path.rend(); ++i) {
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

inline static
comp_schema getValidCFun(AxisHints * hints) {
    switch (hints->nt.type) {
      case node_test_qname : return comp_qname_type; break;
      case node_test_wildcard_star : return comp_type; break;
      case node_test_wildcard_ncname_star : return comp_uri_type; break;
      case node_test_wildcard_star_ncname : return comp_local_type; break;
      default: U_ASSERT(false); return NULL;
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
    if (hint->nodeHeap.empty()) {
        return XNULL;
    } else {
        Node result = hint->nodeHeap.top();
        hint->nodeHeap.pop();
        return result;
    }
}

Node resolveAxis_Descendant(Node node, AxisHints * hint) {
    schema_node_cptr scn = getSchemaNode(node.getPtr());

    /* Find ready (cached) node extraction strategy for given schema node */
    DescendantMap::iterator descMap = hint->descendantPathIndex.find(scn.ptr());

    /* If nothing found, evaluate all available paths from given node, to axis-evaluatable */
    if (descMap == hint->descendantPathIndex.end()) {
        std::vector<schema_node_xptr> schemaNodes;
        SchemaPathList pathList;

        /* Build path list for every resolved node */
        switch (hint->nt.axis) {
          case axis_child:
            getSchemeChildren(scn, hint->nt.uri, hint->nt.local, hint->childTypeMask, getValidCFun(hint), schemaNodes);
            break;
          case axis_descendant:
          case axis_descendant_attr:
            getSchemeDescendants(scn, hint->nt.uri, hint->nt.local, hint->childTypeMask, getValidCFun(hint), schemaNodes);
            break;
          case axis_descendant_or_self:
            getSchemeDescendantsOrSelf(scn, hint->nt.uri, hint->nt.local, hint->childTypeMask, getValidCFun(hint), schemaNodes);
            break;
          default :
            U_ASSERT(false);
        }

        /* Just copy cached result array to our array */
        for (std::vector<schema_node_xptr>::iterator i = schemaNodes.begin(); i != schemaNodes.end(); ++i) {
            pathList.push_back(*i);
        };

        descMap = hint->descendantPathIndex.insert(DescendantMap::value_type(scn.ptr(), pathList)).first;
    }

    U_ASSERT(descMap != hint->descendantPathIndex.end());

    /* Find all nodes for all found pathes for given node.
       We need all nodes to maintain document order.
       MAYBE if document order is not required, we can find just the very first one. */

    traverseSchemaPathList(node, &(descMap->second), &(hint->nodeHeap));

    /* Get the first node from heap */

    Node result = hint->nodeHeap.top();
    hint->nodeHeap.pop();

    return result;
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

Node nextNode_RightSiblingType(Node node, AxisHints * hint) {
    return getRightSiblingByTypeMask(node.getPtr(), hint->childTypeMask);
}


bool testNode_PIName(Node node, AxisHints * hint) {
    if (hint->nt.local != NULL) {
        node.checkp();
        return PINode(node).compareTarget(hint->nt.local) == 0;
    } else {
        return true;
    }
}

bool testNode_Type(Node node, AxisHints * hint) {
    return (node.checkp().getSchemaNode()->type & hint->childTypeMask) > 0;
}

bool testNode_QName(Node node, AxisHints * hint) {
    return node.checkp().getSchemaNode()->node_matches(hint->nt.uri, hint->nt.local, ti_all);
}


struct __axis_resolution_t {
    Axis axis;
    NodeTestType ntt;
    NextNodeProc nextProc;
    TestNodeProc testProc;
    EvaluateAxisProc evalProc;
    t_item childTypeMask;
};

static const __axis_resolution_t procArray[] = {
  {axis_child, node_test_pi,        nextNode_RightSiblingSame, testNode_PIName, resolveAxis_ChildType, pr_ins},
/*
{axis_child, node_test_comment,   nextNode_RightSiblingSame, NULL,            resolveAxis_ChildType, comment},
  {axis_child, node_test_text,      nextNode_RightSiblingSame, NULL,            resolveAxis_ChildType, text},
  {axis_child, node_test_node,      nextNode_RightSiblingType,  NULL,           resolveAxis_ChildType, ti_dmchildren},
  {axis_child, node_test_element,   nextNode_RightSiblingType, NULL,            resolveAxis_ChildType, element},
  {axis_child, node_test_attribute, nextNode_RightSiblingType, NULL,            resolveAxis_ChildType, attribute},
  {axis_child, node_test_document,  nextNode_RightSiblingSame, NULL,            resolveAxis_ChildType, document},

  {axis_child, node_test_qname,                 nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, ti_dmchildren},
  {axis_child, node_test_wildcard_star,         nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, ti_dmchildren},
  {axis_child, node_test_wildcard_ncname_star,  nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, ti_dmchildren},
  {axis_child, node_test_wildcard_star_ncname,  nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, ti_dmchildren},

  {axis_descendant, node_test_pi,        nextNode_RightSiblingMerge, testNode_PIName, resolveAxis_Descendant, pr_ins},
  {axis_descendant, node_test_comment,   nextNode_RightSiblingMerge, NULL,            resolveAxis_Descendant, comment},
  {axis_descendant, node_test_text,      nextNode_RightSiblingMerge, NULL,            resolveAxis_Descendant, text},
  {axis_descendant, node_test_node,      nextNode_RightSiblingMerge, NULL,            resolveAxis_Descendant, ti_dmchildren},
  {axis_descendant, node_test_element,   nextNode_RightSiblingMerge, NULL,            resolveAxis_Descendant, element},
  {axis_descendant, node_test_attribute, nextNode_RightSiblingMerge, NULL,            resolveAxis_Descendant, attribute},
  {axis_descendant, node_test_document,  nextNode_RightSiblingMerge, NULL,            resolveAxis_Descendant, document},

  {axis_descendant, node_test_qname,                 nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, ti_dmchildren},
  {axis_descendant, node_test_wildcard_star,         nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, ti_dmchildren},
  {axis_descendant, node_test_wildcard_ncname_star,  nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, ti_dmchildren},
  {axis_descendant, node_test_wildcard_star_ncname,  nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, ti_dmchildren},

  {axis_attribute, node_test_pi,        nextNode_Null, NULL, resolveAxis_ChildType, 0},
  {axis_attribute, node_test_comment,   nextNode_Null, NULL, resolveAxis_ChildType, 0},
  {axis_attribute, node_test_text,      nextNode_Null, NULL, resolveAxis_ChildType, 0},
  {axis_attribute, node_test_node,      nextNode_Null, NULL, resolveAxis_ChildType, attribute},
  {axis_attribute, node_test_element,   nextNode_Null, NULL, resolveAxis_ChildType, 0},
  {axis_attribute, node_test_attribute, nextNode_Null, NULL, resolveAxis_ChildType, attribute},
  {axis_attribute, node_test_document,  nextNode_Null, NULL, resolveAxis_ChildType, 0},

  {axis_attribute, node_test_qname,                 nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, attribute},
  {axis_attribute, node_test_wildcard_star,         nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, attribute},
  {axis_attribute, node_test_wildcard_ncname_star,  nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, attribute},
  {axis_attribute, node_test_wildcard_star_ncname,  nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, attribute},

  {axis_self, node_test_pi,        nextNode_Null, testNode_Type, resolveAxis_Self, pr_ins},
  {axis_self, node_test_comment,   nextNode_Null, testNode_Type, resolveAxis_Self, comment},
  {axis_self, node_test_text,      nextNode_Null, testNode_Type, resolveAxis_Self, text},
  {axis_self, node_test_node,      nextNode_Null, NULL,          resolveAxis_Self, ti_dmchildren},
  {axis_self, node_test_element,   nextNode_Null, testNode_Type, resolveAxis_Self, element},
  {axis_self, node_test_attribute, nextNode_Null, testNode_Type, resolveAxis_Self, attribute},
  {axis_self, node_test_document,  nextNode_Null, testNode_Type, resolveAxis_Self, document},

  {axis_self, node_test_qname,                 nextNode_Null, testNode_QName, resolveAxis_Self, ti_dmchildren},
  {axis_self, node_test_wildcard_star,         nextNode_Null, testNode_QName, resolveAxis_Self, ti_dmchildren},
  {axis_self, node_test_wildcard_ncname_star,  nextNode_Null, testNode_QName, resolveAxis_Self, ti_dmchildren},
  {axis_self, node_test_wildcard_star_ncname,  nextNode_Null, testNode_QName, resolveAxis_Self, ti_dmchildren},

  {axis_descendant_or_self, node_test_pi,        nextNode_RightSiblingMerge, testNode_PIName, resolveAxis_Descendant, pr_ins},
  {axis_descendant_or_self, node_test_comment,   nextNode_RightSiblingMerge, NULL,            resolveAxis_Descendant, comment},
  {axis_descendant_or_self, node_test_text,      nextNode_RightSiblingMerge, NULL,            resolveAxis_Descendant, text},
  {axis_descendant_or_self, node_test_node,      nextNode_RightSiblingMerge, NULL,            resolveAxis_Descendant, ti_dmchildren},
  {axis_descendant_or_self, node_test_element,   nextNode_RightSiblingMerge, NULL,            resolveAxis_Descendant, element},
  {axis_descendant_or_self, node_test_attribute, nextNode_RightSiblingMerge, NULL,            resolveAxis_Descendant, attribute},
  {axis_descendant_or_self, node_test_document,  nextNode_RightSiblingMerge, NULL,            resolveAxis_Descendant, document},

  {axis_descendant_or_self, node_test_qname,                 nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, ti_dmchildren},
  {axis_descendant_or_self, node_test_wildcard_star,         nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, ti_dmchildren},
  {axis_descendant_or_self, node_test_wildcard_ncname_star,  nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, ti_dmchildren},
  {axis_descendant_or_self, node_test_wildcard_star_ncname,  nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, ti_dmchildren},

  {axis_descendant_attr, node_test_pi,        nextNode_RightSiblingMerge, testNode_PIName, resolveAxis_Descendant, 0},
  {axis_descendant_attr, node_test_comment,   nextNode_RightSiblingMerge, NULL,            resolveAxis_Descendant, 0},
  {axis_descendant_attr, node_test_text,      nextNode_RightSiblingMerge, NULL,            resolveAxis_Descendant, 0},
  {axis_descendant_attr, node_test_node,      nextNode_RightSiblingMerge, NULL,            resolveAxis_Descendant, attribute},
  {axis_descendant_attr, node_test_element,   nextNode_RightSiblingMerge, NULL,            resolveAxis_Descendant, 0},
  {axis_descendant_attr, node_test_attribute, nextNode_RightSiblingMerge, NULL,            resolveAxis_Descendant, attribute},
  {axis_descendant_attr, node_test_document,  nextNode_RightSiblingMerge, NULL,            resolveAxis_Descendant, 0},

  {axis_descendant_attr, node_test_qname,                 nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, attribute},
  {axis_descendant_attr, node_test_wildcard_star,         nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, attribute},
  {axis_descendant_attr, node_test_wildcard_ncname_star,  nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, attribute},
  {axis_descendant_attr, node_test_wildcard_star_ncname,  nextNode_RightSiblingMerge, NULL, resolveAxis_Descendant, attribute},

  {axis_parent, node_test_pi,        nextNode_Null, testNode_Type, resolveAxis_Parent, pr_ins},
  {axis_parent, node_test_comment,   nextNode_Null, testNode_Type, resolveAxis_Parent, comment},
  {axis_parent, node_test_text,      nextNode_Null, testNode_Type, resolveAxis_Parent, text},
  {axis_parent, node_test_node,      nextNode_Null, NULL,          resolveAxis_Parent, ti_dmchildren},
  {axis_parent, node_test_element,   nextNode_Null, testNode_Type, resolveAxis_Parent, element},
  {axis_parent, node_test_attribute, nextNode_Null, testNode_Type, resolveAxis_Parent, attribute},
  {axis_parent, node_test_document,  nextNode_Null, testNode_Type, resolveAxis_Parent, document},

  {axis_parent, node_test_qname,                 nextNode_Null, testNode_QName, resolveAxis_Parent, ti_dmchildren},
  {axis_parent, node_test_wildcard_star,         nextNode_Null, testNode_QName, resolveAxis_Parent, ti_dmchildren},
  {axis_parent, node_test_wildcard_ncname_star,  nextNode_Null, testNode_QName, resolveAxis_Parent, ti_dmchildren},
  {axis_parent, node_test_wildcard_star_ncname,  nextNode_Null, testNode_QName, resolveAxis_Parent, ti_dmchildren},

  {axis_ancestor, node_test_pi,        nextNode_Parent, testNode_Type, resolveAxis_Parent, pr_ins},
  {axis_ancestor, node_test_comment,   nextNode_Parent, testNode_Type, resolveAxis_Parent, comment},
  {axis_ancestor, node_test_text,      nextNode_Parent, testNode_Type, resolveAxis_Parent, text},
  {axis_ancestor, node_test_node,      nextNode_Parent, NULL,          resolveAxis_Parent, ti_dmchildren},
  {axis_ancestor, node_test_element,   nextNode_Parent, testNode_Type, resolveAxis_Parent, element},
  {axis_ancestor, node_test_attribute, nextNode_Parent, testNode_Type, resolveAxis_Parent, attribute},
  {axis_ancestor, node_test_document,  nextNode_Parent, testNode_Type, resolveAxis_Parent, document},

  {axis_ancestor, node_test_qname,                 nextNode_Parent, testNode_QName, resolveAxis_Parent, ti_dmchildren},
  {axis_ancestor, node_test_wildcard_star,         nextNode_Parent, testNode_QName, resolveAxis_Parent, ti_dmchildren},
  {axis_ancestor, node_test_wildcard_ncname_star,  nextNode_Parent, testNode_QName, resolveAxis_Parent, ti_dmchildren},
  {axis_ancestor, node_test_wildcard_star_ncname,  nextNode_Parent, testNode_QName, resolveAxis_Parent, ti_dmchildren},

  {axis_ancestor_or_self, node_test_pi,        nextNode_Parent, testNode_Type, resolveAxis_Self, pr_ins},
  {axis_ancestor_or_self, node_test_comment,   nextNode_Parent, testNode_Type, resolveAxis_Self, comment},
  {axis_ancestor_or_self, node_test_text,      nextNode_Parent, testNode_Type, resolveAxis_Self, text},
  {axis_ancestor_or_self, node_test_node,      nextNode_Parent, NULL,          resolveAxis_Self, ti_dmchildren},
  {axis_ancestor_or_self, node_test_element,   nextNode_Parent, testNode_Type, resolveAxis_Self, element},
  {axis_ancestor_or_self, node_test_attribute, nextNode_Parent, testNode_Type, resolveAxis_Self, attribute},
  {axis_ancestor_or_self, node_test_document,  nextNode_Parent, testNode_Type, resolveAxis_Self, document},

  {axis_ancestor_or_self, node_test_qname,                 nextNode_Parent, testNode_QName, resolveAxis_Self, ti_dmchildren},
  {axis_ancestor_or_self, node_test_wildcard_star,         nextNode_Parent, testNode_QName, resolveAxis_Self, ti_dmchildren},
  {axis_ancestor_or_self, node_test_wildcard_ncname_star,  nextNode_Parent, testNode_QName, resolveAxis_Self, ti_dmchildren},
  {axis_ancestor_or_self, node_test_wildcard_star_ncname,  nextNode_Parent, testNode_QName, resolveAxis_Self, ti_dmchildren},
*/
};

PPAxisStep::PPAxisStep(dynamic_context* _cxt_, operation_info _info_, PPOpIn _child_, NodeTest _nt_)
  : PPIterator(_cxt_, _info_, "PPAxisStep"), child(_child_), nt(_nt_), currentNodeIndir(XNULL), hint(new AxisHints)
{
    U_ASSERT((nt.axis > axis_any && nt.axis < __axis_last) || (nt.type > node_test_invalid && nt.type < __node_test_last));

    int n = (nt.axis - axis_any) * 11 + (nt.type - node_test_invalid);

    U_ASSERT(procArray[n].axis == nt.axis);
    U_ASSERT(procArray[n].ntt == nt.type);

    hint->nt = nt;
    nextNodeProc = procArray[n].nextProc;
    evaluateAxisProc = procArray[n].evalProc;
    testNodeProc = procArray[n].testProc;
    hint->childTypeMask = procArray[n].childTypeMask;
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
