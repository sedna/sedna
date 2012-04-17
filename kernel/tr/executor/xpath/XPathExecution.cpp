#include "XPathExecution.h"
#include "XPathTypes.h"
#include <tr_functions.h>

/**
 *  Evaluates node path, following path from \node to \goal
 */

#ifdef __COMMOUT

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

    for (std::vector<int>::const_iterator i = path.begin(); i != path.end(); ++i) {
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
     *       Push it if it is not XNULL. */
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
     *       Push it if it is not XNULL. */
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









/* TRICKY Be sure not to use virtual_root in node test, as it used to determine principal node kind */
const t_item principal_nk = ti_all;

const static struct {
    pe::node_test_t check; t_item x;
} nodeTypeTestMap[] = {
    {(pe::NodeTestType) -1 /* xpath::node_test_invalid */},
    {pe::nt_document, document},
    {pe::nt_element, element},
    {pe::nt_attribute, attribute},
    {pe::nt_schema_element, element},
    {pe::nt_schema_attribute, attribute},
    {pe::nt_pi, pr_ins},
    {pe::nt_comment, comment},
    {pe::nt_text, text},
    {pe::nt_any_kind, ti_all_valid},

    {pe::nt_qname, principal_nk},
    {pe::nt_wildcard_star, principal_nk},
    {pe::nt_wildcard_prefix, principal_nk},
    {pe::nt_wildcard_name, principal_nk},

    {(pe::NodeTestType) -1},
};

static inline
t_item getNodeTestTypeMask(pe::node_test_t type, pe::axis_t axis) {
    /* TRICKY ! Node kind resolution is done as described here: http://www.w3.org/TR/xquery/#dt-principal-node-kind */
    U_ASSERT(nodeTypeTestMap[type].check == nt.type);

    t_item node_kind = nodeTypeTestMap[type].x;

    if (node_kind == principal_nk) {
        /* http://www.w3.org/TR/xquery/#dt-principal-node-kind */
        return (axis == pe::axis_attribute) ? attribute : element;
    } else if (axis == pe::axis_attribute) {
        return (t_item) (node_kind & attribute);
    } else if (axis == pe::axis_child || axis == pe::axis_descendant || axis == pe::axis_descendant_or_self || axis >= pe::axis_following) {
        return (t_item) (node_kind & ti_dmchildren);
    } else {
        return node_kind;
    }
};

struct SchemaNodeData {
    t_item m_type;
    const char * m_uri;
    const char * m_local;
    
    SchemaNodeData(t_item type, const char * uri, const char * local) : m_type(type), m_uri(uri), m_local(local) {};
};

struct SchemaTestOperatorType {
    inline static bool test(schema_node_cptr node, const SchemaNodeData * data) { return (node->type & data->m_type) > 0; };
    inline static bool testref(const sc_ref &ref, const SchemaNodeData * data) { return (ref.type & data->m_type) > 0; };
};

struct SchemaTestOperatorQNameType {
    inline static bool test(schema_node_cptr node, const SchemaNodeData * data) { return node->matches(data->m_uri, data->m_local, data->m_type); };
    inline static bool testref(const sc_ref &ref, const SchemaNodeData * data) { return ref.matches(data->m_uri, data->m_local, data->m_type); };
};

struct SchemaTestOperatorLocalType {
    inline static bool test(schema_node_cptr node, const SchemaNodeData * data) { return SchemaTestOperatorType::test(node, data) && strcmpex(node->get_name(), data->m_local) == 0; };
    inline static bool testref(const sc_ref &ref, const SchemaNodeData * data) { return SchemaTestOperatorType::testref(ref, data) && strcmpex(ref.name, data->m_local) == 0; };
};

struct SchemaTestOperatorUriType {
    inline static bool test(schema_node_cptr node, const SchemaNodeData * data) { return SchemaTestOperatorType::test(node, data) && same_xmlns_uri(node->get_xmlns(), data->m_uri); };
    inline static bool testref(const sc_ref &ref, const SchemaNodeData * data) { return SchemaTestOperatorType::testref(ref, data) && same_xmlns_uri(ref.get_xmlns(), data->m_uri); };
};

#define CAT_FOR_EACH(T, i, list) for (cat_list<T>::item * i = list->first; i != NULL; i = i->next)

class PathLookupCallback {
public:
    const pe::Path& path;
    SchemaPathList * pathList;
    int path_step;
    SchemaPath * prefixPath;

    PathLookupCallback(const pe::Path& _path, SchemaPathList * _pathList, SchemaPath * _prefixPath)
    : path(_path), pathList(_pathList), path_step(0), prefixPath(_prefixPath) {};

    PathLookupCallback(PathLookupCallback * parent, int _path_step)
     : path(parent->path), pathList(parent->pathList), path_step(_path_step), prefixPath(parent->prefixPath) {};

    execute(schema_node_cptr node);
};

class ISchemaTest {
public:
    virtual bool test(schema_node_cptr node) const;
    virtual t_item getTypeMask() const;
    virtual void getChildren(schema_node_cptr node, PathLookupCallback * callback) const;
    virtual void getDescendants(schema_node_cptr node, PathLookupCallback * callback) const;
};

template<typename TestOp>
class SchemaTest : public ISchemaTest {
private:
    SchemaNodeData data;
public:
    SchemaTest(t_item _type, const char * _uri, const char * _local) : data(_type, _uri, _local) { };

    virtual bool test(schema_node_cptr node) const {
        return TestOp::test(node, &data);
    };

    virtual t_item getTypeMask() const { return data.m_type; };

    virtual void getChildren(schema_node_cptr node, PathLookupCallback * callback) const {
        int childIndex = 0;

        CAT_FOR_EACH(sc_ref, i, (node->children)) {
            if (TestOp::testref(i->object, &data)) {
                callback->prefixPath->push_back(childIndex);
                callback->execute(i->object.snode);
                callback->prefixPath->pop_back();
            };
            childIndex++;
        }
    }

    virtual void getDescendants(schema_node_cptr node, PathLookupCallback * callback) const {
        int childIndex = 0;

        CAT_FOR_EACH(sc_ref, i, (node->children)) {
            callback->prefixPath->push_back(childIndex);

            if (TestOp::testref(i->object, &data)) {
                callback->execute(i->object.snode);
            };

            if ((i->object.type & (element | document)) > 0) {
                getDescendants(i->object.snode, callback);
            }

            callback->prefixPath->pop_back();
            childIndex++;
        }
    }
};

ISchemaTest* createSchemaTest(pe::node_test_t type, pe::axis_t axis, const xsd::QName& qname)
{
    t_item typeTestMasks = getNodeTestTypeMask(type, axis);

    if (typeTestMasks == 0) {
        return NULL;
    }

    if ((typeTestMasks & (element | attribute)) == 0) {
        return new SchemaTest<SchemaTestOperatorType>(typeTestMasks, NULL, NULL);
    } else switch (type) {
        case pe::nt_qname:
            return new SchemaTest<SchemaTestOperatorQNameType>(typeTestMasks, qname.getUri(), qname.getLocalName());
            break;
        case pe::nt_wildcard_star:
            return new SchemaTest<SchemaTestOperatorType>(typeTestMasks, NULL, NULL);
            break;
        case pe::nt_wildcard_name :
            return new SchemaTest<SchemaTestOperatorUriType>(typeTestMasks, qname.getUri(), NULL);
            break;
        case pe::nt_wildcard_prefix :
            return new SchemaTest<SchemaTestOperatorLocalType>(typeTestMasks, NULL, qname.getLocalName());
            break;
        case pe::nt_element:
        case pe::nt_attribute:
        default:
            if (!qname.valid()) {
                return new SchemaTest<SchemaTestOperatorType>(typeTestMasks, NULL, NULL);
            } else {
                return new SchemaTest<SchemaTestOperatorQNameType>(typeTestMasks, qname.getUri(), qname.getLocalName());
            }
            break;
    }

    return NULL;
}


PathLookupCallback::execute(schema_node_cptr node)
{
    if (path_step >= path.getBody()->size()) {
        pathList->push_back(*prefixPath);
    };

    const pe::Step & step = path.getBody()->at(path_step);
    scoped_ptr<ISchemaTest> scmtest(createSchemaTest(step.getTest(), step.getAxis(), step.getQName()));
    scoped_ptr<PathLookupCallback> nextCallback(this, path_step + 1);

    switch (step.getAxis()) {
        case pe::axis_attribute :
        case pe::axis_child :
            scmtest->getChildren(node, nextCallback);
            break;

        case pe::axis_self :
        case pe::axis_descendant_or_self :
            if (scoped_ptr<ISchemaTest>(createSchemaTest(step.getTest(), pe::axis_self, step.getQName()))->test(node)) {
                nextCallback->execute(node);
            };

            if (step.getAxis() == pe::axis_self) {
                break;
            }

        case pe::axis_descendant : 
            scmtest->getDescendants(node, nextCallback);
            break;

        default:
            U_ASSERT(false);
            break;
    };

    scmtest->getChildren(node);
}


void executePathLookup(schema_node_cptr node, const pe::Path& path, SchemaPathList * pathList)
{
    SchemaPath pathStack;
    PathLookupCallback initialCallback(path, pathList, &pathStack);
    
}

/*
 *  TODO: Descendant axis is evaluated as follows: HOOOWWWWW!
 */

Node resolveAxis_Descendant(Node node, AxisHints * hint) {
    schema_node_cptr scn = getSchemaNode(node.getPtr());
    
    /* Find ready (cached) node extraction strategy for given schema node */
    DescendantMap::iterator descMap = hint->descendantPathIndex.find(scn.ptr());
    
    /* If nothing found, evaluate all available paths from given node, to axis-evaluatable */
    if (descMap == hint->descendantPathIndex.end()) {
        t_scmnodes schemaNodes;
        SchemaPathList pathList;
        
        /* Build path list for every resolved node */
        executeNodeTestPath(scn, hint->nt, &schemaNodes, &pathList);
        
        descMap = hint->descendantPathIndex.insert(DescendantMap::value_type(scn.ptr(), pathList)).first;
    }
    
    U_ASSERT(descMap != hint->descendantPathIndex.end());
    
    /* Find all nodes for all found pathes for given node.
     *       We need all nodes to maintain document order.
     *       MAYBE if document order is not required, we can find just the very first one. */
    
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

Node resolveAxis_ChildFirst(Node node, AxisHints * hint) {
    return getFirstChild(node.getPtr());
}

Node resolveAxis_Self(Node node, AxisHints * hint) {
    return node;
}

Node resolveAxis_NULL(Node node, AxisHints * hint) {
    return XNULL;
}

Node nextNode_traverseAll(Node node, AxisHints * hint) {
    node.checkp();
    
    // Try to go to child level
    if ((node.getNodeType() & (element | document)) > 0) {
        Node nextNode = getFirstChildByTypeMask(node.getPtr(), hint->childTypeMask);
        
        if (!nextNode.isNull()) {
            hint->nodeStackPush(node);
            return nextNode;
        }
    };
    
    // Next or up
    while (!node.isNull() && node.getPtr() != hint->baseNode.getPtr()) {
        node = node.checkp().getRight();
        
        if (!node.isNull()) {
            return node;
        }
        
        node = hint->nodeStackPop();
    }
    
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
    
    if (node == hint->baseNode && hint->selfSchemaTest != NULL) {
        return hint->selfSchemaTest->test(node.checkp().getSchemaNode());
    } else if (hint->schemaTest == NULL || !hint->schemaTest->test(node.checkp().getSchemaNode())) {
        return false;
    } else {
        if (node.getNodeType() == pr_ins && hint->nt.getLocal().valid()) {
            return PINode(node).compareTarget(hint->nt.getLocal().getValue()) == 0;
        } else {
            return true;
        }
    }
}

#endif