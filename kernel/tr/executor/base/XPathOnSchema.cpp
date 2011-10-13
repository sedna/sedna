/*
 * File:  XPathOnSchema.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "tr/executor/base/XPathOnSchema.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/dm_accessors.h"
#include "common/errdbg/d_printf.h"

#include "tr/structures/nodetypes.h"

using namespace xpath;

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
// #define FILTER_SCHEMA_OFFSPRINGS(n, x, i, test, action) CAT_FOR_EACH(sc_ref, i, (n->children)) { schema_node_cptr x = i->object.snode; if (test) { action; }; }
// #define FILTER_SCHEMA_OFFSPRINGS_FAST(n, i, test, action) CAT_FOR_EACH(sc_ref, i, (n->children)) { if (test) { action; }; }


template<typename TestOp>
class SchemaTest : public ISchemaTest {
  private:
    SchemaNodeData data;
    mutable SchemaPath currentPath;
  public:
    SchemaTest(t_item _type, const char * _uri, const char * _local) : data(_type, _uri, _local) {
        pathCollection = NULL;
        result = NULL;
    };

    virtual bool empty() const { return data.m_type == 0; };

    virtual bool test(schema_node_cptr node) const {
        return TestOp::test(node, &data);
    };

    virtual t_item getTypeMask() const { return data.m_type; };

    virtual void getOffsprings(schema_node_cptr node) const {
        int childIndex = 0;

        CAT_FOR_EACH(sc_ref, i, (node->children)) {
            if (TestOp::testref(i->object, &data)) {
                result->push_back(i->object.snode);
                if (pathCollection != NULL) { pathCollection->push_back(SchemaPath(1, childIndex)); };
            };
            childIndex++;
        }
    }

    virtual void getDescendants(schema_node_cptr node) const {
        int childIndex = 0;

        CAT_FOR_EACH(sc_ref, i, (node->children)) {
            if (pathCollection != NULL) { currentPath.push_back(childIndex); }

            if (TestOp::testref(i->object, &data)) {
                result->push_back(i->object.snode);

                if (pathCollection != NULL) { pathCollection->push_back(currentPath); };
            };

            if ((i->object.type & (element | document)) > 0) {
                getDescendants(i->object.snode);
            }

            if (pathCollection != NULL) { currentPath.pop_back(); }
            childIndex++;
        }
    }

    virtual void getOffsprings(const t_scmnodes_set& list) const {
        for (t_scmnodes_set::const_iterator i = list.begin(); i != list.end(); i++) {
            if (TestOp::test(*i, &data)) {
                result->push_back(*i);
            }
        }
    }

    virtual void getDescendants(const t_scmnodes_set& list) const {
        for (t_scmnodes_set::const_iterator i = list.begin(); i != list.end(); i++) {
            if (TestOp::test(*i, &data)) {
                result->push_back(*i);
            }

            if (((*i)->type & (element | document)) > 0) {
                getDescendants((*i));
            }
        }
    }
};

/* TRICKY Be sure not to use virtual_root in node test, as it used to determine principal node kind */
const t_item principal_nk = ti_all;

const static struct {
    xpath::NodeTestType check; t_item x;
} nodeTypeTestMap[] = {
  {(xpath::NodeTestType) -1 /* xpath::node_test_invalid */},
  {xpath::node_test_pi,                     pr_ins},
  {xpath::node_test_comment,                comment},
  {xpath::node_test_text,                   text},
  {xpath::node_test_node,                   ti_all_valid},
  {xpath::node_test_element,                element},
  {xpath::node_test_attribute,              attribute},
  {xpath::node_test_document,               document},
  {xpath::node_test_qname,                  principal_nk},
  {xpath::node_test_wildcard_star,          principal_nk},
  {xpath::node_test_wildcard_ncname_star,   principal_nk},
  {xpath::node_test_wildcard_star_ncname,   principal_nk},
  {(xpath::NodeTestType) -1},
};

static inline
t_item getNodeTestTypeMask(const xpath::NodeTest& nt) {
  /* TRICKY ! Node kind resolution is done as described here: http://www.w3.org/TR/xquery/#dt-principal-node-kind */
    U_ASSERT(nodeTypeTestMap[nt.type].check == nt.type);

    t_item node_kind = nodeTypeTestMap[nt.type].x;

    if (node_kind == principal_nk) {
      /* http://www.w3.org/TR/xquery/#dt-principal-node-kind */
        return (nt.axis == axis_attribute || nt.axis == axis_descendant_attr) ? attribute : element;
    } else if (nt.axis == axis_attribute || nt.axis == axis_descendant_attr) {
        return (t_item) (node_kind & attribute);
    } else if (nt.axis == axis_child || nt.axis == axis_descendant || nt.axis == axis_descendant_or_self || nt.axis >= axis_following) {
        return (t_item) (node_kind & ti_dmchildren);
    } else {
        return node_kind;
    }
};

ISchemaTest* createSchemaTest(const xpath::NodeTest& nt)
{
    t_item typeTestMasks = getNodeTestTypeMask(nt);
    xsd::QName qname = nt.getQName();

    if (typeTestMasks == 0) {
        return NULL;
    }

    if ((typeTestMasks & (element | attribute)) == 0) {
        return new SchemaTest<SchemaTestOperatorType>(typeTestMasks, NULL, NULL);
    } else switch (nt.type) {
      case xpath::node_test_qname:
        return new SchemaTest<SchemaTestOperatorQNameType>(typeTestMasks, qname.getUri(), qname.getLocalName());
        break;
      case xpath::node_test_wildcard_star:
        return new SchemaTest<SchemaTestOperatorType>(typeTestMasks, NULL, NULL);
        break;
      case xpath::node_test_wildcard_ncname_star:
        return new SchemaTest<SchemaTestOperatorUriType>(typeTestMasks, nt.getUri().getValue(), NULL);
        break;
      case xpath::node_test_wildcard_star_ncname:
        return new SchemaTest<SchemaTestOperatorLocalType>(typeTestMasks, NULL, nt.getLocal().getValue());
        break;
      case xpath::node_test_element:
      case xpath::node_test_attribute:
      default:
        if (nt.isAnyQName()) {
            return new SchemaTest<SchemaTestOperatorType>(typeTestMasks, NULL, NULL);
        } else {
            return new SchemaTest<SchemaTestOperatorQNameType>(typeTestMasks, qname.getUri(), qname.getLocalName());
        }
        break;
    }

    return NULL;
}

static
void traverseDescendants(ISchemaTest* schema_test, const schema_node_cptr &node, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes) {
    if (extended_nodes && extender_nodes && extended_nodes->find(node.ptr()) != extended_nodes->end()) {
        for (t_scmnodes_set::iterator i = extender_nodes->begin(); i != extender_nodes->end(); i++) {
            schema_test->getDescendants(*extender_nodes);

            traverseDescendants(schema_test, *i, extended_nodes, extender_nodes);
        }
    }
}

void executeNodeTestPath(schema_node_cptr node, const xpath::NodeTest& nt, t_scmnodes* result, SchemaPathList* pathList)
{
    scoped_ptr<ISchemaTest> schemaTest(createSchemaTest(nt));
    scoped_ptr<ISchemaTest> selfSchemaTest;

    schemaTest->setResultCollector(result);
    schemaTest->setPathCollector(pathList);

    if (nt.axis == axis_descendant_or_self || nt.axis == axis_self) {
        NodeTest selfnt = nt;
        selfnt.axis = axis_self;
        selfSchemaTest = createSchemaTest(selfnt);
        selfSchemaTest->setResultCollector(result);
    }

    if (schemaTest.isnull() && selfSchemaTest.isnull()) {
        return;
    }

    if (nt.axis == axis_descendant_or_self || nt.axis == axis_self) {
        if (selfSchemaTest->test(node)) {
            result->push_back(node.ptr());
            pathList->push_back(SchemaPath());
        }
    }

    if (nt.axis == axis_descendant_or_self || nt.axis == axis_descendant || nt.axis == axis_descendant_attr) {
        schemaTest->getDescendants(node);
    } else if (nt.axis == axis_child || nt.axis == axis_attribute) {
        schemaTest->getOffsprings(node);
    }
}


void executeNodeTest(schema_node_cptr node, const NodeTest& nt, t_scmnodes* result,
    t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes)
{
    bool extend = extended_nodes && extender_nodes && extended_nodes->find(node.ptr()) != extended_nodes->end();

    scoped_ptr<ISchemaTest> schemaTest(createSchemaTest(nt));
    scoped_ptr<ISchemaTest> selfSchemaTest;

    schemaTest->setResultCollector(result);

    if (nt.axis == axis_descendant_or_self || nt.axis == axis_self) {
        NodeTest selfnt = nt;
        selfnt.axis = axis_self;
        selfSchemaTest = createSchemaTest(selfnt);
        selfSchemaTest->setResultCollector(result);
    }

    if (schemaTest.isnull() && selfSchemaTest.isnull()) {
        return;
    }

    if (nt.axis == axis_descendant_or_self || nt.axis == axis_self) {
        if (selfSchemaTest->test(node)) {
            result->push_back(node.ptr());
        }
    }

    if (nt.axis == axis_descendant_or_self || nt.axis == axis_descendant || nt.axis == axis_descendant_attr) {
        schemaTest->getDescendants(node);

        if (extend) {
            traverseDescendants(schemaTest.get(), node, extended_nodes, extender_nodes);
        }
    } else if (nt.axis == axis_child || nt.axis == axis_attribute) {
        schemaTest->getOffsprings(node);

        if (extend) {
            schemaTest->getOffsprings(*extender_nodes);
        }
    }
}

t_scmnodes * executePathExpression(schema_node_cptr node, const PathExpression &pe, t_scmnodes * result,
                                   t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes)
{
    return executePathExpression(t_scmnodes(1, node.ptr()), pe, result, extended_nodes, extender_nodes);
}

static
void sort_unique(t_scmnodes * buf) {
    std::sort(buf->begin(), buf->end(), CompareSchemaNode());
    t_scmnodes::iterator newEnd = std::unique(buf->begin(), buf->end());
    buf->resize(newEnd - buf->begin());
}

t_scmnodes * executePathExpression(const t_scmnodes& nodes, const PathExpression &pe, t_scmnodes * result,
                                   t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes)
{
    t_scmnodes buffer1(nodes.begin(), nodes.end()), buffer2;
    t_scmnodes * nodeBuffer = &buffer1;
    t_scmnodes * newBuffer = &buffer2;

    // Each path expression step
    for (size_t i = 0; i < pe.size(); ++i) {
        newBuffer->clear();

        // Union on every step
        const NodeTestUnion testUnion = pe[i];
        for (size_t j = 0; j < testUnion.size; ++j) {
            std::for_each(nodeBuffer->begin(), nodeBuffer->end(),
                          ExecuteNodeTest(testUnion.nodes[j], extended_nodes, extender_nodes, newBuffer));
        }

        t_scmnodes * swap = nodeBuffer;
        nodeBuffer = newBuffer;
        newBuffer = swap;
    }

    result->insert(result->end(), nodeBuffer->begin(), nodeBuffer->end());
    sort_unique(result);

    return result;
}
