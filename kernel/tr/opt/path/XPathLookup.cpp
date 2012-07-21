/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/errdbg/exceptions.h"

#include "DataSources.h"
#include "XPathLookup.h"
#include "XPathExecution.h"
#include "tr/strings/strings.h"
#include "tr/models/XmlConstructor.h"

#include <stack>
#include <queue>

using namespace pe;

OPINFO_DEF(PathSchemaCheck)
OPINFO_DEF(PathEvaluateTraverse)
OPINFO_DEF(PathSchemaResolve)

SchemaLookup& SchemaLookup::compile()
{
    if (atomizedPath.empty() && !path.empty()) {
        atomizedPath = AtomizedPath(path.getBody()->begin(), path.getBody()->end());
    }

    reversePath = atomizedPath.reverse();

    return *this;
}

int SchemaLookup::findSomething(const DataRoot& root, SchemaNodePtrList* output, int limit)
{
    AtomizedPath path = atomizedPath;

    if (!reversePath.empty() && atomizedPath.cost() > reversePath.cost()) {
        path = reversePath;
    };
    
    AtomizedPathVector::const_iterator it;
    SchemaTestAtom * firstNodeTest = NULL;
    doc_schema_node_cptr rootNode = root.getSchemaNode().ptr();

    int i = 0;

    for (it = path.begin(); it != path.end(); ++it) {
        SchemaTestAtom * candidate = dynamic_cast<SchemaTestAtom *>(*it);

        if (candidate != NULL && candidate->nt != pe::nt_type_test) {
            firstNodeTest = candidate;
            break;
        };

        ++i;
    };

    std::vector<schema_node_xptr> nodeCandidates;

    if (firstNodeTest != NULL) {
        rootNode->find_descendant(firstNodeTest->qname.getLocalName(), firstNodeTest->itemType, &nodeCandidates);

        AtomizedPath checkPath(path, i, path.size());

        std::vector<schema_node_xptr>::iterator it = nodeCandidates.begin();

        while (it != nodeCandidates.end()) {
            if (!executeSchemaPathTest(*it, checkPath, NULL, true)) {
                it = nodeCandidates.erase(it);
            } else {
                ++it;
            }
        };
    }

    std::copy(nodeCandidates.begin(), nodeCandidates.end(), std::back_inserter(*output));

    return path.size() - i - 1;
}

bool SchemaLookup::exists(schema_node_cptr base)
{
    return executeSchemaPathTest(base.ptr(), atomizedPath, NULL);
}

void SchemaLookup::execute(schema_node_cptr base, SchemaNodePtrList * output)
{
    SchemaNodePtrSet goalSet;
    executeSchemaPathTest(base.ptr(), atomizedPath, &goalSet);
    std::copy(goalSet.begin(), goalSet.end(), std::back_inserter(*output));
}

void SchemaLookup::executeAll(const SchemaNodePtrList* in, SchemaNodePtrList* output)
{
    SchemaNodePtrSet goalSet;

    for (SchemaNodePtrList::const_iterator it = in->begin(); it != in->end(); ++it) {
        executeSchemaPathTest(*it, atomizedPath, &goalSet);
    };

    std::copy(goalSet.begin(), goalSet.end(), std::back_inserter(*output));
}


PathSchemaCheck::PathSchemaCheck(phop::IValueOperator* _in, const pe::AtomizedPath& apath)
    : ItemOperator(OPINFO_REF, _in), scnLookup(apath) { }

void PathSchemaCheck::reset()
{
    phop::ItemOperator::reset();
    scnLookup.compile();
    cache.clear();
}

void PathSchemaCheck::do_next()
{
    bool satisfy = false;
    tuple_cell tin;

    do {
        in->next();
        tin = in->get();

        if (tin.is_eos()) {
            seteos();
            return;
        };

        U_ASSERT(tin.is_node());

        schema_node_cptr snode = Node(tin.get_node()).checkp().getSchemaNode();
        SchemaCache::const_iterator it = cache.find(snode.ptr());

        if (it == cache.end()) {
            satisfy = scnLookup.exists(snode);
            cache.insert(SchemaCache::value_type(snode.ptr(), satisfy));
        } else {
            satisfy = it->second;
        };
    } while (!satisfy);

    push(tin);
}

PathSchemaResolve::PathSchemaResolve(phop::IValueOperator* _in, const pe::AtomizedPath& apath)
    : ItemOperator(OPINFO_REF, _in), scnLookup(apath)
{
    merge = new PathSchemaMerge();
}

PathSchemaResolve::~PathSchemaResolve()
{
    delete merge;
}


void PathSchemaResolve::do_next()
{
    tuple_cell tin;

    do {
        if (!merge->eos()) {
            push(tuple_cell::node(merge->next()));
            return;
        };

        in->next();
        tin = in->get();

        if (tin.is_eos()) {
            seteos();
            return;
        };

        U_ASSERT(tin.is_node());

        schema_node_cptr snode = Node(tin.get_node()).checkp().getSchemaNode();
        SchemaCache::const_iterator it = cache.find(snode.ptr());

        if (it == cache.end()) {
            SchemaNodePtrList nodePtrSet;
            scnLookup.execute(snode, &nodePtrSet);
            cache.insert(SchemaCache::value_type(snode.ptr(), toNodeSet(nodePtrSet)));

            merge->pushAll(tin.get_node(), snode, cache.at(snode.ptr()));
        } else {
            merge->pushAll(tin.get_node(), snode, it->second);
        };
    } while (!tin.is_eos());

    seteos();
}

void PathSchemaResolve::reset()
{
    phop::ItemOperator::reset();
    merge->clear();
    scnLookup.compile();
    cache.clear();
}

PathEvaluateTraverse::PathEvaluateTraverse(phop::IValueOperator* _in, const pe::AtomizedPath& apath)
    : ItemOperator(OPINFO_REF, _in), traverse(NULL)
{
    traverse = new PathTraverse(apath);
}

PathEvaluateTraverse::~PathEvaluateTraverse()
{
    delete traverse;
}

void PathEvaluateTraverse::do_next()
{
    tuple_cell tin;

    do {
        if (traverse->eos()) {
            in->next();
            tin = in->get();

            if (tin.is_eos()) {
                seteos();
                return;
            };

            U_ASSERT(tin.is_node());
            traverse->push(tin.get_node());
        };

        Node node = traverse->next();

        if (!node.isNull()) {
            push(tuple_cell::node(node.getPtr()));
            return;
        };
    } while (true);
}

void PathEvaluateTraverse::reset()
{
    phop::ItemOperator::reset();
}

XmlConstructor & PathSchemaCheck::__toXML(XmlConstructor & element) const
{
    element.addElementValue(PHOPQNAME("path"), scnLookup.atomizedPath.__toString());
    return element;
};

XmlConstructor & PathEvaluateTraverse::__toXML(XmlConstructor & element) const
{
    element.addElementValue(PHOPQNAME("path"), traverse->getPath().__toString());
    return element;
};

XmlConstructor & PathSchemaResolve::__toXML(XmlConstructor & element) const
{
    element.addElementValue(PHOPQNAME("path"), scnLookup.atomizedPath.__toString());
    return element;
};
