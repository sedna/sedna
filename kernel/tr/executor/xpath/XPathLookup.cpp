/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/errdbg/exceptions.h"

#include "DataSources.h"
#include "XPathLookup.h"
#include "XPathExecution.h"

#include <stack>
#include <queue>

using namespace pe;


SchemaLookup& SchemaLookup::compile()
{
    if (atomizedPath.empty()) {
        atomizedPath = AtomizedPath(path);
    }

    reversePath = atomizedPath.reverse();

    return *this;
}

void SchemaLookup::findSomething(const DataRoot& root, SchemaNodePtrList* output, int limit)
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
        in->next(_context);
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

PathSchemaCheck::~PathSchemaCheck()
{

}

PathSchemaResolve::PathSchemaResolve(phop::IValueOperator* _in, const pe::AtomizedPath& apath)
    : ItemOperator(_in), scnLookup(apath)
{

}


void PathSchemaResolve::do_next()
{
    tuple_cell tin;

    do {
        if (!merge->eos()) {
            push(tuple_cell::node(env->next()));
            return;
        };

        in->next(_context);
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

            env->pushAll(tin.get_node(), snode, cache.at(snode.ptr()));
        } else {
            env->pushAll(tin.get_node(), snode, it->second);
        };
    } while (!tin.is_eos());

    seteos();
}

void PathSchemaResolve::reset()
{
    phop::ItemOperator::reset();
    env->clear();
    scnLookup.compile();
    cache.clear();
}

void PathEvaluateTraverse::do_next()
{
    tuple_cell tin;

    do {
        if (traverse->eos()) {
            in->next(_context);
            tin = in->get();

            if (tin.is_eos()) {
                seteos();
                return;
            };

            traverse->push(tin.get_node());
        };

        Node node = traverse->next();

        if (!node.isNull()) {
        };
    } while (true);
}

void PathEvaluateTraverse::reset()
{
    phop::ItemOperator::reset();
}

