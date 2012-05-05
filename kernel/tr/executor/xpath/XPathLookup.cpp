/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/errdbg/exceptions.h"

#include "DataSources.h"

#include "XPathLookup.h"
#include "XPathExecution.h"

#include "tr/executor/algorithms/SequenceModel.h"

#include <bits/stl_algo.h>
#include <typeinfo>
#include <stack>
#include <queue>

using namespace pe;


SchemaLookup& SchemaLookup::compile()
{
    if (atomizedPath.empty()) {
        atomizedPath = path.atomize();
    }

    reversePath = atomizedPath.reverse();

    return *this;
}

#define FOR_EACH_NODE(list, var) for (std::vector< schema_node_cptr >::const_iterator var = (list).begin(); var != (list).end(); ++var)

typedef std::pair<const AtomizedPathVector::const_iterator &, schema_node_cptr> ExecutionStackItem;
typedef std::stack<ExecutionStackItem> ExecutionStack;

bool executeSchemaPathTest(schema_node_cptr base, const AtomizedPath & path, SchemaNodePtrSet * output, bool _fast = false)
{
    ExecutionStack toTraverse;
    toTraverse.push(ExecutionStackItem(path.begin(), base));

    do {
        const ExecutionStackItem & step = toTraverse.top();
        PathAtom * item = *step.first;
        base = step.second;
        toTraverse.pop();

        if (step.first == path.end()) {
            if (output == NULL) {
                /* In this case function just return true (it found satisfied node) */
                return true;
            };

            output->insert(step.second.ptr());
        } else if (dynamic_cast<AxisPathAtom *>(item) != NULL) {
            AxisPathAtom * axisStep = dynamic_cast<AxisPathAtom *>(item);
            t_item childMask = (t_item) 0;

            if (axisStep->orSelf) {
                toTraverse.push(ExecutionStackItem(step.first + 1, base));
            };

            switch (axisStep->axis) {
              case axis_parent:
                if (base->parent != XNULL) {
                    toTraverse.push(ExecutionStackItem(step.first + 1, base->parent));

                    if (axisStep->closure) {
                        toTraverse.push(ExecutionStackItem(step.first, base->parent));
                    }
                }

                continue;
              case axis_child_or_attribute:
                childMask = (t_item) (ti_dmchildren | attribute);
                break;
              case axis_child:
                childMask = ti_dmchildren;
                break;
              case axis_attribute:
                childMask = attribute;
                break;
              default :
                break;
            };

            CAT_FOR_EACH(sc_ref, i, (base->children)) {
                if ((i->object.type & childMask) != 0) {
                    toTraverse.push(ExecutionStackItem(step.first + 1, i->object.snode));

                    if (axisStep->closure && ((i->object.type & ti_with_children) > 0)) {
                        /* In fast mode we do not traverse child closures (descendants) */

                        if (_fast) { return true; }
                        toTraverse.push(ExecutionStackItem(step.first, i->object.snode));
                    }
                };
            };
        } else if (dynamic_cast<NameTestAtom *>(item) != NULL) {
            NameTestAtom * nameTest =  dynamic_cast<NameTestAtom *>(item);

            if (schemaNodeTest(base, nameTest->nt, SchemaTestData(nameTest->itemType, nameTest->qname.getUri(), nameTest->qname.getLocalName()))) {
                toTraverse.push(ExecutionStackItem(step.first + 1, base));
            };
        } else if (dynamic_cast<TypeTestAtom *>(item) != NULL) {
            TypeTestAtom * typeTest =  dynamic_cast<TypeTestAtom *>(item);

            if ((base->type & typeTest->itemType) > 0) {
                toTraverse.push(ExecutionStackItem(step.first + 1, base));
            };
        } else {
            U_ASSERT(false);
        };
    } while (!toTraverse.empty());

    return !(output == NULL || output->empty());
}

void SchemaLookup::findSomething(const DataRoot& root, SchemaNodePtrList* output, int limit)
{
    AtomizedPath path = atomizedPath;

    if (!reversePath.empty() && atomizedPath.cost() > reversePath.cost()) {
        path = reversePath;
    };
    
    AtomizedPathVector::const_iterator it;
    NameTestAtom * firstNodeTest;
    doc_schema_node_cptr rootNode = root.getSchemaNode().ptr();

    int i = 0;

    for (it = path.begin(); it != path.end(); ++it) {
        firstNodeTest = dynamic_cast<NameTestAtom *>(*it);

        if (firstNodeTest != NULL) {
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

class PathSchemaCheck : public phop::ItemOperator {
typedef std::map<schema_node_xptr, bool> SchemaCache;
    SchemaLookup scnLookup;
    SchemaCache cache;
protected:
    virtual void do_next();
public:
    PathSchemaCheck(IValueOperator * _in, const AtomizedPath& apath)
      : ItemOperator(_in), scnLookup(apath) {};

    virtual void reset();
};

class PathEvaluateTraverse : public phop::ItemOperator {
    PathTraverse * traverse;
protected:
    virtual void do_next();
public:
    virtual void reset();
};

class PathSchemaResolve : public phop::ItemOperator {
typedef std::map<schema_node_xptr, SchemaNodeList> SchemaCache;
    SchemaLookup scnLookup;
    SchemaCache cache;
    PathSchemaMerge * env;
protected:
    virtual void do_next();
public:
    PathSchemaResolve(IValueOperator * _in, const AtomizedPath& apath)
      : ItemOperator(_in), scnLookup(apath) {};

    virtual void reset();
};

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

void PathSchemaResolve::do_next()
{
    tuple_cell tin;

    do {
        if (!env->eos()) {
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

        traverse->next();

    } while (true);
}

void PathEvaluateTraverse::reset()
{
    phop::ItemOperator::reset();
}




#ifdef HUIHUIHUI


class AncestorSequence : public ExecutablePathAtom {
    std::string sequence;
    // P - parent, A - parent closure, S parent closure or self, D - document test, N - name test
public:
};


class ExecutablePathAtom : public PathAtom {
public:
    virtual void eval(PathExecutionEvironment * env);
};

template <typename SchemaTestOperator>
class ExecutableNodeTest : public ExecutablePathAtom {
};

class AncestorSequenceSchemaCheck : public AncestorSequence {
    std::vector<NameTestAtom> testList;
};

class AncestorSequenceEval : public AncestorSequence {
};

class ChildBySchemaEval : public ExecutablePathAtom {
};

class DescendantBySchemaEval : public ExecutablePathAtom {
};

class DescendantTraverse : public ExecutablePathAtom {
};


void PathLookup::compile()
{
    for (AtomizedPathVector::const_iterator it = path.begin(); it != path.end(); ++it) {
        PathAtom * item = *it;

        if (dynamic_cast<AxisPathAtom *>(item) != NULL) {
            AxisPathAtom * axisStep = dynamic_cast<AxisPathAtom *>(item);
            t_item childMask = (t_item) 0;

            if (axisStep->orSelf) {
                nodeset.push(NodeIterator(node, NULL, path + 1));
            };

            switch (axisStep->axis) {
                case axis_parent:
            if ()
            }
        }
    };
}



AncestorAxisLookup::AncestorAxisLookup(const pe::AtomizedPath& _path)
  : PathLookup(_path)
{

}

void AncestorAxisLookup::execute(const Node& node)
{
    env->nodeset.push();
  
    NodeIterator result;

    const AtomizedPath & path = env->nodeset.front().path;
}

Node PathExecutionEvironment::eval(Node node) {
    NodeIterator result;
    U_ASSERT(it != path.end());

    const AtomizedPath & path = tstack.top().path;

    if (path.empty()) {
        return node;
    };

    PathAtom * item = path.at(0);

    if (dynamic_cast<AxisPathAtom *>(item) != NULL) {
        AxisPathAtom * axisStep = dynamic_cast<AxisPathAtom *>(item);
        t_item childMask = (t_item) 0;

        if (axisStep->orSelf) {
            nodeset.push(NodeIterator(node, NULL, path + 1));
        };

        switch (axisStep->axis) {
            case axis_parent:
                if (node.checkp().getParentIndirection() != XNULL) {
                    nodeset.push(NodeIterator(node, NULL, path + 1));

                    if (axisStep->closure) {
                        nodeset.push(NodeIterator(node, NULL, path));
                    }
                }

                continue;
            case axis_child_or_attribute:
                childMask = (t_item) (ti_dmchildren | attribute);
                break;
            case axis_child:
                childMask = ti_dmchildren;
                break;
            case axis_attribute:
                childMask = attribute;
                break;
            default :
                break;
        };

        CAT_FOR_EACH(sc_ref, i, (base->children)) {
            if ((i->object.type & childMask) != 0) {
                toTraverse.push(ExecutionStackItem(step.first + 1, i->object.snode));

                if (axisStep->closure && ((i->object.type & ti_with_children) > 0)) {
                    /* In fast mode we do not traverse child closures (descendants) */

                    if (_fast) { return true; }
                    toTraverse.push(ExecutionStackItem(step.first, i->object.snode));
                }
            };
        };
    } else if (dynamic_cast<NameTestAtom *>(item) != NULL) {
        NameTestAtom * nameTest =  dynamic_cast<NameTestAtom *>(item);
        SchemaTestData data(nameTest->itemType, nameTest->qname.getUri(), nameTest->qname.getLocalName());

        switch (nameTest->nt) {
            case pe::nt_wildcard_name: ;
            if (SchemaTestOperatorLocalType::test(base, &data)) {
                toTraverse.push(ExecutionStackItem(step.first + 1, base));
            };
            break;
            case pe::nt_wildcard_prefix: ;
            if (SchemaTestOperatorUriType::test(base, &data)) {
                toTraverse.push(ExecutionStackItem(step.first + 1, base));
            };
            break;
            case pe::nt_qname: ;
            if (SchemaTestOperatorQNameType::test(base, &data)) {
                toTraverse.push(ExecutionStackItem(step.first + 1, base));
            };
            break;
            default:
                U_ASSERT(false);
                break;
        };
    } else if (dynamic_cast<TypeTestAtom *>(item) != NULL) {
        TypeTestAtom * typeTest =  dynamic_cast<TypeTestAtom *>(item);
        if ((base->type & typeTest->itemType) > 0) {
            toTraverse.push(ExecutionStackItem(step.first + 1, base));
        };
    } else {
        U_ASSERT(false);
    };
};


struct PathStepData {
};

struct PathStep {
    virtual next(PathStepData * data) = 0;
    virtual get(PathStepData * data) = 0;
};

struct AncestorStepData : public PathStep {
    virtual next(PathStepData* data);
    virtual get(PathStepData* data);
};

struct DescendantStepData : public PathStep {
    virtual next(PathStepData* data);
    virtual get(PathStepData* data);
};




void * pathEvalModel(const AtomizedPath & apath) {
    if (apath.empty()) {
        return NULL;
    };

    PathAtom

     (apath.begin()) {
        PathAtom * item = *step.first;
    };
};


#define CALL(OBJECT, METHOD) ((OBJECT)->*(METHOD));

PathStackElement evaluatePath(const PathStackElement & in) {
    PathStackElement result;
    
//    in.node;
    return result;
};

Node VPathLookup::next()
{
    Node node;

    while (!env->stack.empty() && env->stack.top().path.empty()) {
        if (env->stack.empty()) {
            return Node();
        }

        Node node = CALL(env, env->stack.top().next)(env->stack.top().node);

        if (env->stack.top().path.empty()) {
            return node;
        };

        PathStackElement element;

        element.node = node;
        element.next = 
    };

    return node;
}

#endif 