/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "XPathLookup.h"
#include "DataSources.h"
#include "SchemaTests.h"

#include "common/errdbg/exceptions.h"

#include <bits/stl_algo.h>
#include <typeinfo>
#include <stack>
#include <queue>

using namespace pe;


SchemaLookup& SchemaLookup::compile()
{
    atomizedPath = path.atomize();
    reversePath = atomizedPath.reverse();

    return *this;
}

#define FOR_EACH_NODE(list, var) for (std::vector< schema_node_cptr >::const_iterator var = (list).begin(); var != (list).end(); ++var)

typedef std::pair<const AtomizedPathVector::const_iterator &, schema_node_cptr> ExecutionStackItem;
typedef std::stack<ExecutionStackItem> ExecutionStack;

bool executePathEx(schema_node_cptr base, const AtomizedPath & path, std::set<schema_node_xptr>* output, bool _fast = false)
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
    } while (!toTraverse.empty());

    return !(output == NULL || output->empty());
}

void SchemaLookup::findSomething(const DataRoot& root, std::vector< schema_node_xptr >* output, int limit)
{
    AtomizedPath path = atomizedPath;

    if (atomizedPath.cost() > reversePath.cost()) {
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
            if (!executePathEx(*it, checkPath, NULL, true)) {
                it = nodeCandidates.erase(it);
            } else {
                ++it;
            }
        };
    }

    std::copy(nodeCandidates.begin(), nodeCandidates.end(), std::back_inserter(*output));
}

void SchemaLookup::execute(schema_node_cptr base, std::vector< schema_node_xptr >* output)
{
    std::set<schema_node_xptr> goalSet;
    executePathEx(base.ptr(), atomizedPath, &goalSet);
    std::copy(goalSet.begin(), goalSet.end(), std::back_inserter(*output));
}

SchemaLookup::SchemaLookup(const pe::Path& _path) : path(_path), atomizedPath()
{
    
}

SchemaLookup::~SchemaLookup()
{

}

typedef Node (*PathExecutionEvironment::NextNodeProc)(Node node);
typedef bool (*PathExecutionEvironment::TestNodeProc)(Node node);

struct NodeIterator {
    typedef bool (*NodeIterator::NextNodeProc)();

    Node node;
    NextNodeProc _next;
    AtomizedPath path;

    NodeIterator(Node _node, NextNodeProc __next, const AtomizedPath &_path)
    : node(_node), _next(__next), path(_path) {};

    Node get() const { return node; };

    inline bool next() {
        if (_next != NULL) {
            return this->*_next();
        } else {
            node = NULL; return false;
        }
    }

    bool nextParent();
};

struct PathExecutionEvironment {
    std::queue<NodeIterator> nodeset;
    Node eval(Node node, const AtomizedPath & path);
};

VPathLookup::VPathLookup(const pe::AtomizedPath& _path)
    : path(_path) { }

VPathLookup::~VPathLookup()
{

}

void VPathLookup::compile()
{
    
}

void VPathLookup::execute(const Node& node)
{
    
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
