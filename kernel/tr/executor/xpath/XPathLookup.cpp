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

    return !output->empty();
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

    std::copy(nodeCandidates.begin(), nodeCandidates.end(), output->end());
}

void SchemaLookup::execute(schema_node_cptr base, std::vector< schema_node_xptr >* output)
{
    std::set<schema_node_xptr> goalSet;
    executePathEx(base.ptr(), atomizedPath, &goalSet);
    std::copy(goalSet.begin(), goalSet.end(), output->end());
}

SchemaLookup::SchemaLookup(const pe::Path& _path) : atomizedPath(), path(_path)
{
    
}

SchemaLookup::~SchemaLookup()
{

}


/*
class DumbIterator : public IPathIterator {
  public:
    virtual Node next();
};

class ReusableIterator : public IPathIterator {
  public:
    virtual void init(Node node) = 0;
};

class StateIterator : public ReusableIterator {
  public:
    TestNodeProc test;
    Node currentNode;

    void init(Node node, Node currentNode);
};

class StepByStepLookup : public PathLookup {
  private:
//    AxisHints * evaluationInfo;
    pe::Step step;
    StateIterator * currentIterator;
    NextNodeProc resolve;
  public:
    StepByStepLookup(const Path & path);
    virtual ~StepByStepLookup();

    virtual void compile();
    virtual NodeIterator execute(const Node& node);
};

class PathIndexLookup : public PathLookup {
  private:
    LookupInfo * lookupInfo;
    ReusableIterator * currentIterator;
  public:
    PathIndexLookup(const Path & _path);
    virtual ~PathIndexLookup();

    void compile();

    virtual NodeIterator execute(const Node& node);
};

class ParentIterator : public StateIterator {
  public:
    NodeHeapStorage nodeHeap;
};

class MergeIterator : public StateIterator {
  public:
    NodeHeapStorage nodeHeap;
};

class SiblingMergeIterator : public MergeIterator {
  public:
    virtual Node next();
};

class DescendantMergeIterator : public MergeIterator {
  public:
    Node baseNode;

    virtual Node next();
};

class TraverseAll : public StateIterator {
  public:
    Node baseNode;

    virtual Node next();
};

PathLookup::PathLookup(const pe::Path& _path)
  : path(_path) { }

StepByStepLookup::StepByStepLookup(const pe::Path& path)
  : PathLookup(path), currentIterator(NULL)
{
    if (path.getBody()->size() != 1) {
        throw USER_EXCEPTION_FNERROR("Optimizer error", "StepByStepLookup not possible with given path");
    };

    step = path.getBody()->at(0);
}

StepByStepLookup::~StepByStepLookup()
{
    delete currentIterator;
}

void StepByStepLookup::compile()
{
/*
    switch (step.getAxis()) {
      case axis_child : {
        resolve = resolveAxis_ChildAny();
      } break;
      case axis_descendant : {
        resolve = resolve
      } break;
      case axis_descendant_or_self : {
      } break;
      
      case axis_ancestor : {
        resolve = nextNode_Parent();
        currentIterator = new ParentIterator();
      } break;

      case axis_parent : {
        resolve = nextNode_Parent();
        currentIterator = new DumbIterator();
      } break;
    }

    currentIterator =
}

NodeIterator StepByStepLookup::execute(const Node& node)
{
    return currentIterator;
//    currentIterator->init(node, resolve(node, NULL));
}




// StepByStepLookup




/*
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
*/


