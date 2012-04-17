/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "XPathLookup.h"
#include "XPathExecution.h"

#include "common/errdbg/exceptions.h"

using namespace pe;

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
*/    
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


