#include "XPathExecution.h"
#include "XPathTypes.h"
#include "AtomizedPath.h"

#include <stack>

using namespace pe;

/**
 *  Evaluates node path, following path from \node to \goal
 */

/*
static
Node getRightBrother(Node node, Node commonAncestor) {
    Node n = getNextDescriptorOfSameSort(node.getPtr());
    
     Check if this node still is a descendant of the base one
    if (!n.isNull()) {
        int relation = nid_cmp_effective(n.getPtr(), commonAncestor.getPtr());

        if (relation != 2 && relation != 0) {
            return XNULL;
        }
    }
    
    return n;
};
*/
    
static
Node getRightBrother(Node node, const NidString & nstr) {
    Node n = getNextDescriptorOfSameSort(node.getPtr());

    /* Check if this node still is a descendant of the base one */
    if (!n.isNull()) {
        int relation = NidString(n.getPtr()).compare(nstr);

        if (relation != 2 && relation != 0) {
            return XNULL;
        }
    }

    return n;
};

static
xptr getFirstNodeByPath(Node node, const SchemaPath & path, const NidString & commonAncestorNid) {
    xptr nodeI = node.getPtr();

    for (std::vector<int>::const_iterator i = path.begin(); i != path.end(); ++i) {
        xptr nodeJ = getChildAt(nodeI, *i);

        /* If we found the first child node on the step of path traversing,
         * we should scan through all children of this type to find one, that have
         * requested grandchild for the next step */

        while (nodeJ == XNULL) {
            nodeI = getRightBrother(nodeI, commonAncestorNid).getPtr();

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

void NodeIterator::nextNodeCommonAncestor()
{
    node = getRightBrother(node, *baseNID);
}

void NodeIterator::nextNodeParent()
{
    node = XNULL;
}

void NodeIterator::nextNodeSibling()
{
    node = XNULL;
}

void PathStepIterator::nextSiblingSchema()
{
    node = getNextByTypeMask(node.checkp().getPtr(), mask);

    if (node.isNull()) {
        snode = XNULL;
    } else {
        snode = node.checkp().getSchemaNode();
    }
}


void PathTraverse::clear()
{
    mergelist.clear();
    _eos = true;
}

Node PathTraverse::next()
{
    while (!mergelist.empty()) {
        PathStepList::iterator pos = mergelist.begin();
        PathStepIterator step = *pos;
        AtomizedPath path = step.path;
        Node node = step.get().checkp();

        if (!pos->next()) {
            pos = mergelist.erase(pos);
        };

        if (path.empty()) {
            _eos = mergelist.empty();
            return node;
        };

        PathAtom * item = path.at(0);

        if (dynamic_cast<ParentAtom *>(item) != NULL) {
            ParentAtom * axisStep = dynamic_cast<ParentAtom *>(item);
            int selfCorrection = 0;

            if (axisStep->orSelf) {
                pos = mergelist.insert(pos, step.passStep());
                selfCorrection = 1;
            };

            Node parent = node.getActualParent();

            if (!parent.isNull()) {
                pos = mergelist.insert(pos + selfCorrection, PathStepIterator(NodeIterator(parent), path + 1, step.snode->parent));

                if (axisStep->closure) {
                    pos = mergelist.insert(pos + 1, PathStepIterator(NodeIterator(parent), path, step.snode->parent));
                }
            }
        } else if (dynamic_cast<ChildAtom *>(item) != NULL) {
            /* FIXME: This is not a fair Document Order! */

            ChildAtom * axisStep = dynamic_cast<ChildAtom *>(item);

            if (axisStep->orSelf) {
                pos = mergelist.insert(pos, step.passStep());
            };

            Node child = getFirstChildByTypeMask(node.getPtr(), axisStep->childMask);

            pos = mergelist.insert(pos, NextChildNode(child, path + 1, axisStep->childMask));

            if (axisStep->closure && step.snode->has_children()) {
                pos = mergelist.insert(pos, NextChildNode(child, path, axisStep->childMask));
            }
        } else if (dynamic_cast<SchemaTestAtom *>(item) != NULL) {
            SchemaTestAtom * test = dynamic_cast<SchemaTestAtom *>(item);

            if (test->test(step.snode)) {
                step.path = step.path + 1;
                pos = mergelist.insert(pos, step);
            };
        } else {
            U_ASSERT(false);
        }
    };

    _eos = mergelist.empty();
    return Node();
}

void PathSchemaMerge::clear()
{
    mergeheap.clear();
    _eos = true;
}

void PathSchemaMerge::pushAll(Node node, schema_node_cptr base, const SchemaNodeList& nodeList)
{
    SchemaPath path;
    path.reserve(64);
    counted_ptr<NidString> commonAncestorNid = new NidString(node.getPtr());

    for (SchemaNodeList::const_iterator it = nodeList.begin(); it != nodeList.end(); ++it) {
        path.clear();
        getNodePath(base, *it, path);

        mergeheap.push_back(NIDMergeHeap::value_type(
          NidString(node.getPtr()),
          NodeIterator(
              getFirstNodeByPath(node, path, *commonAncestorNid),
              &NodeIterator::nextNodeCommonAncestor,
              commonAncestorNid)));
    };

    std::make_heap(mergeheap.begin(), mergeheap.end(), NIDMergeHeapCompare());

    _eos = mergeheap.empty();
}

PathTraverse::PathTraverse(const pe::AtomizedPath& _path)
  : _eos(true)
{
    for (pe::AtomizedPathVector::const_iterator it = _path.begin(); it != _path.end(); ++it) {
        PathAtom * newAtom = NULL;

        if (dynamic_cast<AxisPathAtom *>(*it) == NULL) {
            newAtom = (*it)->clone();
        } else {
            AxisPathAtom * atom = dynamic_cast<AxisPathAtom *>(*it);

            switch (atom->axis) {
                case axis_child:
                    newAtom = new ChildAtom(ti_dmchildren, atom->closure, atom->orSelf);
                    break;
                case axis_child_or_attribute:
                    newAtom = new ChildAtom(ti_all_valid, atom->closure, atom->orSelf);
                    break;
                case axis_attribute:
                    newAtom = new ChildAtom(ti_first_children, atom->closure, atom->orSelf);
                    break;
                case axis_parent:
                    newAtom = new ParentAtom(atom->closure, atom->orSelf);
                    break;
                default:
                    U_ASSERT(false);
                    newAtom = atom->clone();
                    break;
            };
        };

        path.push_back(newAtom);
    };

    path.setImmutable();
}



typedef std::pair<const AtomizedPathVector::const_iterator &, schema_node_cptr> ExecutionStackItem;
typedef std::stack<ExecutionStackItem> ExecutionStack;

bool executeSchemaPathTest(schema_node_cptr base, const AtomizedPath & path, SchemaNodePtrSet * output, bool _fast)
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
        } else if (dynamic_cast<SchemaTestAtom *>(item) != NULL) {
            SchemaTestAtom * test =  dynamic_cast<SchemaTestAtom *>(item);
            
            if (test->test(base)) {
                toTraverse.push(ExecutionStackItem(step.first + 1, base));
            };
        } else {
            U_ASSERT(false);
        };
    } while (!toTraverse.empty());
    
    return !(output == NULL || output->empty());
}
