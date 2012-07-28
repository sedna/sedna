#include "GraphRewriter.h"
#include "DataGraphCollection.h"
#include "Predicates.h"

#include <algorithm>

using namespace opt;
using namespace pe;

void opt::DataGraphRewriter::mergeNodes(const DataNodeIndex & master, const DataNodeIndex& alias)
{
    U_ASSERT(alias.p->type == DataNode::dnAlias || alias.p->type == DataNode::dnExternal);

    PlanDescIterator it(alias.predicates);
    int i;

    while (-1 != (i = it.next())) {
        Predicate * p = graph.dg->predicates[i];

        for (DataNodeList::iterator d = p->dataNodeList.begin(); d != p->dataNodeList.end(); ++d) {
            if (*d == alias.p) {
                *d = master.p;
            }
        }
    }

    graph.dg->dataNodes[alias.p->index] = NULL;

    if (alias.output) {
        graph.dg->outputNodes |= master.p->indexBit;
        graph.dg->outputNodes &= ~alias.p->indexBit;
    };
    
    alias.p->replacedWith = master.p;
    alias.p->type = DataNode::dnReplaced;

    if (alias.p->varTupleId != opt::invalidTupleId) {
        if (master.p->varTupleId != opt::invalidTupleId) {
            graph.dg->owner->mergeVariables(alias.p->varTupleId, master.p->varTupleId);
        } else {
            master.p->varTupleId = alias.p->varTupleId;
            graph.dg->owner->addVariable(master.p);
        };

        graph.dg->owner->removeVariable(alias.p);
    };
}

void DataGraphRewriter::deleteRedundantConsts()
{
    // TODO: delete consts;
/*
  DataNode ** dataNodes = graph.dg->dataNodes;

    FOR_ALL_GRAPH_ELEMENTS(dataNodes, i) {
        DataNode * dn = dataNodes[i];

        if (dn->type == DataNode::dnConst) {
            DataNode * producerPtr = graph.dg->owner->getVariable(dn->varTupleId).producer;

            if (producerPtr != NULL && dataNodes[producerPtr->index] ==  producerPtr) {
                mergeNodes(
                    index.nodes[producerPtr->index],
                    index.nodes[dn->index]);
            };
        }
    };

    graph.update();
    index.update();
*/    
}


void opt::DataGraphRewriter::selfReferenceResolution()
{
    DataNode ** dataNodes = graph.dg->dataNodes;

    FOR_ALL_GRAPH_ELEMENTS(dataNodes, i) {
        DataNode * dn = dataNodes[i];

        if (dn->type == DataNode::dnExternal) {
            DataNode * producerPtr = graph.dg->owner->getVariable(dn->varTupleId).producer;

            if (producerPtr != NULL && dataNodes[producerPtr->index] == producerPtr) {
                mergeNodes(
                    graph.nodeIndex[producerPtr->index],
                    graph.nodeIndex[dn->index]);
            };
        }
    };

    graph.update();
}

void DataGraphRewriter::aliasResolution()
{
    DataNode ** dataNodes = graph.dg->dataNodes;

/* Replace aliases
 */
    FOR_ALL_GRAPH_ELEMENTS(dataNodes, i) {
        if (dataNodes[i]->type == DataNode::dnAlias) {
            mergeNodes(
              graph.nodeIndex[dataNodes[i]->aliasFor->index],
              graph.nodeIndex[dataNodes[i]->index]);
        }
    };

    graph.update();
}

/*
 *  Replaces all value comparisons, that have structural cmp op with structural comparisons.
 */

void DataGraphRewriter::structuralComparison()
{
    PredicateIndex * predicates = graph.predicateIndex;

    for (PredicateList::iterator it = graph.predicates.begin(); it != graph.predicates.end(); ++it) {
        ValuePredicate * pred = dynamic_cast<ValuePredicate*>(*it);

        if (pred != NULL && (pred->cmp.op == Comparison::do_after || pred->cmp.op == Comparison::do_before)) {
            pe::Step step;

            if (pred->cmp.op == Comparison::do_after) {
                step = pe::Step(pe::axis_following, nt_any_kind, xsd::QNameAny);
            } else {
                step = pe::Step(pe::axis_preceding, nt_any_kind, xsd::QNameAny);
            };

            StructuralPredicate * rep = new StructuralPredicate(pred->left(), pred->right(), step);
            *it = rep;
        };
    };

    graph.rebuild();
}

void DataGraphRewriter::doPathExpansion()
{
    DataGraph * dg = graph.dg;
  
    FOR_ALL_GRAPH_ELEMENTS(dg->dataNodes, i) {
        DataNode * dn = dg->dataNodes[i];

        if (dg->dataNodes[i]->type != DataNode::dnFreeNode
              || (singlePlanDesc(i) & dg->outputNodes) > 0) {
            continue;
        };

        PlanDescIterator it(graph.nodeIndex[dn->index].predicates);

        int leftIdx = it.next();
        int rightIdx = it.next();

        if (leftIdx == -1 || rightIdx == -1 || it.next() != -1) {
            continue;
        };

        StructuralPredicate * left = dynamic_cast<StructuralPredicate *> (dg->predicates[leftIdx]);
        StructuralPredicate * right = dynamic_cast<StructuralPredicate *> (dg->predicates[rightIdx]);

        /* If datanode is connected with more then one predicate, it is not optimizable
         */

        if (left == NULL || right == NULL) {
            continue;
        };

        if (left->left() == dn) {
            U_ASSERT(right->right() == dn);
            std::swap(left, right);
        };

        U_ASSERT(right->left() == dn && left->right() == dn);

        pe::Path path = left->path + right->path;

        if (!path.inversable()) {
            continue;
        };

        left->path = path;

        dg->predicates[right->index] = NULL;
        dg->owner->removeVariable(dg->dataNodes[i]);
        dg->dataNodes[i]->type = opt::DataNode::dnReplaced;
        dg->dataNodes[i]->replacedWith = NULL;
        dg->dataNodes[i] = NULL;

        /* This step is absolutely important to iterate through graph further
         */
        left->dataNodeList[1] = right->right();

        DataNodeIndex * rightOfRight = &(graph.nodeIndex[right->right()->index]);

        rightOfRight->predicates &= ~right->indexBit;
        rightOfRight->predicates |= left->indexBit;
    };

    graph.update();
}

/* Static optimization phase */

void DataGraphRewriter::expandAbsolutePath()
{
    DataNodeList list1, list2;
    DataNodeList *frontList = &list1, *backList = &list2;
    DataGraph * dg = graph.dg;

    FOR_ALL_GRAPH_ELEMENTS(dg->dataNodes, i) {
        // TODO : External propagade external 
        if (dg->dataNodes[i]->type == DataNode::dnDatabase) {
            frontList->push_back(dg->dataNodes[i]);
        };
    };

    while (!frontList->empty()) {
        backList->clear();

        for (DataNodeList::iterator d = frontList->begin(); d != frontList->end(); ++d) {
            DataNode * dn = *d;
            PlanDescIterator it(graph.nodeIndex[dn->index].predicates);
            int i;

            while (-1 != (i = it.next())) {
                StructuralPredicate * pred =
                  dynamic_cast<StructuralPredicate*>(graph.dg->predicates[i]);

                // TODO : not every path can be concatinated, some should be broken
                if (NULL != pred && pred->left() == dn &&
                  pred->right()->type == DataNode::dnFreeNode)
                {
                    pe::Path path = pred->left()->path + pred->path;

                    if (!path.inversable()) {
                        continue;
                    }

                    pred->right()->type = DataNode::dnDatabase;
                    pred->right()->root = pred->left()->root;
                    pred->right()->path = path;

                    backList->push_back(pred->right());
                }
            }
        };

        DataNodeList* swp = frontList;
        frontList = backList;
        backList = swp;
    }

    // NOTE: No need to update anything for we 
}

/*
void DataGraph::precompile()
{
    DataNodeList list1, list2;
    DataNodeList *frontList = &list1, *backList = &list2;

    typedef std::set< std::pair<DataNode *, DataNode *> > RemovalList;
    RemovalList removalCandidates;
    
    FOR_ALL_GRAPH_ELEMENTS(dataNodes, i) {
        if (dataNodes[i]->type == DataNode::dnDatabase) {
            frontList->push_back(dataNodes[i]);
        };
    };

    while (!frontList->empty()) {
        backList->clear();

        for (DataNodeList::iterator d = frontList->begin(); d != frontList->end(); ++d) {
            DataNode * dn = *d;
            PlanDescIterator it((*d)->predicates);
            int i;

            while (-1 != (i = it.next())) {
                StructuralPredicate * pred = dynamic_cast<StructuralPredicate*>(predicates[i]);

                // TODO : not every path can be concatinated, some should be broken
                if (NULL != pred && pred->left() == dn &&
                  pred->right()->type == DataNode::dnFreeNode)
                {
                    pe::Path path = pred->left()->path + pred->path;

                    if (!path.inversable()) {
                        continue;
                    }

                    pred->right()->type = DataNode::dnDatabase;
                    pred->right()->root = pred->left()->root;
                    pred->right()->path = path;
                    pred->right()->producedFrom = pred;

                    backList->push_back(pred->right());
                }
            }
        };

        DataNodeList* swp = frontList;
        frontList = backList;
        backList = swp;
    }

    updateIndex();
}
*/

