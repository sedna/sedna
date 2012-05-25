#include "tr/opt/algebra/DataGraph.h"
#include "tr/opt/algebra/Predicates.h"
#include "tr/opt/phm/PhysicalModel.h"

#include "tr/models/XmlConstructor.h"

#include <bits/algorithmfwd.h>

using namespace opt;

DataGraph::DataGraph(DataGraphMaster* _owner) : lastIndex(1), owner(_owner)
{
    memset(predicates, 0, sizeof(predicates));
    memset(dataNodes, 0, sizeof(dataNodes));
}

Comparison::Comparison(const scheme_list* lst) : op(invalid)
{
    if (lst->size() < 1 || lst->at(0).type != SCM_SYMBOL) {
        throw USER_EXCEPTION2(SE1004, "Invalid comparison LR string");
    }

    const char * _type = lst->at(0).internal.symb;

    if (strcmp(_type, "gc::eq") == 0) {
        op = g_eq;
    } else if (strcmp(_type, "do::before") == 0) {
        op = do_before;
    } else {
        throw USER_EXCEPTION2(SE1004, "Invalid comparison LR string");
    };
}

void * SPredicate::compile(PhysicalModel* model)
{
    return model->compile(this);
}

void * VPredicate::compile(PhysicalModel* model)
{
    return model->compile(this);
}

void BinaryPredicate::setVertices(DataGraph* dg, TupleId _left, TupleId _right)
{
    dataNodeList.push_back(dg->owner->getVarNode(_left));
    dataNodeList.push_back(dg->owner->getVarNode(_right));
}

PlanDesc DataGraph::getNeighbours(PlanDesc x)
{
    PlanDescIterator iter(x);
    PlanDesc result = 0;

    int i;
    while (-1 != (i = iter.next())) {
        result |= predicates[i]->neighbours;
    }

    return (result & ~x);
}

void DataGraph::updateIndex()
{
    int nodeIndex = 0;
    allPredicates = 0;

    outputNodes.clear();

    FOR_ALL_GRAPH_ELEMENTS(dataNodes, i) {
        DataNode * dd = dataNodes[i];

        dd->predicates = 0;
        dd->absoluteIndex = nodeIndex++;

        if (dd->output) {
            outputNodes.push_back(dd);
        };
    };

    nodeCount = nodeIndex;

    FOR_ALL_GRAPH_ELEMENTS(predicates, i) {
        Predicate * pp = predicates[i];
        
        pp->dataNodeMask = 0;
        allPredicates |= pp->indexBit;

        for (DataNodeList::iterator d = pp->dataNodeList.begin(); d != pp->dataNodeList.end(); ++d) {
            pp->dataNodeMask |= (*d)->indexBit;
            (*d)->predicates |= pp->indexBit;
        }
    }

    FOR_ALL_GRAPH_ELEMENTS(predicates, i) {
        Predicate * pp = predicates[i];
        pp->neighbours = 0;

        for (DataNodeList::iterator d = pp->dataNodeList.begin(); d != pp->dataNodeList.end(); ++d) {
            pp->neighbours |= (*d)->predicates;
        }
    }
}

bool Predicate::replacable(DataNode* n1, DataNode* n2)
{
    return false;
}

Predicate* Predicate::replace(DataNode* n1, DataNode* n2)
{
    return this;
}

using namespace pe;

bool DataGraph::replaceNode(DataNode* n1, DataNode* n2)
{
    typedef std::vector<Predicate*> TempPredicateSet;

    TempPredicateSet /* sourceOnlySet, intersection, */ allSet;

    if (n1->output) {
        return false;
    };

    int i;

    PlanDescIterator it(n1->predicates);
    while (-1 != (i = it.next())) {
        U_ASSERT(this->predicates[i] != NULL);

        Predicate * p = this->predicates[i];

/*
        if ((p->dataNodes & n1->indexBit) > 0) {
            sourceOnlySet.push_back(p);
        } else {
            intersection.push_back(p);
        };
*/

        allSet.push_back(p);
    }

    for (TempPredicateSet::const_iterator i = allSet.begin(); i != allSet.end(); ++i) {
        if (!(*i)->replacable(n1, n2)) {
            return false;
        }
    };

    for (TempPredicateSet::const_iterator i = allSet.begin(); i != allSet.end(); ++i) {
        Predicate * np = (*i)->replace(n1, n2);

        if (np != *i) {
//            delete *i;
            this->predicates[(*i)->index] = np;
        };
    }

    /* By this time there should be no predicates with d1 left */
//    delete n1;
    this->dataNodes[n1->index] = NULL;

    updateIndex();

    return true;
}

bool SPredicate::replacable(DataNode* n1, DataNode* n2)
{
    return this->disposable && path.forall(pe::StepPredicate(pe::CDPAAxisTest));
}

Predicate * SPredicate::replace(DataNode* n1, DataNode* n2)
{
    if (replacable(n1, n2)) {
        return NULL;
    } else {
        return this;
    }
}

/* Static optimization phase */

void DataGraph::precompile()
{
    DataNodeList list1, list2;
    DataNodeList *frontList = &list1, *backList = &list2;

    typedef std::set< std::pair<DataNode *, DataNode *> > RemovalList;
    RemovalList removalCandidates;

    updateIndex();

    // TODO : replace same nodes;

// Replace document order comparisons with path expressions
    
    FOR_ALL_GRAPH_ELEMENTS(predicates, i) {
        VPredicate * pred = dynamic_cast<VPredicate*>(predicates[i]);

        if (pred != NULL && (pred->cmp.op == Comparison::do_after || pred->cmp.op == Comparison::do_before)) {
            pe::Step step;

            if (pred->cmp.op == Comparison::do_after) {
                step = pe::Step(pe::axis_following, nt_any_kind, xsd::QNameAny);
            } else {
                step = pe::Step(pe::axis_preceding, nt_any_kind, xsd::QNameAny);
            };

            SPredicate * rep = new SPredicate();

            rep->path = pe::Path(step);
            rep->outer = false;

            owner->replacePredicate(this, pred, rep);
        };
    }

// Find all root vertices

    FOR_ALL_GRAPH_ELEMENTS(dataNodes, i) {
        if (dataNodes[i]->type == DataNode::dnDatabase) {
            frontList->push_back(dataNodes[i]);
        };
    };

// Expand all path expressions from root vertices

    while (!frontList->empty()) {
        backList->clear();

        for (DataNodeList::iterator d = frontList->begin(); d != frontList->end(); ++d) {
            DataNode * dn = *d;
            PlanDescIterator it((*d)->predicates);
            int i;

            while (-1 != (i = it.next())) {
                SPredicate * pred = dynamic_cast<SPredicate*>(predicates[i]);

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

                    pred->disposable = true;

                    backList->push_back(pred->right());

                    if (!dn->output) {
                        removalCandidates.insert(RemovalList::key_type(dn, pred->right()));
                    }
                }
            }
        };

        DataNodeList* swp = frontList;
        frontList = backList;
        backList = swp;
    }

    updateIndex();
// Optimization: try to delete delete all redundant nodes

    for (RemovalList::const_iterator p = removalCandidates.begin(); p != removalCandidates.end(); ++p) {
        replaceNode(p->first, p->second);
    };
}












/* Serialization */



std::string DataGraph::toLRString() const
{
    std::stringstream stream;
    stream << "(datagraph ";

    stream << "(";

    FOR_ALL_GRAPH_ELEMENTS(dataNodes, i) {
        stream << dataNodes[i]->toLRString();
    };

    stream << ")";

    stream << "(";

    FOR_ALL_GRAPH_ELEMENTS(predicates, i) {
        stream << predicates[i]->toLRString();
    };
    stream << ")";

    stream << ")";
    return stream.str();
}

XmlConstructor & DataGraph::toXML(XmlConstructor & producer) const
{
    producer.openElement(CDGQNAME("datagraph"));

    FOR_ALL_GRAPH_ELEMENTS(dataNodes, i) {
        dataNodes[i]->toXML(producer);
    };

    FOR_ALL_GRAPH_ELEMENTS(predicates, i) {
        predicates[i]->toXML(producer);
    };

    producer.closeElement();

    return producer;
}

std::string DataNode::toLRString() const
{
    std::stringstream stream;

    stream << "(" << getName();

    switch(type) {
        case dnDatabase :
            stream << " root ";
            stream << root.toLRString();
            stream << path.toLRString();
            break;
        case dnFreeNode :
            stream << " free ";
            break;
        case dnConst :
            stream << " const ";
            break;
        case dnExternal :
            stream << " ext ";
            break;
        case dnAlias :
            stream << " alias ";
            break;
    };

    if (output) {
        stream << " output ";
    };

    stream << ")";

    return stream.str();
}

XmlConstructor & DataNode::toXML(XmlConstructor & element) const
{
    const char * nodetype = NULL;

    switch(type) {
        case dnDatabase : nodetype = "root"; break;
        case dnFreeNode : nodetype = "free"; break;
        case dnConst : nodetype = "const"; break;
        case dnExternal : nodetype = "ext"; break;
        case dnAlias : nodetype = "alias"; break;
    };

    element.openElement(CDGQNAME("node"));
    
    element.addAttributeValue(CDGQNAME("type"), nodetype);
    element.addAttributeValue(CDGQNAME("name"), getName());
    element.addAttributeValue(CDGQNAME("id"), tuple_cell::atomic_int(index));

    if (output) {
        element.addAttributeValue(CDGQNAME("output"), tuple_cell::atomic(true));
    };

    switch(type) {
        case dnDatabase :
            element.addElementValue(CDGQNAME("root"), root.toLRString());
            element.addElementValue(CDGQNAME("path"), path.toXPathString());
            break;
        case dnAlias :
            element.addElementValue(CDGQNAME("source"), tuple_cell::atomic_int(index));
            break;
        case dnFreeNode :
        case dnConst :
        case dnExternal :
            break;
    };

    element.closeElement();

    return element;
}


std::string DataNode::getName() const
{
    std::stringstream stream;

    if (varName.isnull()) {
        stream << "%" << varIndex;
    } else {
        stream << *varName;
    }

    return stream.str();
}

std::string SPredicate::toLRString() const
{
    std::stringstream stream;
    stream << "(sj " << left()->getName() << " " << right()->getName() << " " << path.toLRString() << ")";
    return stream.str();
}

std::string Comparison::toLRString() const
{
    std::stringstream stream;
    stream << "(";

    switch (op) {
        case g_eq :
            stream << "gc::eq";
            break;
        case do_before:
            stream << "do::before";
            break;
        default:
            stream << "invalid";
    };

    stream << ")";
    return stream.str();
}

std::string VPredicate::toLRString() const
{
    std::stringstream stream;
    stream << "(vj " << left()->getName() << " " << right()->getName() << " " << cmp.toLRString() << ")";
    return stream.str();
}

XmlConstructor & SPredicate::toXML(XmlConstructor & element) const
{
    element.openElement(CDGQNAME("SPredicate"));
  
    element.addAttributeValue(CDGQNAME("id"), tuple_cell::atomic_int(index));
    element.addElementValue(CDGQNAME("path"), path.toXPathString());
    element.addElementValue(CDGQNAME("left"), tuple_cell::atomic_int(left()->index));
    element.addElementValue(CDGQNAME("right"), tuple_cell::atomic_int(right()->index));
    
    element.closeElement();

    return element;
}

XmlConstructor & VPredicate::toXML(XmlConstructor & element) const
{
    element.openElement(CDGQNAME("SPredicate"));

    element.addAttributeValue(CDGQNAME("id"), tuple_cell::atomic_int(index));
    element.addElementValue(CDGQNAME("cmp"), cmp.toLRString());
    element.addElementValue(CDGQNAME("left"), tuple_cell::atomic_int(left()->index));
    element.addElementValue(CDGQNAME("right"), tuple_cell::atomic_int(right()->index));

    element.closeElement();

    return element;
}
