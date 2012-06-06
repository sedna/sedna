#include "tr/opt/algebra/DataGraph.h"
#include "tr/opt/algebra/Predicates.h"
#include "tr/opt/phm/PhysicalModel.h"

#include "tr/models/XmlConstructor.h"

#include <algorithm>

using namespace opt;

DataGraph::DataGraph(DataGraphMaster* _owner) : lastIndex(1), owner(_owner)
{
    memset(predicates, 0, sizeof(predicates));
    memset(dataNodes, 0, sizeof(dataNodes));
}

Predicate::Predicate(DataGraph* dg)
{
    if (dg != NULL) {
        dg->owner->createPredicate(dg, this);
    }
}

BinaryPredicate::BinaryPredicate(DataGraph* dg, DataNode* left, DataNode* right)
    : Predicate(dg)
{
    dataNodeList.push_back(left);
    dataNodeList.push_back(right);
}

SPredicate::SPredicate(DataGraph* dg, DataNode* left, DataNode* right, const pe::Path& _path)
    : BinaryPredicate(dg, left, right), outer(false), path(_path)
{
    
}

VPredicate::VPredicate(DataGraph* dg, DataNode* left, DataNode* right, const Comparison & _cmp)
    : BinaryPredicate(dg, left, right), cmp(_cmp)
{

}

FPredicate::FPredicate(DataGraph* dg, DataNode* left, DataNode* right, phop::IFunction* f)
    : BinaryPredicate(dg, left, right), func(f)
{

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

void* FPredicate::compile(PhysicalModel* model)
{
    return model->compile(this);
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
            while ((*d)->type == DataNode::dnReplaced) {
                *d = (*d)->replacedWith;
            };

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

using namespace pe;

void DataGraph::sameNode(DataNode* master, DataNode* alias)
{
    U_ASSERT(alias->type == DataNode::dnAlias || alias->type == DataNode::dnExternal);

    PlanDescIterator it(alias->predicates);
    int i;

    while (-1 != (i = it.next())) {
        Predicate * p = predicates[i];

        for (DataNodeList::iterator d = p->dataNodeList.begin(); d != p->dataNodeList.end(); ++d) {
            if (*d == alias) {
                *d = master;
            }
        }
    }

    dataNodes[alias->index] = NULL;
    alias->replacedWith = master;
    alias->type = DataNode::dnReplaced;
    master->output = master->output || alias->output;

    U_ASSERT(master->varTupleId == alias->varTupleId || master->varTupleId == -1);
    U_ASSERT(master->varName.empty() || master->varName == alias->varName);
    
    master->varTupleId = alias->varTupleId;
    master->varName = alias->varName;
}

/* Static optimization phase */

void DataGraph::precompile()
{
    DataNodeList list1, list2;
    DataNodeList *frontList = &list1, *backList = &list2;

    typedef std::set< std::pair<DataNode *, DataNode *> > RemovalList;
    RemovalList removalCandidates;

    updateIndex();

/* Find self references
 */
    FOR_ALL_GRAPH_ELEMENTS(dataNodes, i) {
        DataNode * dn = dataNodes[i];
        if (dn->type == DataNode::dnExternal) {

            std::pair<VariableMap::iterator, VariableMap::iterator> jt =
                owner->variableMap.equal_range(dn->varTupleId);

            for (VariableMap::iterator j = jt.first; j != jt.second; ++j) {
                if (dataNodes[j->second->index] == j->second &&
                    j->second != dn &&
                    j->second->type != DataNode::dnExternal)
                {
                    sameNode(j->second, dn);
                };
            };
        }
    };

/* Replace aliases
 */
    FOR_ALL_GRAPH_ELEMENTS(dataNodes, i) {
        if (dataNodes[i]->type == DataNode::dnAlias) {
            sameNode(dataNodes[i]->source, dataNodes[i]);
        }
    };

    updateIndex();

/* Replace document order comparisons with path expressions
 */
    FOR_ALL_GRAPH_ELEMENTS(predicates, i) {
        VPredicate * pred = dynamic_cast<VPredicate*>(predicates[i]);

        if (pred != NULL && (pred->cmp.op == Comparison::do_after || pred->cmp.op == Comparison::do_before)) {
            pe::Step step;

            if (pred->cmp.op == Comparison::do_after) {
                step = pe::Step(pe::axis_following, nt_any_kind, xsd::QNameAny);
            } else {
                step = pe::Step(pe::axis_preceding, nt_any_kind, xsd::QNameAny);
            };

            SPredicate * rep = new SPredicate(NULL, pred->left(), pred->right(), step);
            owner->replacePredicate(this, pred, rep);
        };
    }

    updateIndex();
    
/* Remove all redundant nodes (i.e. all, that comes with path expression)
 */

    FOR_ALL_GRAPH_ELEMENTS(dataNodes, i) {
        DataNode * dn = dataNodes[i];
        
        if (dataNodes[i]->type != DataNode::dnFreeNode || dataNodes[i]->output) {
            continue;
        };
        
        PlanDescIterator it(dn->predicates);

        int leftIdx = it.next();
        int rightIdx = it.next();

        if (leftIdx == -1 || rightIdx == -1 || it.next() != -1) {
            continue;
        };

        SPredicate * left = dynamic_cast<SPredicate *> (predicates[leftIdx]);
        SPredicate * right = dynamic_cast<SPredicate *> (predicates[rightIdx]);

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

        predicates[right->index] = NULL;
        dataNodes[i] = NULL;

        /* This step is absolutely important to iterate through graph further
         */
        left->dataNodeList[1] = right->right();

        right->right()->predicates =
            (right->right()->predicates & ~right->indexBit) | left->indexBit;
    };

    updateIndex();
    
/* Find all root vertices
*/
    FOR_ALL_GRAPH_ELEMENTS(dataNodes, i) {
        if (dataNodes[i]->type == DataNode::dnDatabase) {
            frontList->push_back(dataNodes[i]);
        };                      
    };
/*
/* Propagate all path expressions from root vertices

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

                    backList->push_back(pred->right());
                }
            }
        };

        DataNodeList* swp = frontList;
        frontList = backList;
        backList = swp;
    }
    
    updateIndex();
*/
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
        case dnReplaced :
            stream << " REMOVED ";
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
        case dnReplaced : nodetype = "REMOVED"; break;
    };

    element.openElement(CDGQNAME("node"));
    
    element.addAttributeValue(CDGQNAME("type"), nodetype);
    element.addAttributeValue(CDGQNAME("name"), getName());
    element.addAttributeValue(CDGQNAME("id"), tuple_cell::atomic_int(varIndex));
    element.addAttributeValue(CDGQNAME("index"), tuple_cell::atomic_int(index));

    if (output) {
        element.addAttributeValue(CDGQNAME("output"), tuple_cell::atomic(true));
    };

    switch(type) {
        case dnDatabase :
            element.addElementValue(CDGQNAME("root"), root.toLRString());
            element.addElementValue(CDGQNAME("path"), path.toXPathString());
            break;
        case dnAlias :
            element.addElementValue(CDGQNAME("source"), tuple_cell::atomic_int(source->varIndex));
            break;
        case dnReplaced :
            element.addElementValue(CDGQNAME("with"), tuple_cell::atomic_int(replacedWith->varIndex));
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

    if (varName.empty()) {
        stream << "%" << varIndex;
    } else {
        stream << varName;
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

std::string FPredicate::toLRString() const
{
  return "";
}

std::string VPredicate::toLRString() const
{
    std::stringstream stream;
    stream << "(vj " << left()->getName() << " " << right()->getName() << " " << cmp.toLRString() << ")";
    return stream.str();
}

XmlConstructor& FPredicate::toXML(XmlConstructor& element) const
{
    element.openElement(CDGQNAME("Function"));

    element.addAttributeValue(CDGQNAME("id"), tuple_cell::atomic_int(index));
//    element.addElementValue(CDGQNAME("path"), path.toXPathString());
    element.addElementValue(CDGQNAME("left"), tuple_cell::atomic_int(left()->index));
    element.addElementValue(CDGQNAME("right"), tuple_cell::atomic_int(right()->index));

    element.closeElement();

    return element;
}

XmlConstructor & SPredicate::toXML(XmlConstructor & element) const
{
    element.openElement(CDGQNAME("StructurePredicate"));
  
    element.addAttributeValue(CDGQNAME("id"), tuple_cell::atomic_int(index));
    element.addElementValue(CDGQNAME("path"), path.toXPathString());
    element.addElementValue(CDGQNAME("left"), tuple_cell::atomic_int(left()->index));
    element.addElementValue(CDGQNAME("right"), tuple_cell::atomic_int(right()->index));
    
    element.closeElement();

    return element;
}

XmlConstructor & VPredicate::toXML(XmlConstructor & element) const
{
    element.openElement(CDGQNAME("ValuePredicate"));

    element.addAttributeValue(CDGQNAME("id"), tuple_cell::atomic_int(index));
    element.addElementValue(CDGQNAME("cmp"), cmp.toLRString());
    element.addElementValue(CDGQNAME("left"), tuple_cell::atomic_int(left()->index));
    element.addElementValue(CDGQNAME("right"), tuple_cell::atomic_int(right()->index));

    element.closeElement();

    return element;
}
