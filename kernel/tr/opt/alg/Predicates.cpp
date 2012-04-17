#include "tr/opt/alg/Predicates.h"
#include "tr/opt/phm/PhysicalModel.h"
#include "tr/opt/alg/DataGraph.h"

#include "tr/structures/producer.h"

// tmp
#include <boost/lexical_cast.hpp>

#include <bits/algorithmfwd.h>

DataRoot::DataRoot(const data_root_type_t _type, const char* _name)
    : type(_type), name(NULL)
{
    name = new std::string(_name);
}

DataRoot::DataRoot(const scheme_list* lst)
    : type(drt_null), name(NULL)
{
    if (lst->size() != 2 || lst->at(0).type != SCM_SYMBOL || lst->at(1).type != SCM_STRING) {
        throw USER_EXCEPTION2(SE1004, "Invalid root LR string");
    }

    const char * _type = lst->at(0).internal.symb;

    switch (_type[0]) {
        case 'd' : if (strncmp("doc", _type, 3) == 0) {
            type = drt_document;
            break;
        };
        case 'c' : if (strncmp("coll", _type, 4) == 0) {
            type = drt_collection;
            break;
        };
        default : 
            throw USER_EXCEPTION2(SE1004, "Invalid root type");
    };

    name = new std::string(lst->at(1).internal.str);
}

DataGraph::DataGraph(DataGraphMaster* _owner) : lastIndex(1), owner(_owner), predicates(64, NULL), dataNodes(64, NULL)
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

void BinaryPredicate::setVertices(DataGraph* dg, TupleId left, TupleId right)
{
    leftNode = dg->owner->getVarNode(left);
    rightNode = dg->owner->getVarNode(right);
    dataNodes = leftNode->indexBit | rightNode->indexBit;
}

PlanDesc DataGraph::getNeighbours(PlanDesc x)
{
    PlanDescIterator iter(x);
    PlanDesc result = 0;
    
    int i;
    while (-1 != (i = iter.next())) {
        result |= predicates.at(i)->neighbours;
    }

    return (result & ~x);
}

void DataGraph::updateIndex()
{
    allPredicates = 0;

    for (DataNodeList::iterator d = dataNodes.begin(); d != dataNodes.end(); ++d) {
        if (*d != NULL) {
            (*d)->predicates = 0;
        }
    };

    for (PredicateList::iterator p = predicates.begin(); p != predicates.end(); ++p) {
        if (*p != NULL) {
            (*p)->update();
            allPredicates |= (*p)->indexBit;
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

    if (n1->output || n1->ordered || n1->grouping) {
        return false;
    };

    int i;

    PlanDescIterator it(n1->predicates);
    while (-1 != (i = it.next())) {
        U_ASSERT(this->predicates.at(i) != NULL);

        Predicate * p = this->predicates.at(i);

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
    const StepPredicate redundantPathStep(StepPredicate::axis(axis_child) | StepPredicate::axis(axis_parent));

    return this->disposable && path.forall(redundantPathStep);
}

Predicate * SPredicate::replace(DataNode* n1, DataNode* n2)
{
    if (replacable(n1, n2)) {
        return NULL;
    } else {
        return this;
    }
}


void BinaryPredicate::update()
{
    dataNodes = leftNode->indexBit | rightNode->indexBit;

    leftNode->predicates |= indexBit;
    rightNode->predicates |= indexBit;

    neighbours = (leftNode->predicates | rightNode->predicates) & ~indexBit;
}







/* Static optimization phase */


void DataGraph::precompile()
{
    DataNodeList list1, list2;
    DataNodeList *frontList = &list1, *backList = &list2;

    typedef std::set< std::pair<DataNode *, DataNode *> > RemovalList;
    RemovalList removalCandidates;

// Find all root vertices

    for (DataNodeList::iterator d = dataNodes.begin(); d != dataNodes.end(); ++d) {
        if (*d != NULL && (*d)->type == DataNode::dnDatabase) {
            frontList->push_back(*d);
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
                SPredicate * pred = dynamic_cast<SPredicate*>(this->predicates.at(i));

                if (NULL != pred && pred->leftNode == dn &&
                  pred->rightNode->type == DataNode::dnFreeNode)
                {
                    pe::Path path = (pred->leftNode->path + pred->path).squeeze();

/*
                    if (!path.evaluatable()) {
                        continue;
                    }
*/
                    pred->rightNode->type = DataNode::dnDatabase;
                    pred->rightNode->root = pred->leftNode->root;
                    pred->rightNode->path = path;

                    pred->disposable = true;

                    backList->push_back(pred->rightNode);

                    if (!dn->output) {
                        removalCandidates.insert(RemovalList::key_type(dn, pred->rightNode));
                    }
                }
            }
        };

        DataNodeList* swp = frontList;
        frontList = backList;
        backList = swp;
    }

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
    for (DataNodeList::const_iterator i = dataNodes.begin(); i != dataNodes.end(); ++i) {
        if (*i != NULL) {
            stream << (*i)->toLRString();
        }
    };
    stream << ")";

    stream << "(";
    for (PredicateList::const_iterator i = predicates.begin(); i != predicates.end(); ++i) {
        if (*i != NULL) {
            stream << (*i)->toLRString();
        }
    };
    stream << ")";

    stream << ")";
    return stream.str();
}

#define CDGQNAME(N) xsd::QName::getConstantQName(NULL_XMLNS, N)

IElementProducer* DataGraph::toXML(IElementProducer* producer) const
{
    IElementProducer* result = producer->addElement(CDGQNAME("datagraph"), xs_anyType);

    for (DataNodeList::const_iterator i = dataNodes.begin(); i != dataNodes.end(); ++i) {
        if (*i != NULL) {
            (*i)->toXML(result);
        }
    };

    for (PredicateList::const_iterator i = predicates.begin(); i != predicates.end(); ++i) {
        if (*i != NULL) {
            (*i)->toXML(result);
        }
    };

    result->close();

    return result;
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
    };

    if (output) {
        stream << " output ";
    };

    stream << ")";

    return stream.str();
}

IElementProducer* DataNode::toXML(IElementProducer* element) const
{
    const char * nodetype = NULL;
    IElementProducer* child;

    switch(type) {
        case dnDatabase : nodetype = "root"; break;
        case dnFreeNode : nodetype = "free"; break;
        case dnConst : nodetype = "const"; break;
        case dnExternal : nodetype = "ext"; break;
    };

    element = element->addElement(CDGQNAME("node"), xs_untyped);
    element->addAttribute(CDGQNAME("type"), text_source_cstr(nodetype), xs_string);
    element->addAttribute(CDGQNAME("name"), text_source_cstr(getName().c_str()), xs_string);
    element->addAttribute(CDGQNAME("id"), text_source_cstr(boost::lexical_cast<std::string, int>(index).c_str()), xs_integer);

    if (output) {
        element->addAttribute(CDGQNAME("output"), text_source_cstr("true"), xs_boolean);
    };

    switch(type) {
        case dnDatabase :
            child = element->addElement(CDGQNAME("root"), xs_untyped);
            child->addText(text_source_cstr(root.toLRString().c_str()));
            child->close();

            child = element->addElement(CDGQNAME("path"), xs_untyped);
            child->addText(text_source_cstr(path.toXPathString().c_str()));
            child->close();

            break;
        case dnFreeNode :
            break;
        case dnConst :
            break;
        case dnExternal :
            break;
    };

    element->close();

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


std::string DataRoot::toLRString() const
{
    std::stringstream stream;

    stream << "(";

    switch(type) {
        case drt_document :
            stream << " doc ";
            stream << "\"" << *name << "\"";
            break;
        case drt_collection :
            stream << " collection ";
            stream << "\"" << *name << "\"";
            break;
        case drt_null :
            stream << " null ";
            break;
        case drt_external :
            stream << " ext ";
            stream << "\"" << *name << "\"";
            break;
    };

    stream << ")";

    return stream.str();
}

std::string SPredicate::toLRString() const
{
    std::stringstream stream;
    stream << "(sj " << leftNode->getName() << " " << rightNode->getName() << " " << path.toLRString() << ")";
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
    stream << "(vj " << leftNode->getName() << " " << rightNode->getName() << " " << cmp.toLRString() << ")";
    return stream.str();
}

IElementProducer* SPredicate::toXML(IElementProducer* element) const
{
    IElementProducer* child;

    element = element->addElement(CDGQNAME("SPredicate"), xs_untyped);
    element->addAttribute(CDGQNAME("id"), text_source_cstr(boost::lexical_cast<std::string, int>(index).c_str()), xs_integer);

    child = element->addElement(CDGQNAME("path"), xs_untyped);
    child->addText(text_source_cstr(path.toXPathString().c_str()));
    child->close();

    child = element->addElement(CDGQNAME("left"), xs_untyped);
    child->addAttribute(CDGQNAME("node-id"), text_source_cstr(boost::lexical_cast<std::string, int>(leftNode->index).c_str()), xs_integer);
    child->close();

    child = element->addElement(CDGQNAME("right"), xs_untyped);
    child->addAttribute(CDGQNAME("node-id"), text_source_cstr(boost::lexical_cast<std::string, int>(rightNode->index).c_str()), xs_integer);
    child->close();

    element->close();

    return element;
}

IElementProducer* VPredicate::toXML(IElementProducer* element) const
{
    IElementProducer* child;

    element = element->addElement(CDGQNAME("VPredicate"), xs_untyped);
    element->addAttribute(CDGQNAME("id"), text_source_cstr(boost::lexical_cast<std::string, int>(index).c_str()), xs_integer);

    child = element->addElement(CDGQNAME("cmp"), xs_untyped);
    child->addText(text_source_cstr(cmp.toLRString().c_str()));
    child->close();

    child = element->addElement(CDGQNAME("left"), xs_untyped);
    child->addAttribute(CDGQNAME("node-id"), text_source_cstr(boost::lexical_cast<std::string, int>(leftNode->index).c_str()), xs_integer);
    child->close();

    child = element->addElement(CDGQNAME("right"), xs_untyped);
    child->addAttribute(CDGQNAME("node-id"), text_source_cstr(boost::lexical_cast<std::string, int>(rightNode->index).c_str()), xs_integer);
    child->close();

    element->close();

    return element;
}
