#ifndef _DATA_GRAPHS_H_
#define _DATA_GRAPHS_H_

#include "tr/opt/OptTypes.h"

class XmlConstructor;

namespace opt {

class SPredicate;

struct Predicate : public IPlanDisposable {
    int index;
    PlanDesc indexBit;

    PlanDesc neighbours;
    PlanDesc dataNodeMask;
    PlanDesc evaluateAfter;

    DataNodeList dataNodeList;

//    bool createContext;
//    TupleId contextTuple;

    Predicate(DataGraph * dg);

    virtual void * compile(PhysicalModel * model) = 0;

    virtual std::string toLRString() const = 0;
    virtual XmlConstructor & toXML(XmlConstructor & ) const = 0;
};

struct DataNode : public IPlanDisposable {
    enum data_node_type_t {
        dnConst = 1, dnExternal, dnDatabase, dnFreeNode, dnAlias, dnReplaced
    } type;

    DataNode * replacedWith; // If node is replaced, tells, what is it replaced with
    TupleId varIndex; // Global index in master graph

    int index; // Index in graph 
    PlanDesc indexBit; // Shifted index in graph
    int absoluteIndex; // Node index used while building execution schema
    PlanDesc predicates; // Connected predicates

    // Data root information and path information
    DataRoot root;
    pe::Path path;

    MemoryTupleSequencePtr sequence; // Value of constant node

    DataNode * aliasFor;

    std::string varName;
    TupleId varTupleId; // Variable node came from

    SPredicate * producedFrom; // Used in compilation

    DataNode(data_node_type_t _type, int _varIndex, int _index)
        : type(_type), replacedWith(NULL), varIndex(_varIndex),
            index(_index), indexBit(1ULL << _index), absoluteIndex(0),
            predicates(0), aliasFor(NULL), varTupleId(opt::invalidTupleId), producedFrom(NULL)
    { };

    std::string getName() const;
    XmlConstructor & toXML(XmlConstructor & ) const;
};

class DataGraphMaster;

struct DataGraph : public IPlanDisposable {
    DataGraphMaster * owner;

    Predicate * predicates[MAX_GRAPH_SIZE];
    DataNode * dataNodes[MAX_GRAPH_SIZE];

    PlanDesc inputNodes;
    PlanDesc outputNodes;

    XmlConstructor & toXML(XmlConstructor & ) const;
};

struct DataGraphWrapper {
    DataGraph * dg;

    DataNodeList nodes;
    DataNodeList in;
    DataNodeList out;

    PredicateList predicates;

    DataGraphWrapper(DataGraph * _dg);
};

struct DataGraphBuilder {
    int lastIndex;

    DataNodeList nodes;
    DataNodeList out;

    PredicateList predicates;

    DataGraph * build(DataGraphMaster * master);
};

struct DataGraphIndex {
    DataGraph * dg;

    PlanDesc allPredicates;
    PlanDesc freePositions;

    DataGraphIndex(DataGraph * _dg);

    PlanDesc getNeighbours(PlanDesc x);
    void update();
};

};

#define FOR_ALL_GRAPH_ELEMENTS(EL, IV) for (unsigned IV = 0; IV < MAX_GRAPH_SIZE; ++IV) if ((EL)[IV] != NULL) \

#endif /* _DATA_GRAPHS_H_ */
