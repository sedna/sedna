#ifndef _DATA_GRAPHS_H_
#define _DATA_GRAPHS_H_

#include "tr/opt/OptTypes.h"
#include "tr/opt/path/DataSources.h"

class XmlConstructor;

namespace opt {

class DataGraph;
class StructuralPredicate;

struct Predicate : public IPlanDisposable {
    int index;
    PlanDesc indexBit;

    PredicateList neighbours;
    PredicateList evaluateAfter;
    DataNodeList dataNodeList;

//    bool createContext;
//    TupleId contextTuple;

    void setIndex(int _index) {
        index = _index;
        indexBit = 1ULL << _index;
    };

    Predicate() : index(0), indexBit(0) {};

    virtual void * compile(PhysicalModel * model) = 0;

//    virtual std::string toLRString() const = 0;
    virtual XmlConstructor & toXML(XmlConstructor & ) const = 0;
};

struct DataNode : public IPlanDisposable {
    enum data_node_type_t {
        dnConst = 1, dnExternal, dnDatabase, dnFreeNode, dnAlias, dnReplaced
    } type;

    DataNode * replacedWith; // If node is replaced, tells, what is it replaced with

    int index; // Index in graph 
    PlanDesc indexBit; // Shifted index in graph

    int absoluteIndex; // Node index used while building execution schema

    // Data root information and path information
    DataRoot root;
    pe::Path path;

    MemoryTupleSequencePtr sequence; // Value of constant node

    DataNode * aliasFor;

    TupleId varTupleId; // Variable node came from

    StructuralPredicate * producedFrom; // Used in compilation

    DataGraph * parent;
    
    explicit DataNode(data_node_type_t _type)
        : type(_type), replacedWith(NULL), index(0), indexBit(0),
          absoluteIndex(0), aliasFor(NULL),
          varTupleId(opt::invalidTupleId), producedFrom(NULL),
          parent(NULL)
    { };

    void setIndex(int _index) {
        index = _index;
        indexBit = 1ULL << _index;
    };

    XmlConstructor & toXML(XmlConstructor & ) const;
};

struct PredicateIndex {
    Predicate * p;
    PlanDesc neighbours;
    PlanDesc dataNodeMask;
    PlanDesc evaluateAfter;
};

struct DataNodeIndex {
    DataNode * p;
    PlanDesc predicates;
    bool input;
    bool output;
};

class DataGraphMaster;

struct DataGraph : public IPlanDisposable {
    DataGraphMaster * owner;

    Predicate * predicates[MAX_GRAPH_SIZE];
    DataNode * dataNodes[MAX_GRAPH_SIZE];

    PlanDesc inputNodes;
    PlanDesc outputNodes;

    explicit DataGraph(DataGraphMaster* _owner);
    XmlConstructor & toXML(XmlConstructor & ) const;
};

struct DataGraphWrapper {
    DataGraph * dg;

    DataNodeList nodes;
    DataNodeList in;
    DataNodeList out;

    TupleScheme inTuples;
    TupleScheme outTuples;

    PredicateList predicates;

    explicit DataGraphWrapper(DataGraph * _dg);

    void update();
    void rebuild();
};

struct DataGraphBuilder {
    DataNodeList nodes;
    DataNodeList out;

    PredicateList predicates;

    DataGraph * build(DataGraphMaster * master);
    DataGraph * make(DataGraphMaster * master, DataGraph * graph);
};

struct DataGraphIndex {
    DataGraph * dg;

    PredicateIndex predicates[MAX_GRAPH_SIZE];
    DataNodeIndex nodes[MAX_GRAPH_SIZE];
    
    PlanDesc allPredicates;

    explicit DataGraphIndex(DataGraph * _dg);

    PlanDesc getNeighbours(PlanDesc x) {
        PlanDesc result = 0;
        PlanDescIterator iter(x);

        int i;
        while (-1 != (i = iter.next())) {
            result |= predicates[i].neighbours;
        }

        return (result & ~x);
    };

    void update();
};

};

#define FOR_ALL_GRAPH_ELEMENTS(EL, IV) for (unsigned IV = 0; IV < MAX_GRAPH_SIZE; ++IV) if ((EL)[IV] != NULL)
#define FOR_ALL_INDEXES(EL, IV) for (unsigned IV = 0; IV < MAX_GRAPH_SIZE; ++IV) if ((EL)[IV].p != NULL)

#endif /* _DATA_GRAPHS_H_ */
