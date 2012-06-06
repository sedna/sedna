#ifndef _PREDICATES_H
#define _PREDICATES_H

#include "tr/opt/OptTypes.h"
#include "tr/opt/path/XPathTypes.h"
#include "tr/opt/path/DataSources.h"
#include "tr/executor/base/tuple.h"

namespace phop {
class IFunction;
}

class XmlConstructor;

namespace opt {

class DataNode;
class DataGraphMaster;

struct DataGraph {
    int lastIndex;
    int nodeCount;

    DataGraphMaster * owner;

    Predicate * predicates[MAX_GRAPH_SIZE];
    DataNode * dataNodes[MAX_GRAPH_SIZE];

    PlanDesc allPredicates;
    PlanDesc freePositions;
    PlanDesc inputNodes;

    PlanDesc getNeighbours(PlanDesc x);

    void updateIndex();
    void precompile();

    DataNodeList outputNodes;

    DataGraph(DataGraphMaster * _owner);

    void sameNode(DataNode * master, DataNode * alias);

    std::string toLRString() const;
    XmlConstructor & toXML(XmlConstructor & producer) const;
};

extern const int reverseComparisonMap[];

struct Comparison {
    enum comp_t {
        invalid = 0,
        g_eq = 0x01, g_gt = 0x02, g_ge = 0x03, g_lt = 0x04, g_le = 0x05,
        do_before = 0x11, do_after = 0x12,
    } op;

    Comparison() : op(invalid) {};
    explicit Comparison(enum comp_t _op) : op(_op) {};
    Comparison(const Comparison & x) : op(x.op) {};
    Comparison(const scheme_list * lst);

    bool inversable() const { return op == g_eq; };

    Comparison inverse() const {
        if (op == g_eq) {
            return *this;
        };
        
        U_ASSERT(false);
        return Comparison(invalid);
    };

    std::string toLRString() const;
};

struct Predicate {
    PlanDesc indexBit;
    int index;

    PlanDesc neighbours;
    PlanDesc dataNodeMask;
    PlanDesc evaluateAfter;

    DataNodeList dataNodeList;

    bool createContext;
    TupleId contextTuple;

    Predicate(DataGraph * dg);
    
    virtual void * compile(PhysicalModel * model) = 0;

    virtual std::string toLRString() const = 0;
    virtual XmlConstructor & toXML(XmlConstructor & ) const = 0;
};

struct BinaryPredicate : public Predicate {
    BinaryPredicate(DataGraph * dg, DataNode * left, DataNode * right);
    
    DataNode * left() const { return dataNodeList[0]; };
    DataNode * right() const { return dataNodeList[1]; };
};

/*
 * Predicate is used for test and switch expressions
 * Returns a constant boolean value (the result of comparison)
 */

struct PhantomPredicate : public Predicate {
    DataNode * goalNode;
    tuple_cell value;

    virtual void * compile(PhysicalModel * model);

    virtual std::string toLRString() const;
    virtual XmlConstructor & toXML(XmlConstructor & ) const;
};

/*
 * Predicate for function evaluation
 */
struct FPredicate : public BinaryPredicate {
    phop::IFunction * func;

    FPredicate(DataGraph * dg, DataNode* left, DataNode* right, phop::IFunction * f);

    virtual void * compile(PhysicalModel * model);

    virtual std::string toLRString() const;
    virtual XmlConstructor & toXML(XmlConstructor & ) const;
};

/*
 * Predicate for value operations
 */
struct VPredicate : public BinaryPredicate {
    Comparison cmp;

    VPredicate(opt::DataGraph* dg, opt::DataNode* left, opt::DataNode* right, const opt::Comparison& _cmp);
    
    virtual void * compile(PhysicalModel * model);

    virtual std::string toLRString() const;
    virtual XmlConstructor & toXML(XmlConstructor & ) const;
};

/*
 * Predicate for structural operations
 */
struct SPredicate : public BinaryPredicate {
    bool outer;
    pe::Path path;

    SPredicate(DataGraph* dg, DataNode* left, DataNode* right, const pe::Path & path);
    
    virtual void * compile(PhysicalModel * model);

    virtual std::string toLRString() const;
    virtual XmlConstructor & toXML(XmlConstructor & ) const;
};

typedef std::vector<tuple_cell> MemoryTupleSequence;
typedef counted_ptr< std::vector<tuple_cell> > MemoryTupleSequencePtr;

struct DataNode {
    enum data_node_type_t {
        dnConst = 1, dnExternal, dnDatabase, dnFreeNode, dnAlias, dnReplaced
    } type;

    DataNode * replacedWith;
    
    // Global index in master graph
    TupleId varIndex;
    
    // Index in graph and shifted index in graph
    int index;
    PlanDesc indexBit;

    // Is node output in parent graph 
    bool output;
    
    // Node index used while building execution schema
    int absoluteIndex;

    // Connected predicates
    PlanDesc predicates;

    // Data root information and path information
    DataRoot root;
    pe::Path path;

    // Value of constant node
    MemoryTupleSequencePtr sequence;
    
    // ???
    DataNode * source; 

    // Variable node came from
    std::string varName;
    TupleId varTupleId;

    // Used in compilation
    SPredicate * producedFrom;

    DataNode(data_node_type_t _type, int _varIndex, int _index)
      : type(_type), replacedWith(NULL), varIndex(_varIndex),
        index(_index), indexBit(1ULL << _index), output(false),
        absoluteIndex(0), predicates(0), source(NULL), varTupleId(-1), producedFrom(NULL)
    { };
    
    std::string getName() const;
    std::string toLRString() const;
    XmlConstructor & toXML(XmlConstructor & ) const;
};

};

#define FOR_ALL_GRAPH_ELEMENTS(EL, IV) for (unsigned IV = 0; IV < MAX_GRAPH_SIZE; ++IV) if ((EL)[IV] != NULL) \

#endif /* _PREDICATES_H */
