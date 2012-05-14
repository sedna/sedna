#ifndef _PREDICATES_H
#define _PREDICATES_H

#include "tr/opt/OptTypes.h"
#include "tr/opt/path/XPathTypes.h"
#include "tr/opt/path/DataSources.h"
#include "tr/executor/base/tuple.h"

struct IElementProducer;

namespace opt {

class DataNode;
class DataGraphMaster;

struct DataGraph {
    int lastIndex;
    int nodeCount;

    DataGraphMaster * owner;

    PredicateList predicates;
    DataNodeList dataNodes;

    PlanDesc allPredicates;

    void updateIndex();
    void precompile();

    DataNodeList outputNodes;
    //    DataNodeList groupByNodes;
    //    DataNodeList orderByNodes;
    //    DataNodeList contextNodes;

    VariableMap varMap;

    DataGraph(DataGraphMaster * _owner);

    bool replaceNode(DataNode * what, DataNode * with_what);

    PlanDesc getNeighbours(PlanDesc x);

    std::string toLRString() const;
    IElementProducer * toXML(IElementProducer * producer) const;
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

    bool inversable() const { return false; };
    Comparison inverse() const { U_ASSERT(false); return Comparison(invalid); };

    std::string toLRString() const;
};


struct Predicate {
    PlanDesc indexBit;
    int index;

    PlanDesc neighbours;
    PlanDesc dataNodeMask;

    DataNodeList dataNodeList;

    virtual void * compile(PhysicalModel * model) = 0;

    virtual bool replacable(DataNode * n1, DataNode * n2);
    virtual Predicate * replace(DataNode * n1, DataNode * n2);

    virtual std::string toLRString() const = 0;
    virtual IElementProducer * toXML(IElementProducer * ) const = 0;
};

struct BinaryPredicate : public Predicate {
    void setVertices(DataGraph* dg, TupleId left, TupleId right);

    DataNode * left() const { return dataNodeList[0]; };
    DataNode * right() const { return dataNodeList[1]; };
};

struct VPredicate : public BinaryPredicate {
    Comparison cmp;

    virtual void * compile(PhysicalModel * model);

    virtual std::string toLRString() const;
    virtual IElementProducer * toXML(IElementProducer * ) const;
};

/* TODO:

struct ValuedExpression : public BinaryPredicate {
};

struct LongIndexCandidate : public BinaryPredicate {
};
*/

/*
struct GPredicate : public BinaryPredicate {
    virtual void * compile(PhysicalModel * model);
};

struct NPredicate : public BinaryPredicate {
    virtual void * compile(PhysicalModel * model);
};
*/

struct SPredicate : public BinaryPredicate {
    bool disposable;

    bool outer;
    pe::Path path;

    SPredicate() : disposable(false) {};

    virtual void * compile(PhysicalModel * model);

    virtual bool replacable(DataNode * n1, DataNode * n2);
    virtual Predicate * replace(DataNode* n1, DataNode* n2);

    virtual std::string toLRString() const;
    virtual IElementProducer * toXML(IElementProducer * ) const;
};

typedef std::vector<tuple_cell> MemoryTupleSequence;

struct DataNode {
    counted_ptr<std::string> varName;

    int absoluteIndex;

    TupleId varIndex;
    PlanDesc indexBit;
    int index;

    bool output, grouping, ordered;

    PlanDesc predicates;

    enum data_node_type_t {
        dnConst, dnExternal, dnDatabase, dnFreeNode
    } type;

    DataRoot root;
    pe::Path path;
    DataNode * source;

    // FIXME: Uninitialized!
    SPredicate * producedFrom;

    counted_ptr<MemoryTupleSequence> sequence; // Actually, we assume, that this is the ONLY pointer to this array

    std::string getName() const;
    std::string toLRString() const;
    IElementProducer * toXML(IElementProducer * ) const;
};

};

#endif /* _PREDICATES_H */
