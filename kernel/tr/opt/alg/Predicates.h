#ifndef _PREDICATES_H
#define _PREDICATES_H

#include "tr/opt/OptTypes.h"
#include "tr/executor/xpath/XPathTypes.h"
#include "tr/executor/base/tuple.h"

struct IElementProducer;

class DataNode;
class DataGraphMaster;

class DataRoot {
public:
    enum data_root_type_t {
        drt_null,
        drt_document,
        drt_collection,
        drt_external,
    };
private:
    data_root_type_t type;
    counted_ptr<std::string> name;
public:
    DataRoot() : type(drt_null), name(NULL) {};
    DataRoot(data_root_type_t type, const char * name);
    DataRoot(const counted_ptr<db_entity> dbe);
    DataRoot(const scheme_list * x);

    counted_ptr<db_entity> toDBEntity() const;

    DataRoot(const DataRoot & x) : type(x.type), name(x.name) {};
    const DataRoot& operator=(const DataRoot& x) {
        if (&x != this) {
            type = x.type;
            name = x.name;
        }
        return *this;
    }

    std::string toLRString() const;
};

struct DataGraph {
    int lastIndex;

    DataGraphMaster * owner;

    PredicateList predicates;
    DataNodeList dataNodes;

    void updateIndex();
    void precompile();

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

struct Comparison {
    enum comp_t {
        invalid,
        g_eq, g_gt, g_ge, g_lt, g_le,
        do_before, do_after,
    } op;

    Comparison() : op(invalid) {};
    Comparison(enum comp_t _op) : op(_op) {};
    Comparison(const Comparison & x) : op(x.op) {};
    Comparison(const scheme_list * lst);

    std::string toLRString() const;
};


struct Predicate {
    PlanDesc indexBit;
    int index;

    PlanDesc neighbours;
    PlanDesc dataNodes;

    virtual void * compile(PhysicalModel * model) = 0;
    virtual void update() = 0;

    virtual bool replacable(DataNode * n1, DataNode * n2);
    virtual Predicate * replace(DataNode * n1, DataNode * n2);

    virtual std::string toLRString() const = 0;
    virtual IElementProducer * toXML(IElementProducer * ) const = 0;
};

struct BinaryPredicate : public Predicate {
    DataNode * leftNode;
    DataNode * rightNode;

    void setVertices(DataGraph* dg, TupleId left, TupleId right);

    virtual void update();
};

struct VPredicate : public BinaryPredicate {
    Comparison cmp;

    virtual void * compile(PhysicalModel * model);

    virtual std::string toLRString() const;
    virtual IElementProducer * toXML(IElementProducer * ) const;
};

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
    counted_ptr<MemoryTupleSequence> sequence; // Actually, we assume, that this is the ONLY pointer to this array

    std::string getName() const;
    std::string toLRString() const;
    IElementProducer * toXML(IElementProducer * ) const;
};

#endif /* _PREDICATES_H */
