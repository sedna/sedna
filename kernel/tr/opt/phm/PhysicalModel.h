#ifndef PHYSICAL_MODEL_H
#define PHYSICAL_MODEL_H

#include "tr/models/XmlConstructor.h"
#include "tr/models/rtti.h"

#include "common/sedna.h"

#include "tr/opt/OptTypes.h"
#include "tr/opt/graphs/DataGraphs.h"

namespace opt {

struct TupleValueInfo {
    enum element_status_t {
        empty = 0,
        available,
        evaluated,
    };

    element_status_t status;
    tuple_info_t m_stat;

    inline TupleStatistics *& statistics() { return m_stat.statistics; }

    DataNode * node;
    POProt * _gen;
};

/* This structure describe tuple the raw */
class TupleChrysalis : public IPlanDisposable, IXMLSerializable {
public:
    explicit TupleChrysalis(size_t size);
    explicit TupleChrysalis(const TupleChrysalis * parent);

    std::vector<TupleValueInfo> tuples;
    TupleValueInfo * get(TupleId i) { return &(tuples.at(i)); };

    unsigned _width;
    unsigned width() const { return _width; };

    Range rowSize;
    Range rowCount;

    /* Sort info */
    inline void unsorted() {
        for (std::vector<TupleValueInfo>::iterator it = tuples.begin(); it != tuples.end(); ++it) {
            it->m_stat.flags &= !(tuple_info_t::sf_ddo_sorted);
        }
    };

    virtual XmlConstructor & toXML(XmlConstructor &) const;
};

class POProtIn { public:
    POProt * op;
    TupleId index;

    POProtIn() : op(NULL), index(0) {};
    POProtIn(POProt * _op, const TupleId _index) : op(_op), index(_index) { };
    POProtIn(const POProtIn& _other) : op(_other.op), index(_other.index) { };

    POProt * operator->() const { return op; }
};

class POProt : public ObjectBase, public IPlanDisposable, IXMLSerializable {
    RTTI_DECL(physical_model_POProt, ObjectBase)

    phop::IOperator * op;
protected:
    OperationCost * cost;

    XmlConstructor & __commonToXML(XmlConstructor &) const;
    virtual XmlConstructor & __toXML(XmlConstructor &) const;

    virtual phop::IOperator * compile() = 0;
public:
    std::vector<POProtIn> in;
    TupleChrysalis * result;
    std::vector<TupleId> resultSet;

    OperationCost * getCost() {
        if (cost == NULL) { evaluateCost(NULL); }
        return cost;
    }

    phop::IOperator * recompile() { op = compile(); return op; };
    
    phop::IOperator * getStatement() {
        U_ASSERT(op == NULL);
      
        if (op == NULL) {
            op = compile();
        };

        return op;
    };

    POProt(clsinfo_t pinfo) : ObjectBase(pinfo), op(NULL), cost(NULL), result(NULL) {};

    virtual void evaluateCost(CostModel * model) = 0;
    virtual XmlConstructor & toXML(XmlConstructor &) const;
};

class TupleRef { public:
    TupleChrysalis * tupleDesc;
    TupleId tid;

    TupleRef() : tupleDesc(NULL), tid(0) {};
    TupleRef(TupleChrysalis * _tupleDesc, TupleId _tid) : tupleDesc(_tupleDesc), tid(_tid) {};
    TupleRef(const TupleRef & _x) : tupleDesc(_x.tupleDesc), tid(_x.tid) {};

//    explicit TupleRef(const POProtIn & _x) : tupleDesc(_x.op->result), tid(_x.index) {};
    explicit TupleRef(const POProtIn & _x, TupleChrysalis * _default)
      : tupleDesc(_default), tid(_x.index) { if (_x.op != NULL) { tupleDesc = _x.op->result; }; };

    explicit TupleRef(const POProtIn & _x)
      : tupleDesc(_x.op->result), tid(_x.index) { tupleDesc = _x.op->result; };

    TupleRef & operator=(const TupleRef & _x) { if (this != &_x) { tupleDesc = _x.tupleDesc; tid = _x.tid; }; return *this; };

    TupleValueInfo * get() const { return tupleDesc->get(tid); }
    TupleValueInfo & operator*() const { return *tupleDesc->get(tid); };
    TupleValueInfo * operator->() const { return tupleDesc->get(tid); }
};

typedef std::vector<POProt *> OperationList;

class PlanInfo : public IPlanDisposable, public IXMLSerializable {
friend class PhysicalModel;
protected:
    PlanDesc desc, parent;
    mutable double totalCost;
    double evaluateTotalCost() const;

    OperationList opList;
    OperationList branchList;
    TupleChrysalis * initialTupleSet;
public:
    explicit PlanInfo(size_t initialTupleSetSize);
    explicit PlanInfo(const PlanInfo * parent, PlanDesc _desc);

    TupleRef initTupleSet(DataNode * node);

    double getTotalCost() const {
        if (totalCost == 0) {
            totalCost = evaluateTotalCost();
        };

        return totalCost;
    };

    POProtIn getRef(TupleId i) const {
        return POProtIn(branchList.at(i), i);
    };

    void updateBranch(POProt * op);

    PlanDesc getDesc() const { return desc; };
    PlanInfo * extend(Predicate * what) const;
    phop::ITupleOperator * compile();

    virtual XmlConstructor & toXML(XmlConstructor& constructor) const;
};

struct Candidate;

class PhysicalModel {
    POProtIn doMaterialize(TupleId t, bool addToTree);
public:
    PlanInfo * plan;
    DataGraph * dg;
    POProt * result;
    
    PhysicalModel(PlanInfo * _plan) : plan(_plan), dg(NULL), result(NULL) {};
    
    TupleRef initialRef(TupleId t) const { return TupleRef(plan->initialTupleSet, t); };
    
    POProtIn materialize(const POProtIn& tref) {
        if (tref.op == NULL) {
            return doMaterialize(tref.index, true);
        } else {
            return tref;
        };
    };

    inline void pushOp(POProt * op)
    {
        plan->updateBranch(op);
        plan->opList.push_back(op);
    }
    
    TupleChrysalis * updateOne(TupleChrysalis* parent, const POProtIn& op) const;
    TupleChrysalis * updateTwo(TupleChrysalis* x, TupleChrysalis* y, POProt* op, TupleId ind1, TupleId ind2) const;

    void * compile(ValuePredicate * pred);
    void * compile(StructuralPredicate * pred);
};

}

#endif /* PHYSICAL_MODEL_H */
