/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef PHYSICAL_MODEL_H
#define PHYSICAL_MODEL_H

#include "common/sedna.h"
#include "tr/opt/OptTypes.h"

class IElementProducer;
class PPIterator;

class PlanInfo;
class CostModel;

struct TupleStatistics;
struct OperationCost;
struct VPredicate;
struct SPredicate;

class POProt;

namespace phop {
    class IOperator;
}

struct TupleValueInfo {
    enum element_status_t {
        empty = 0,
        available,
        evaluated,
    };

    element_status_t status;
    TupleStatistics * statistics;

    enum order_t {
        none = 0, node_unique, node_ddo, value
    };
    
    order_t order;
    
    DataNode * node;
    POProt * _gen;
};

/* This structure describe tuple the raw */
class TupleChrysalis : public IPlanDisposable {
public:
    explicit TupleChrysalis(size_t size);
    explicit TupleChrysalis(const TupleChrysalis * parent);

    std::vector<TupleValueInfo> tuples;
    std::vector<unsigned> sortOrder;

    unsigned _width;

    Range rowSize;
    Range rowCount;

    unsigned width() const { return _width; };

    TupleValueInfo * get(TupleId i) { return &(tuples.at(i)); };
};

class POProtIn { public:
    POProt * op;
    TupleId index;

    POProtIn() : op(NULL), index(0) {};
    POProtIn(POProt * _op, const TupleId _index) : op(_op), index(_index) { };
    POProtIn(const POProtIn& _other) : op(_other.op), index(_other.index) { };
};

struct prot_info_t
{
    const char * name;
};

class POProt : public IPlanDisposable {
    const prot_info_t * protInfo;
protected:
    OperationCost * cost;
    IElementProducer * __commonToXML(IElementProducer *) const;
    virtual IElementProducer * __toXML(IElementProducer *) const;
public:
    std::vector<POProtIn> in;
    TupleChrysalis * result;
    std::vector<int> resultSet;

    const prot_info_t * getProtInfo() const { return protInfo; };

    OperationCost * getCost() {
        if (cost == NULL) { evaluateCost(NULL); }
        return cost;
    }

    POProt(const prot_info_t * pinfo) : protInfo(pinfo), cost(NULL), result(NULL) {};

    virtual void evaluateCost(CostModel * model) = 0;
    virtual phop::IOperator * compile() = 0;
    virtual IElementProducer * toXML(IElementProducer *) const;
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

    TupleRef & operator=(const TupleRef & _x) { if (this != &_x) { tupleDesc = _x.tupleDesc; tid = _x.tid; }; return *this; };

    TupleValueInfo * get() const { return tupleDesc->get(tid); }
    TupleValueInfo & operator*() const { return *tupleDesc->get(tid); };
    TupleValueInfo * operator->() const { return tupleDesc->get(tid); }
};

class PlanInfo : public IPlanDisposable {
friend class PhysicalModel;
protected:
    typedef std::vector<POProt *> OperationList;

    PlanDesc desc, parent;
    OperationList opList;
    mutable double totalCost;
    OperationList branchList;
    TupleChrysalis * initialTupleSet;

    double evaluateTotalCost() const;
public:
    PlanInfo(size_t initialTupleSetSize);
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

    PlanDesc getDesc() const { return desc; };
    PlanInfo * extend(Predicate * what) const;
    PPIterator * compile();

    IElementProducer * toXML(IElementProducer *) const;
};

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
    
    void updateBranch(POProt * op);
    
    TupleChrysalis * updateOne(TupleChrysalis* parent, const POProtIn& op);
    TupleChrysalis * updateTwo(TupleChrysalis* x, TupleChrysalis* y, POProt* op, TupleId ind1, TupleId ind2);

    void * compile(VPredicate * pred);
    void * compile(SPredicate * pred);
};

#endif /* PHYSICAL_MODEL_H */
