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

struct Statistics;
struct OperationCost;
struct VPredicate;
struct SPredicate;

class POProt;

struct ElementDescriptor {
    enum element_status_t {
        empty = 0,
        available,
        evaluated,
    };

    element_status_t status;
    Statistics * statistics;

#ifndef EL_DEBUG 
    union {
#endif /* EL_DEBUG */
        DataNode * node;
        POProt * _gen;
#ifndef EL_DEBUG
    };
#endif /* EL_DEBUG */
};

/* This structure describe tuple the raw */
class TupleChrysalis : public IPlanDisposable {
public:
    explicit TupleChrysalis(size_t size);
    explicit TupleChrysalis(const TupleChrysalis * parent);

    std::vector<ElementDescriptor> tuples;
    
    Range rowSize;
    Range rowCount;

    uint64_t availableTupleMask;
    size_t size() const { tuples.size(); };
    ElementDescriptor * get(TupleId i) const { return tuples.at(i); };
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
    IElementProducer * __commonToXML(IElementProducer *) const;
public:
    OperationCost * cost;

    std::vector<POProtIn> in;
    TupleChrysalis * result;

    const prot_info_t * getProtInfo() const { return protInfo; };
    
    POProt(const prot_info_t * pinfo) : protInfo(pinfo), cost(NULL), result(NULL) {};

    virtual PPIterator * compile() = 0;
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

    ElementDescriptor & operator*() const { return *tupleDesc->get(tid); };
    ElementDescriptor * operator->() const { return tupleDesc->get(tid); }
};

class PhysicalModel {
    POProtIn doMaterialize(TupleId t);
public:
    PlanInfo * plan;
    DataGraph * dg;
    POProt * result;

    PhysicalModel(PlanInfo * _plan) : plan(_plan), dg(NULL), result(NULL) {};

    TupleRef initialRef(TupleId t) const { return TupleRef(plan->initialTupleSet, t); };
    
    POProtIn materialize(const POProtIn& tref) {
        if (tref.op == NULL) {
            return doMaterialize(tref.index);
        } else {
            return tref;
        };
    };

    void updateBranch(const POProt * op);
    
    TupleRef updateOne(const TupleChrysalis * parent, const POProtIn & op);
    TupleRef updateTwo(const TupleChrysalis * x, const TupleChrysalis * y, POProt * op, TupleId ind1, TupleId ind2);

    void * compile(VPredicate * pred);
    void * compile(SPredicate * pred);
};

class PlanInfo : public IPlanDisposable {
friend class PhysicalModel;
protected:
    typedef std::vector<POProt *> OperationList;

    PlanDesc desc;
    OperationList opList;
    mutable double totalCost;
    OperationList branchList;
    TupleChrysalis * initialTupleSet;

    double evaluateTotalCost() const;
public:
    PlanInfo(size_t initialTupleSetSize);
    explicit PlanInfo(const PlanInfo * parent);
  
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

#endif /* PHYSICAL_MODEL_H */
