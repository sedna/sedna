/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef PHYSICAL_MODEL_H
#define PHYSICAL_MODEL_H

#include "common/sedna.h"
#include "tr/opt/OptTypes.h"

struct OperationCost;
class PPIterator;

class POProt : public IPlanDisposable {
public:
    OperationCost * cost;

    SchemeElement * leftElement;
    SchemeElement * rightElement;

    virtual PPIterator * compile() = 0;
};

struct VPredicate;
struct SPredicate;

class PlanInfo;

class PhysicalModel {
public:
    PlanInfo * plan;
    DataGraph * dg;
    POProt * result;

    void * compile(VPredicate * pred);
    void * compile(SPredicate * pred);
};

class PlanTupleScheme : public IPlanDisposable {
private:
    typedef std::map<int, SchemeElement *> PlanTupleSchemeMap;
    PlanTupleSchemeMap content;
public:
    PlanTupleScheme() {};
    PlanTupleScheme(const PlanTupleScheme & scheme) : content(scheme.content) {};
    virtual ~PlanTupleScheme() {};

    SchemeElement * get(int index) const { return content.at(index); };
    int update(int index, SchemeElement * el);
};

class PlanInfo : public IPlanDisposable {
friend class PhysicalModel;
protected:
    mutable double totalCost;

    PlanTupleScheme * schema;

    typedef std::vector<POProt *> JoinTree;
    JoinTree joinTree;
    PlanDesc desc;

    double evaluateTotalCost() const;
public:
    SchemeElement * materialize(SchemeElement *);
    static SchemeElement * initSchemeElement(DataNode * node);
    static PlanInfo * evaluateInitialPlanInfo(PlanTupleScheme * scheme, Predicate * pred, PlanDesc desc);

    double getTotalCost() const {
        if (totalCost == 0) {
            totalCost = evaluateTotalCost();
        };

        return totalCost;
    };

    PlanDesc getDesc() const { return desc; };
    PlanInfo * clone() const;
    PlanInfo * apply(Predicate * what);
    PPIterator * compile();
};

#endif /* PHYSICAL_MODEL_H */
