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

struct OperationCost;
struct VPredicate;
struct SPredicate;

struct prot_info_t
{
    const char * name;
};

class POProt : public IPlanDisposable {
    const prot_info_t * protInfo;
public:
    OperationCost * cost;

    SchemeElement * leftElement;
    SchemeElement * rightElement;

    const prot_info_t * getProtInfo() const { return protInfo; };
    
    POProt(const prot_info_t * pinfo) : protInfo(pinfo), cost(NULL), leftElement(NULL), rightElement(NULL) {};

    virtual PPIterator * compile() = 0;
    virtual IElementProducer * toXML(IElementProducer *) const = 0;
};

class PhysicalModel {
public:
    PlanInfo * plan;
    DataGraph * dg;
    POProt * result;

    PhysicalModel(PlanInfo * _plan) : plan(_plan), dg(NULL), result(NULL) {};

    SchemeElement * materialize(SchemeElement * el);

    void * compile(VPredicate * pred);
    void * compile(SPredicate * pred);
};

class PlanInfo : public IPlanDisposable {
friend class PhysicalModel;
protected:
    mutable double totalCost;

    typedef std::map<int, SchemeElement *> PlanTupleSchemeMap;
    PlanTupleSchemeMap scheme;

    typedef std::vector<POProt *> JoinTree;
    JoinTree joinTree;
    PlanDesc desc;

    double evaluateTotalCost() const;
public:
    PlanInfo();
    explicit PlanInfo(const PlanInfo * parent);
  
    SchemeElement* initSchemeElement(DataNode * node);

    double getTotalCost() const {
        if (totalCost == 0) {
            totalCost = evaluateTotalCost();
        };

        return totalCost;
    };

    int updateScheme(int index, SchemeElement * el);
    
    PlanDesc getDesc() const { return desc; };

    PlanInfo * extend(Predicate * what) const;

    PPIterator * compile();

    IElementProducer * toXML(IElementProducer *) const;
};

#endif /* PHYSICAL_MODEL_H */
