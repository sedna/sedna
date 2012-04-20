/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef COST_MODEL_H
#define COST_MODEL_H

#include "tr/opt/OptTypes.h"
#include "tr/cat/catptr.h"
#include "tr/executor/xpath/XPathTypes.h"
#include "tr/opt/alg/Predicates.h"

class POProt;
class TupleRef;

struct PathCostModel : public IPlanDisposable {
    IPlanDisposable * data;

    Range card;
    Range blockCount;

    Range nidSize;
    Range iterationCost;
    Range schemaTraverseCost;
};

struct ValueCostModel : public IPlanDisposable {
    IPlanDisposable * data;

    Range size;
    Range atomizationCost;
};

struct SequenceInfo : public IPlanDisposable {
    Range card;
    Range blockCount;
    Range sortCost;
};

struct ComparisonInfo : public IPlanDisposable {
    Range opCost;
    Range selectivity;
};

struct TupleStatistics : public IPlanDisposable {
    Range distinctValues;
    Range valueSize;

    PathCostModel * pathInfo;
    ValueCostModel * valueInfo;

    TupleStatistics() : distinctValues(0), valueSize(0), pathInfo(NULL), valueInfo(NULL) {}

    explicit TupleStatistics(const TupleStatistics * _x)
        : distinctValues(_x->distinctValues), valueSize(_x->valueSize), pathInfo(_x->pathInfo), valueInfo(_x->valueInfo) {};
};

struct OperationCost : public IPlanDisposable {
    Range firstCost;
    Range nextCost;
    Range fullCost;
};

extern const double C_IO_Cost;
extern const double C_CPU_Cost;

class CostModel {
public:
    PathCostModel * getAbsPathCost(const DataRoot& root, const pe::Path & path, TupleStatistics * result);
    PathCostModel * getPathCost(const TupleRef & base, const pe::Path & path, TupleStatistics * result);
    ValueCostModel * getValueCost(PathCostModel * m, TupleStatistics * result);

    SequenceInfo * getDocOrderSequenceCost(const TupleRef & tuple);
    SequenceInfo * getValueSequenceCost(const TupleRef & tuple);

    double getIOCost() const { return C_IO_Cost; };
    double getCPUCost() const { return C_CPU_Cost; };
    double getNodeSize() const { return sizeof(tuple_cell); };

    TupleStatistics* getConstInfo(const MemoryTupleSequence* cnst);

    ComparisonInfo * getCmpInfo(TupleStatistics* m1, TupleStatistics* m2, const Comparison& cmp);
    ComparisonInfo * getDocOrderInfo(PathCostModel* m1, PathCostModel* m2, const pe::Path & path);
    
//    void startProfile();
//    void endProfile();
};

extern CostModel * publicCostModel;

#endif /* COST_MODEL_H */
