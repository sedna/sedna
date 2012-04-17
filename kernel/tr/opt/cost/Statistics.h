/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef COST_MODEL_H
#define COST_MODEL_H

#include "tr/opt/OptTypes.h"
#include "tr/cat/catptr.h"
#include "tr/executor/xpath/XPathTypes.h"

class POProt;

struct BaseOperationCosts {
    double valueSortCost;
    double textSize;
};

struct TupleStatistics : public IPlanDisposable {
    Range nodeCount;
    Range schemaNodeCount;

    void * values;
    void * baseOperationCosts;
};

struct Statistics : public IPlanDisposable {
    TupleStatistics * self;
};

struct OperationCost : public IPlanDisposable {
    Range firstCost;
    Range nextCost;
    Range fullCost;
};

class CostModel {
public:
    void evaluateBaseStatistics(SchemeElement * stats);
    Range getBaseSortCost(Statistics * stats);
    Range getPathSelectivity(SchemeElement * base, const pe::Path & path);
};

extern CostModel * publicCostModel;

// #define GET_COST(FN)
// #define PROFILE(OP)
// #define PROFILABLE(FN)

// #define FN_PROFILABLE_DECL(FN)

#endif /* COST_MODEL_H */
