#ifndef _COMPARISON_MODELS_H_
#define _COMPARISON_MODELS_H_

#include "tr/opt/OptTypes.h"
#include "tr/opt/alg/Predicates.h"
#include "tr/opt/phm/PhysicalModel.h"

#include "tr/structures/producer.h"

#include "tr/executor/algorithms/ValueFunction.h"
#include "tr/executor/algorithms/Comparison.h"

class CostModel;
struct SequenceInfo;
struct ComparisonInfo;

struct ComparisonPrototype : public IPlanDisposable {
    virtual ICollationTupleSerializer * createTupleSerializer(unsigned idx) = 0;
    virtual TupleCellComparison getTupleCellComparison() = 0;
    virtual ValueFunction getValueFunction(unsigned idxL, unsigned idxR) = 0;

    virtual ComparisonInfo * getComparisonCost(CostModel * model, TupleRef left, TupleRef right) = 0;
    virtual SequenceInfo * getSequenceCost(CostModel * model, TupleRef in) = 0;

    virtual IElementProducer* __toXML(IElementProducer* ) const = 0;
};

struct GeneralComparisonPrototype : public ComparisonPrototype {
    Comparison cmp;

    GeneralComparisonPrototype(const Comparison &_cmp) : cmp(_cmp) {};

    virtual ICollationTupleSerializer* createTupleSerializer(unsigned idx);
    virtual TupleCellComparison getTupleCellComparison();
    virtual ValueFunction getValueFunction(unsigned idxL, unsigned idxR);

    virtual ComparisonInfo * getComparisonCost(CostModel* model, TupleRef left, TupleRef right);
    virtual SequenceInfo * getSequenceCost(CostModel* model, TupleRef in);

    virtual IElementProducer* __toXML(IElementProducer* ) const;
};

struct PathComparisonPrototype : public ComparisonPrototype {
    pe::Path path;

    PathComparisonPrototype(const pe::Path & _path) : path(_path) {};

    virtual ICollationTupleSerializer* createTupleSerializer(unsigned idx);
    virtual TupleCellComparison getTupleCellComparison();
    virtual ValueFunction getValueFunction(unsigned idxL, unsigned idxR);

    virtual ComparisonInfo * getComparisonCost(CostModel* model, TupleRef left, TupleRef right);
    virtual SequenceInfo * getSequenceCost(CostModel* model, TupleRef in);

    virtual IElementProducer* __toXML(IElementProducer* ) const;
};


#endif /* _COMPARISON_MODELS_H_ */