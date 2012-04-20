#ifndef _OPERATIONS_H
#define _OPERATIONS_H

#include "tr/opt/OptTypes.h"
#include "tr/opt/phm/PhysicalModel.h"
#include "tr/opt/alg/Predicates.h"

struct Statistics;

class BinaryOpPrototype : public POProt {
public:
    BinaryOpPrototype(const prot_info_t* pinfo, const POProtIn & _left, const POProtIn & _right)
      : POProt(pinfo) { in.push_back(_left); in.push_back(_right); };
};

class SortMergeJoinPrototype : public BinaryOpPrototype {
protected:
    const Comparison cmp;
public:
    SortMergeJoinPrototype(PhysicalModel * model, const POProtIn & _left, const POProtIn & _right, const Comparison& _cmp);

    virtual void evaluateCost(CostModel* model);
    virtual PPIterator* compile();
};

class StructuralJoinPrototype : public BinaryOpPrototype {
    pe::Path path;
protected:
    virtual IElementProducer* __toXML(IElementProducer* ) const;
public:
    StructuralJoinPrototype(PhysicalModel * model, const POProtIn & _left, const POProtIn & _right, const pe::Path& _path);

    virtual void evaluateCost(CostModel* model);
    virtual PPIterator* compile();
};

class AbsPathScanPrototype : public POProt {
    DataRoot dataRoot;
    pe::Path path;

    bool heapMergeTraverse;
protected:
    virtual IElementProducer* __toXML(IElementProducer* ) const;
public:
    AbsPathScanPrototype(PhysicalModel * model, const TupleRef & tref);

    virtual void evaluateCost(CostModel* model);
    virtual PPIterator* compile();
};

class PathEvaluationPrototype : public POProt {
    pe::Path path;
protected:
    virtual IElementProducer* __toXML(IElementProducer* ) const;
public:
    PathEvaluationPrototype(PhysicalModel * model, const POProtIn & _left, const TupleRef & _right, const pe::Path& _path);

    virtual void evaluateCost(CostModel* model);
    virtual PPIterator* compile();
};

class ValueScanPrototype : public POProt {
    const Comparison cmp;
    counted_ptr<MemoryTupleSequence> value;
public:
    ValueScanPrototype(PhysicalModel * model, const POProtIn & _left, const TupleRef & _right, const Comparison& _cmp);

    virtual void evaluateCost(CostModel* model);
    virtual PPIterator* compile();
};

#endif /* _OPERATIONS_H */
