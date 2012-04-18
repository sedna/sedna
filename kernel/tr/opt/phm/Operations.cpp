#include "Operations.h"
#include "tr/opt/cost/Statistics.h"

double blockSize = 64*1000;

#define OPINFO(OP) static const prot_info_t OP##_info = {#OP, };
#define OPREF(OP) (&(OP##_info))

OPINFO(AbsPathScanPrototype)
OPINFO(PathEvaluationPrototype)
OPINFO(SortMergeJoinPrototype)
OPINFO(StructuralJoinPrototype)

AbsPathScanPrototype::AbsPathScanPrototype(PhysicalModel* model, const TupleRef& tref)
  : POProt(OPREF(AbsPathScanPrototype)) 
{
    result = model->updateOne(tref.tupleDesc, POProtIn(this, tref.tid));
}

PathEvaluationPrototype::PathEvaluationPrototype(PhysicalModel* model, const POProtIn& _left, const TupleRef& _right, const pe::Path& _path)
  : POProt(OPREF(PathEvaluationPrototype)), path(_path), result(_right)
{
    in.push_back(_left);
    result = model->updateOne(_left.op->result, POProtIn(this, _right.tid));
}

SortMergeJoinPrototype::SortMergeJoinPrototype(PhysicalModel* model, const POProtIn& _left, const POProtIn& _right, const Comparison& _cmp)
  : BinaryOpPrototype(OPREF(SortMergeJoinPrototype), _left, _right), cmp(_cmp)
{
    result = model->updateTwo(_left.op->result, POProtIn(this, _left.index), POProtIn(this, _right.index));
}

StructuralJoinPrototype::StructuralJoinPrototype(PhysicalModel* model, const POProtIn& _left, const POProtIn& _right, const pe::Path& _path)
  : BinaryOpPrototype(OPREF(StructuralJoinPrototype), _left, _right), path(_path)
{

}


/*
SortMergeJoinPrototype::SortMergeJoinPrototype(SchemeElement* _left, SchemeElement* _right, const Comparison& _cmp)
 : leftIn(_left), rightIn(_right), cmp(_cmp)
{
    U_ASSERT(_left->evaluated);
    U_ASSERT(_right->evaluated);
  
    leftElement = new SchemeElement(*_left);
    rightElement = new SchemeElement(*_right);
    cost = new OperationCost();

    leftElement->pop = POProtIn();

    cost->firstCost =
      _left->pop.op->cost->fullCost +
      _right->pop.op->cost->fullCost +
      publicCostModel->getBaseSortCost(_left->statistics) +
      publicCostModel->getBaseSortCost(_right->statistics);

    cost->nextCost = Range(1.0, 1.0);
    cost->fullCost = cost->firstCost +
      cost->nextCost *
        (_left->statistics->self->nodeCount + _right->statistics->self->nodeCount);
}

AbsPathScanPrototype::AbsPathScanPrototype(SchemeElement* _in, const pe::Path& _path, const DataRoot& _dataRoot)
  : PathExpressionPrototype(_path), dataRoot(_dataRoot)
{
    leftElement = NULL;
    rightElement = _in;

    cost = new OperationCost();

    publicCostModel->evaluateBaseStatistics(_in);
    cost->firstCost = _in->statistics->self->schemaNodeCount;
    cost->nextCost = Range(1.0, 1.0);
    cost->fullCost = cost->firstCost + cost->nextCost * _in->statistics->self->nodeCount;
}


AxisStepPrototype::AxisStepPrototype(SchemeElement* _in, SchemeElement* _out, const pe::Path& _path)
  : PathExpressionPrototype(_path)
{
    U_ASSERT(_in->evaluated);

    leftElement = _in;
    rightElement = new SchemeElement(*_out);

    cost = new OperationCost();

    cost->firstCost = _in->pop.op->cost->firstCost;
    cost->nextCost = _in->pop.op->cost->nextCost + _path.getBody()->size();
    cost->fullCost = cost->nextCost * leftElement->statistics->self->nodeCount;

    U_ASSERT(false); // TODO//
//    rightElement->statistics = publicCostModel->getPathSelectivity(_in, _path);
}

PathExpressionPrototype::PathExpressionPrototype(const pe::Path& _path)
  : POProt(), path(_path)
 { }



StructuralSortMergeJoinPrototype::StructuralSortMergeJoinPrototype(SchemeElement* _left, SchemeElement* _right, const pe::Path& _path)
  : PathExpressionPrototype(_path)
{
    U_ASSERT(_left->evaluated);
    U_ASSERT(_right->evaluated);

    leftElement = new SchemeElement(*_left);
    rightElement = new SchemeElement(*_right);
    cost = new OperationCost();

    leftElement->pop = POProtIn();

    cost->firstCost =
      _left->pop.op->cost->fullCost +
      _right->pop.op->cost->fullCost +
      publicCostModel->getBaseSortCost(_left->statistics) +
      publicCostModel->getBaseSortCost(_right->statistics);

    cost->nextCost = Range(1.0, 1.0);
    cost->fullCost = cost->firstCost +
      cost->nextCost *
        (_left->statistics->self->nodeCount + _right->statistics->self->nodeCount);
}



PPIterator* AbsPathScanPrototype::compile()
{
    return PathExpressionPrototype::compile();
}

PPIterator* AxisStepPrototype::compile()
{
    return PathExpressionPrototype::compile();
}

PPIterator* PathExpressionPrototype::compile()
{
    return NULL;
}

PPIterator* SortMergeJoinPrototype::compile()
{
    return NULL;
}

PPIterator* StructuralSortMergeJoinPrototype::compile()
{
    return PathExpressionPrototype::compile();
}

*/