#include "Operations.h"
#include "tr/opt/cost/Statistics.h"

double blockSize = 64*1000;

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

