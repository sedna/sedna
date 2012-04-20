#include "Operations.h"
#include "tr/opt/cost/Statistics.h"
#include "tr/structures/producer.h"

#define OPINFO(OP) static const prot_info_t OP##_info = {#OP, };
#define OPREF(OP) (&(OP##_info))
#define CDGQNAME(N) xsd::QName::getConstantQName(NULL_XMLNS, N)

OPINFO(AbsPathScanPrototype)
OPINFO(PathEvaluationPrototype)
OPINFO(SortMergeJoinPrototype)
OPINFO(StructuralJoinPrototype)
OPINFO(ValueScanPrototype)

AbsPathScanPrototype::AbsPathScanPrototype(PhysicalModel* model, const TupleRef& tref)
  : POProt(OPREF(AbsPathScanPrototype)), dataRoot(), path()
{
    U_ASSERT(tref->node->type == DataNode::dnDatabase);

    dataRoot = tref->node->root;
    path = tref->node->path;

    result = model->updateOne(tref.tupleDesc, POProtIn(this, tref.tid));
    resultSet.push_back(tref.tid);

    evaluateCost(publicCostModel);
}

PPIterator* AbsPathScanPrototype::compile()
{

}

IElementProducer* AbsPathScanPrototype::__toXML(IElementProducer* element) const
{
    POProt::__toXML(element);

    IElementProducer * child;

    child = element->addElement(CDGQNAME("root"));
    child->addText(text_source_cstr(dataRoot.toLRString().c_str()));
    child->close();

    child = element->addElement(CDGQNAME("path"));
    child->addText(text_source_cstr(path.toXPathString().c_str()));
    child->close();

    return element;
}


PathEvaluationPrototype::PathEvaluationPrototype(PhysicalModel* model, const POProtIn& _left, const TupleRef& _right, const pe::Path& _path)
  : POProt(OPREF(PathEvaluationPrototype)), path(_path)
{
    in.push_back(_left);
    result = model->updateOne(_left.op->result, POProtIn(this, _right.tid));
    resultSet.push_back(_right.tid);

    evaluateCost(publicCostModel);
}

PPIterator* PathEvaluationPrototype::compile()
{
    
}

IElementProducer* PathEvaluationPrototype::__toXML(IElementProducer* element) const
{
    POProt::__toXML(element);

    IElementProducer * child = element->addElement(CDGQNAME("path"));
    child->addText(text_source_cstr(path.toXPathString().c_str()));
    child->close();

    return element;
}

SortMergeJoinPrototype::SortMergeJoinPrototype(PhysicalModel* model, const POProtIn& _left, const POProtIn& _right, const Comparison& _cmp)
  : BinaryOpPrototype(OPREF(SortMergeJoinPrototype), _left, _right), cmp(_cmp)
{
    result = model->updateTwo(_left.op->result, _right.op->result, this, _left.index, _right.index);
    resultSet.push_back(_left.index);
    resultSet.push_back(_right.index);

    evaluateCost(publicCostModel);
}

PPIterator* SortMergeJoinPrototype::compile()
{

}

StructuralJoinPrototype::StructuralJoinPrototype(PhysicalModel* model, const POProtIn& _left, const POProtIn& _right, const pe::Path& _path)
  : BinaryOpPrototype(OPREF(StructuralJoinPrototype), _left, _right), path(_path)
{
    result = model->updateTwo(_left.op->result, _right.op->result, this, _left.index, _right.index);
    resultSet.push_back(_left.index);
    resultSet.push_back(_right.index);

    evaluateCost(publicCostModel);
}

PPIterator* StructuralJoinPrototype::compile()
{

}

IElementProducer* StructuralJoinPrototype::__toXML(IElementProducer* element) const
{
    POProt::__toXML(element);

    IElementProducer * child = element->addElement(CDGQNAME("path"));
    child->addText(text_source_cstr(path.toXPathString().c_str()));
    child->close();

    return element;
}


ValueScanPrototype::ValueScanPrototype(PhysicalModel* model, const POProtIn& _left, const TupleRef& _right, const Comparison& _cmp)
  : POProt(OPREF(ValueScanPrototype)), cmp(_cmp)
{
    result = model->updateOne(_left.op->result, POProtIn(this, _left.index));
    resultSet.push_back(_left.index);

    evaluateCost(publicCostModel);
}

PPIterator* ValueScanPrototype::compile()
{

    
}










struct XLogXOp { double operator() (double x) { return x*log(x); } };

void AbsPathScanPrototype::evaluateCost(CostModel* model)
{
    TupleStatistics * stats = new TupleStatistics();
    result->get(resultSet.at(0))->statistics = stats;
    PathCostModel * costInfo = model->getAbsPathCost(dataRoot, path, stats);

    result->rowCount = stats->distinctValues;
    result->rowSize = 1;

    cost = new OperationCost();

    cost->firstCost = costInfo->schemaTraverseCost;

    Range evalCost = costInfo->blockCount * model->getIOCost() + costInfo->card * model->getCPUCost();
    Range heapSortCost = costInfo->card.map<XLogXOp>() * model->getCPUCost();

    cost->nextCost = (evalCost + heapSortCost) / stats->distinctValues;
    cost->fullCost = cost->firstCost + evalCost + heapSortCost;
}

void SortMergeJoinPrototype::evaluateCost(CostModel* model)
{
    TupleRef leftIn(in[0], NULL), rightIn(in[1], NULL);
    TupleRef leftResult(result, resultSet[0]), rightResult(result, resultSet[1]);

    if (leftIn->statistics == NULL || rightIn->statistics == NULL) {
        U_ASSERT(false);
        return;
    };

    cost = new OperationCost();

    model->getValueCost(leftIn->statistics->pathInfo, leftIn->statistics);
    model->getValueCost(rightIn->statistics->pathInfo, rightIn->statistics);

    leftResult->statistics = new TupleStatistics(leftIn->statistics);
    rightResult->statistics = new TupleStatistics(rightIn->statistics);

    SequenceInfo * leftSeq = model->getValueSequenceCost(leftIn);
    SequenceInfo * rightSeq = model->getValueSequenceCost(rightIn);

    ComparisonInfo * cmpInfo = model->getCmpInfo(leftIn->statistics, rightIn->statistics, cmp);

    leftResult->statistics->distinctValues *= std::max(1.0, cmpInfo->selectivity.avg());
    rightResult->statistics->distinctValues *= std::max(1.0, cmpInfo->selectivity.avg());

    result->rowCount = std::min(leftIn.tupleDesc->rowCount, rightIn.tupleDesc->rowCount) * cmpInfo->selectivity;

    cost->firstCost =
        in.at(0).op->getCost()->fullCost +
        in.at(1).op->getCost()->fullCost +
        leftSeq->sortCost + rightSeq->sortCost;

    Range mergeCost =
        leftSeq->card * cmpInfo->opCost +
        rightSeq->card * cmpInfo->opCost +
        leftSeq->blockCount * model->getIOCost() +
        rightSeq->blockCount * model->getIOCost();

    cost->nextCost = mergeCost / result->rowCount;
    cost->fullCost = cost->firstCost + mergeCost;
}

void ValueScanPrototype::evaluateCost(CostModel* model)
{
    TupleRef inRef(in[0], NULL);
    TupleRef outRef(result, resultSet[0]);

    U_ASSERT(inRef->statistics);

    cost = new OperationCost();

    model->getValueCost(inRef->statistics->pathInfo, inRef->statistics);
    outRef->statistics = new TupleStatistics(inRef->statistics);

    ComparisonInfo * cmpInfo = model->getCmpInfo(inRef->statistics, model->getConstInfo(value.get()), cmp);

    outRef->statistics->distinctValues *= cmpInfo->selectivity;
    result->rowCount = outRef->statistics->distinctValues;

    OperationCost * inCost = in.at(0).op->getCost();

    cost->firstCost = inCost->firstCost;
    cost->fullCost  = inCost->fullCost + inRef.tupleDesc->rowCount * cmpInfo->opCost;
    cost->nextCost  = (cost->fullCost - cost->firstCost) / result->rowCount;
}

void PathEvaluationPrototype::evaluateCost(CostModel* model)
{
    TupleRef inRef(in[0], NULL);
    TupleRef outRef(result, resultSet[0]);
    OperationCost * inCost = in.at(0).op->getCost();

    U_ASSERT(inRef->statistics);

    cost = new OperationCost();

    outRef->statistics = new TupleStatistics();
    model->getPathCost(inRef, path, outRef->statistics);

    result->rowCount = outRef->statistics->distinctValues;
    result->rowSize = inRef.tupleDesc->rowSize + 1;

    cost->firstCost = inCost->firstCost;
    cost->fullCost  = inCost->fullCost + inRef->statistics->distinctValues * outRef->statistics->pathInfo->iterationCost;
    cost->nextCost  = (cost->fullCost - cost->firstCost) / result->rowCount;
}

void StructuralJoinPrototype::evaluateCost(CostModel* model)
{
    TupleRef leftIn(in[0], NULL), rightIn(in[1], NULL);
    TupleRef leftResult(result, resultSet[0]), rightResult(result, resultSet[1]);

    if (leftIn->statistics == NULL || rightIn->statistics == NULL) {
        U_ASSERT(false);
        return;
    };

    cost = new OperationCost();

    leftResult->statistics = new TupleStatistics(leftIn->statistics);
    rightResult->statistics = new TupleStatistics(rightIn->statistics);

    SequenceInfo * leftSeq = model->getDocOrderSequenceCost(leftIn);
    SequenceInfo * rightSeq = model->getDocOrderSequenceCost(rightIn);

    ComparisonInfo * cmpInfo = model->getDocOrderInfo(leftIn->statistics->pathInfo, rightIn->statistics->pathInfo, path);

    leftResult->statistics->distinctValues *= std::max(1.0, cmpInfo->selectivity.avg());
    rightResult->statistics->distinctValues *= std::max(1.0, cmpInfo->selectivity.avg());

    result->rowCount = std::min(leftIn.tupleDesc->rowCount, rightIn.tupleDesc->rowCount) * cmpInfo->selectivity;

    cost->firstCost = in.at(0).op->getCost()->fullCost + in.at(1).op->getCost()->fullCost +
        leftSeq->sortCost + rightSeq->sortCost;

    Range mergeCost =
        leftSeq->card * cmpInfo->opCost +
        rightSeq->card * cmpInfo->opCost +
        leftSeq->blockCount * model->getIOCost() +
        rightSeq->blockCount * model->getIOCost();

    cost->nextCost = mergeCost / result->rowCount;
    cost->fullCost = cost->firstCost + mergeCost;
}

