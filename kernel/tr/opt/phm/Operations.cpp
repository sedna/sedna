#include "Operations.h"
#include "tr/opt/cost/Statistics.h"
#include "tr/structures/producer.h"
#include "tr/executor/xpath/XPathExecution.h"
#include "tr/executor/algorithms/SequenceModel.h"
#include "tr/executor/algorithms/Scans.h"
#include "tr/executor/algorithms/Joins.h"
#include "tr/executor/xpath/XPathLookup.h"

#define OPINFO(OP) static const prot_info_t OP##_info = {#OP, };
#define OPREF(OP) (&(OP##_info))
#define CDGQNAME(N) xsd::QName::getConstantQName(NULL_XMLNS, N)

OPINFO(AbsPathScanPrototype)
OPINFO(PathEvaluationPrototype)
OPINFO(SortMergeJoinPrototype)
OPINFO(StructuralJoinPrototype)
OPINFO(ValueScanPrototype)
OPINFO(ValidatePathPrototype)

IElementProducer* elementValue(IElementProducer* element, const char * name, tuple_cell value)
{
    element = element->addElement(CDGQNAME(name));
    element->addText(text_source_tuple_cell(value));
    element->close();

    return element;
}

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

StructuralJoinPrototype::StructuralJoinPrototype(PhysicalModel* model, const POProtIn& _left, const POProtIn& _right, const pe::Path& _path)
  : BinaryOpPrototype(OPREF(StructuralJoinPrototype), _left, _right), path(_path)
{
    result = model->updateTwo(_left.op->result, _right.op->result, this, _left.index, _right.index);
    resultSet.push_back(_left.index);
    resultSet.push_back(_right.index);

    evaluateCost(publicCostModel);
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
    in.push_back(_left);
  
    result = model->updateOne(_left.op->result, POProtIn(this, _left.index));
    resultSet.push_back(_left.index);

    evaluateCost(publicCostModel);
}

ValidatePathPrototype::ValidatePathPrototype(PhysicalModel* model, const POProtIn& _tuple)
  : POProt(OPREF(ValidatePathPrototype))
{
    in.push_back(_tuple);
    
    result = model->updateOne(_tuple.op->result, POProtIn(this, _tuple.index));
    resultSet.push_back(_tuple.index);

    DataNode * dn = _tuple.op->result->get(_tuple.index)->node;

    if (dn != NULL) {
        path = dn->path;
        dataRoot = dn->root;
    }

    evaluateCost(publicCostModel);
}

IElementProducer* ValidatePathPrototype::__toXML(IElementProducer* element) const
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


/*

MagicJoinPrototype::MagicJoinPrototype(PhysicalModel* model, const POProtIn& _left, const POProtIn& _right, const pe::Path& _path)
  : BinaryOpPrototype(model, _left, _right), path(_path)
{
    U_ASSERT(_left.op == NULL || _right.op == NULL);
  
    result = model->updateTwo(_left.op->result, _right.op->result, this, _left.index, _right.index);
    resultSet.push_back(_left.index);
    resultSet.push_back(_right.index);

    evaluateCost(publicCostModel);
}

IElementProducer* MagicJoinPrototype::__toXML(IElementProducer* ) const
{
    POProt::__toXML(element);

    IElementProducer * child = element->addElement(CDGQNAME("path"));
    child->addText(text_source_cstr(path.toXPathString().c_str()));
    child->close();

    return element;
}
*/

struct XLogXOp { double operator() (double x) { return x*log(1+x); } };

void AbsPathScanPrototype::evaluateCost(CostModel* model)
{
    TupleStatistics * stats = new TupleStatistics();
    result->get(resultSet.at(0))->statistics = stats;
    PathCostModel * costInfo = model->getAbsPathCost(dataRoot, path, stats);

    result->rowCount = stats->distinctValues;
    result->rowSize = model->getNodeSize();

    cost = new OperationCost();

    cost->firstCost = costInfo->schemaTraverseCost;

    Range evalCost = costInfo->blockCount * model->getIOCost() + costInfo->card * model->getCPUCost();

    wantSort = true;
    Range heapSortCost = (wantSort ? (costInfo->card.map<XLogXOp>() * model->getCPUCost()) : Range(0.0));

    if (stats->distinctValues.upper == 0) {
        cost->nextCost = 0;
    } else if (stats->distinctValues.lower == 0) {
        cost->nextCost = (evalCost + heapSortCost) / stats->distinctValues.upper;
    } else {
        cost->nextCost = (evalCost + heapSortCost) / stats->distinctValues;
    }

    U_ASSERT(cost->nextCost.lower > 0);
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
        in.at(1).op->getCost()->fullCost;

    // TODO if sorted, update sorted information
    cost->firstCost +=
        leftSeq->sortCost + rightSeq->sortCost;

    Range mergeCost =
        leftSeq->card * cmpInfo->opCost +
        rightSeq->card * cmpInfo->opCost +
        leftSeq->blockCount * model->getIOCost() +
        rightSeq->blockCount * model->getIOCost();

    cost->nextCost = mergeCost / result->rowCount;
    cost->fullCost = cost->firstCost + mergeCost;

    // Update sort information
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
    const OperationCost * inCost = in.at(0).op->getCost();

    U_ASSERT(inRef->statistics);

    cost = new OperationCost();

    outRef->statistics = new TupleStatistics();
    model->getPathCost(inRef, path, outRef->statistics);

    result->rowCount = outRef->statistics->distinctValues;
    result->rowSize = inRef.tupleDesc->rowSize + model->getNodeSize();

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

    // This operation may be implemented in two different ways with completely different costs
    
    Range bestFirstCost = in.at(0).op->getCost()->firstCost + in.at(1).op->getCost()->firstCost;
    Range worseFirstCost = in.at(0).op->getCost()->fullCost + in.at(1).op->getCost()->fullCost +
        leftSeq->sortCost + rightSeq->sortCost;

    Range bestMergeCost = cmpInfo->opCost * std::max(leftIn.tupleDesc->rowCount, rightIn.tupleDesc->rowCount);

    Range worseMergeCost =
        leftSeq->card * cmpInfo->opCost +
        rightSeq->card * cmpInfo->opCost +
        leftSeq->blockCount * model->getIOCost() +
        rightSeq->blockCount * model->getIOCost();

    Range mergeCost = Range(bestMergeCost.lower, worseMergeCost.upper).normalize();
        
    cost->firstCost = Range(bestFirstCost.lower, worseFirstCost.upper).normalize();
    cost->nextCost = mergeCost / result->rowCount;
    cost->fullCost = cost->firstCost + mergeCost;
}

void ValidatePathPrototype::evaluateCost(CostModel* model)
{
    POProt * opIn = in.at(0).op;

    cost = new OperationCost();
    *cost = *opIn->getCost();

    if (!path.getBody().isnull()) {
        cost->nextCost += model->getAbsPathCost(dataRoot, path, NULL)->iterationCost;
    }
}












struct ExecutionBlockWarden {
    ExecutionBlockWarden(POProt * opin)
    {
        phop::ExecutionBlock::current()->source = opin;
    };
    
    ~ExecutionBlockWarden()
    {
        phop::ExecutionBlock::current()->source = NULL;
    };
};

phop::IOperator * AbsPathScanPrototype::compile()
{
    ExecutionBlockWarden(this);

    SchemaNodePtrSet schemaNodes;
    phop::TupleList inTuples;

    executeSchemaPathTest(dataRoot.getSchemaNode(), pe::AtomizedPath(path.getBody()->begin(), path.getBody()->end()), &schemaNodes, false);

    for (SchemaNodePtrSet::const_iterator it = schemaNodes.begin(); it != schemaNodes.end(); ++it) {
        inTuples.push_back(
            phop::MappedTupleIn(
                new phop::TupleFromItemOperator(
                    new phop::SchemaScan(schema_node_cptr(*it))), 0, 0));
    };

    return new phop::DocOrderMerge(1, inTuples);
}

phop::IOperator * PathEvaluationPrototype::compile()
{
    ExecutionBlockWarden(this);
    // TODO : make effective evaluation

    phop::ITupleOperator * opin = dynamic_cast<phop::ITupleOperator *>(in.at(0).op->compile());
    phop::TupleIn aopin(opin, in.at(0).index);

    U_ASSERT(opin != NULL);

    phop::IValueOperator * ain = new phop::ReduceToItemOperator(aopin);

    pe::PathVectorPtr pathBody = path.getBody();
    pe::PathVector::const_iterator it = pathBody->begin();

    while (it != pathBody->end()) {
        pe::PathVector::const_iterator pstart = it;
        
        while (it != pathBody->end() && it->satisfies(pe::StepPredicate(pe::ParentAxisTest))) {
            ++it;
        };

        if (pstart != it) {
            ain = new pe::PathEvaluateTraverse(ain, pe::AtomizedPath(pstart, it));
        }

        pe::PathVector::const_iterator cstart = it;

        while (it != pathBody->end() &&
                it->satisfies(pe::StepPredicate(pe::ChildAxisTest))) {
            ++it;
        };

        if (cstart != it) {
            ain = new pe::PathSchemaResolve(ain, pe::AtomizedPath(cstart, it));
        }

        if (pstart == it) {
            U_ASSERT(false);
            break;
        };
        
        ++it;
    };

    ain = new pe::PathEvaluateTraverse(ain, pe::AtomizedPath(it, pathBody->end()));

    return new phop::NestedEvaluation(aopin, ain);
}

phop::IOperator * SortMergeJoinPrototype::compile()
{
    ExecutionBlockWarden(this);

    return NULL;
}

phop::IOperator * StructuralJoinPrototype::compile()
{
    ExecutionBlockWarden(this);

    return NULL;
}

phop::IOperator * ValueScanPrototype::compile()
{
    ExecutionBlockWarden(this);

    return NULL;
}

phop::IOperator * ValidatePathPrototype::compile()
{
    ExecutionBlockWarden(this);

    return NULL;
}
