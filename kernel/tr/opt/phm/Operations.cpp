#include "Operations.h"

#include "tr/opt/cost/Statistics.h"
#include "tr/opt/phm/ComparisonModels.h"

#include "tr/models/XmlConstructor.h"

#include "tr/opt/path/XPathExecution.h"
#include "tr/opt/SequenceModel.h"
#include "tr/opt/algorithms/Scans.h"
#include "tr/opt/algorithms/Joins.h"
#include "tr/opt/path/XPathLookup.h"
#include "tr/executor/base/ITupleSerializer.h"
#include "tr/opt/algorithms/ComparisonOperation.h"
#include "tr/opt/functions/Functions.h"
#include "tr/opt/algebra/IndependentPlan.h"

using namespace opt;

#define OPINFO(OP) static const prot_info_t OP##_info = {#OP, };
#define OPREF(OP) (&(OP##_info))
#define CDGQNAME(N) xsd::QName::getConstantQName(NULL_XMLNS, N)

OPINFO(AbsPathScanPrototype)
OPINFO(PathEvaluationPrototype)
OPINFO(MergeJoinPrototype)
OPINFO(StructuralJoinPrototype)
OPINFO(ValueScanPrototype)
OPINFO(ValidatePathPrototype)
OPINFO(EvaluatePrototype)
OPINFO(ExternalVarPrototype)

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

XmlConstructor & AbsPathScanPrototype::__toXML(XmlConstructor & element) const
{
    element.addElementValue(CDGQNAME("root"), dataRoot.toLRString());
    element.addElementValue(CDGQNAME("path"), path.toXPathString());
    return POProt::__toXML(element);
}


PathEvaluationPrototype::PathEvaluationPrototype(PhysicalModel* model, const POProtIn& _left, const TupleRef& _right, const pe::Path& _path)
  : POProt(OPREF(PathEvaluationPrototype)), path(_path)
{
    in.push_back(_left);
    result = model->updateOne(_left.op->result, POProtIn(this, _right.tid));
    resultSet.push_back(_right.tid);

    evaluateCost(publicCostModel);
}

XmlConstructor & PathEvaluationPrototype::__toXML(XmlConstructor & element) const
{
    element.addElementValue(CDGQNAME("path"), path.toXPathString());
    return POProt::__toXML(element);
}

MergeJoinPrototype::MergeJoinPrototype(PhysicalModel* model, const POProtIn& _left, const POProtIn& _right, ComparisonPrototype * _comparison)
  : BinaryOpPrototype(OPREF(MergeJoinPrototype), _left, _right), comparison(_comparison)
{
    result = model->updateTwo(_left.op->result, _right.op->result, this, _left.index, _right.index);
    resultSet.push_back(_left.index);
    resultSet.push_back(_right.index);

    if (dynamic_cast<GeneralComparisonPrototype *>(comparison) != NULL) {
        needLeftSort = true;
        needRightSort = true;
    } else {
        needLeftSort = false;
        needRightSort = false;
    };

    evaluateCost(publicCostModel);
}

XmlConstructor & MergeJoinPrototype::__toXML(XmlConstructor & element) const
{
    comparison->__toXML(element);
    return POProt::__toXML(element);
}

FilterTuplePrototype::FilterTuplePrototype(PhysicalModel* model, const POProtIn& _left, const POProtIn& _right, ComparisonPrototype* _comparison)
  : BinaryOpPrototype(OPREF(MergeJoinPrototype), _left, _right), comparison(_comparison)
{
    result = model->updateTwo(_left.op->result, _right.op->result, this, _left.index, _right.index);
    resultSet.push_back(_left.index);
    resultSet.push_back(_right.index);

    evaluateCost(publicCostModel);
}

XmlConstructor & FilterTuplePrototype::__toXML(XmlConstructor & element) const
{
    comparison->__toXML(element);
    return POProt::__toXML(element);
}

ValueScanPrototype::ValueScanPrototype(PhysicalModel* model, const POProtIn& _left, const TupleRef& _right, const Comparison& _cmp)
  : POProt(OPREF(ValueScanPrototype)), cmp(_cmp)
{
    in.push_back(_left);
  
    result = model->updateOne(_left.op->result, POProtIn(this, _left.index));
    resultSet.push_back(_left.index);

    value = _right->node->sequence;

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

XmlConstructor & ValidatePathPrototype::__toXML(XmlConstructor & element) const
{
    element.addElementValue(CDGQNAME("root"), dataRoot.toLRString());
    element.addElementValue(CDGQNAME("path"), path.toXPathString());

    return POProt::__toXML(element);
}

EvaluatePrototype::EvaluatePrototype(PhysicalModel* model, const opt::POProtIn& _left, const opt::TupleRef& _right, phop::IFunction* _func)
    : POProt(OPREF(EvaluatePrototype)), func(_func)
{
    in.push_back(_left);
    result = model->updateOne(_left.op->result, POProtIn(this, _right.tid));
    resultSet.push_back(_right.tid);

    evaluateCost(publicCostModel);
}

XmlConstructor & EvaluatePrototype::__toXML(XmlConstructor & element) const
{
//    element.addElementValue(CDGQNAME("root"), dataRoot.toLRString());
//    element.addElementValue(CDGQNAME("path"), path.toXPathString());

    return POProt::__toXML(element);
}

ExternalVarPrototype::ExternalVarPrototype(PhysicalModel* model, const TupleRef& tref)
  : POProt(OPREF(ExternalVarPrototype)), tid(invalidTupleId)
{
//    context = optimizer->context()->executor;
    tid = tref->node->varTupleId;
}

XmlConstructor& ExternalVarPrototype::__toXML(XmlConstructor& element) const
{
    element.openElement(CDGQNAME("tuple"));
    element.addAttributeValue(CDGQNAME("id"), tuple_cell::atomic_int(tid));
    element.closeElement();

    return opt::POProt::__toXML(element);
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

XmlConstructor & MagicJoinPrototype::__toXML(XmlConstructor & ) const
{
    POProt::__toXML(element);

    XmlConstructor & child = element->addElement(CDGQNAME("path"));
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

void MergeJoinPrototype::evaluateCost(CostModel* model)
{
    TupleRef leftIn(in[0], NULL), rightIn(in[1], NULL);
    TupleRef leftResult(result, resultSet[0]), rightResult(result, resultSet[1]);

    if (leftIn->statistics == NULL || rightIn->statistics == NULL) {
        U_ASSERT(false);
        return;
    };

    cost = new OperationCost();

    SequenceInfo * leftSeq = comparison->getSequenceCost(model, leftIn);
    SequenceInfo * rightSeq = comparison->getSequenceCost(model, rightIn);

    EvaluationInfo * cmpInfo = comparison->getComparisonCost(model, leftIn, rightIn); ;

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

void FilterTuplePrototype::evaluateCost(CostModel* model)
{
    TupleRef leftIn(in[0], NULL), rightIn(in[1], NULL);
    TupleRef leftResult(result, resultSet[0]), rightResult(result, resultSet[1]);

    if (leftIn->statistics == NULL || rightIn->statistics == NULL) {
        U_ASSERT(false);
        return;
    };

    cost = new OperationCost();

    EvaluationInfo * cmpInfo = comparison->getComparisonCost(model, leftIn, rightIn);

    leftResult->statistics->distinctValues *= std::max(1.0, cmpInfo->selectivity.avg());
    rightResult->statistics->distinctValues *= std::max(1.0, cmpInfo->selectivity.avg());

    result->rowCount = leftIn.tupleDesc->rowCount * cmpInfo->selectivity;

    OperationCost * initialCost = in[0].op->getCost();
    
    cost->firstCost = initialCost->firstCost;
    cost->fullCost = initialCost->fullCost + leftIn.tupleDesc->rowCount * cmpInfo->opCost;
    cost->nextCost = (cost->fullCost - cost->firstCost) / result->rowCount;

    // Update sort information
}

void ExternalVarPrototype::evaluateCost(CostModel* model)
{
    U_ASSERT(false);
}


void ValueScanPrototype::evaluateCost(CostModel* model)
{
    TupleRef inRef(in[0], NULL);
    TupleRef outRef(result, resultSet[0]);

    U_ASSERT(inRef->statistics);

    cost = new OperationCost();

    model->getValueCost(inRef->statistics->pathInfo, inRef->statistics);
    outRef->statistics = new TupleStatistics(inRef->statistics);

    EvaluationInfo * cmpInfo = model->getCmpInfo(inRef->statistics, model->getConstInfo(value.get()), cmp);

    outRef->statistics->distinctValues *= cmpInfo->selectivity;
    result->rowCount = outRef->statistics->distinctValues;

    OperationCost * inCost = in.at(0).op->getCost();

    cost->firstCost = inCost->firstCost;
    cost->fullCost  = inCost->fullCost + inRef.tupleDesc->rowCount * cmpInfo->opCost;
    cost->nextCost  = (cost->fullCost - cost->firstCost) / result->rowCount;
}

void EvaluatePrototype::evaluateCost(CostModel* model)
{
    TupleRef inRef(in[0], NULL);
    TupleRef outRef(result, resultSet[0]);

    const OperationCost * inCost = in.at(0).op->getCost();

    U_ASSERT(inRef->statistics);

    cost = new OperationCost();

    outRef->statistics = new TupleStatistics(inRef->statistics);

    result->rowCount = inRef.tupleDesc->rowCount;
    result->rowSize = inRef.tupleDesc->rowSize + model->getNodeSize();
 
    cost->firstCost = inCost->firstCost;
    cost->nextCost  = inCost->nextCost + model->getCPUCost();
    cost->fullCost  = cost->firstCost + cost->nextCost * result->rowCount;
}

void PathEvaluationPrototype::evaluateCost(CostModel* model)
{
    TupleRef inRef(in[0], NULL);
    TupleRef outRef(result, resultSet[0]);
    const OperationCost * inCost = in.at(0).op->getCost();

    U_ASSERT(inRef->statistics);

    cost = new OperationCost();

    outRef->statistics = new TupleStatistics();
    
    PathCostModel * costInfo = model->getPathCost(inRef, path, outRef->statistics);

    result->rowCount = inRef.tupleDesc->rowCount * outRef->statistics->distinctValues / inRef->statistics->distinctValues;
    result->rowSize = inRef.tupleDesc->rowSize + model->getNodeSize();

    cost->firstCost = inCost->firstCost;
    cost->fullCost  =
        inCost->fullCost + inRef->statistics->distinctValues * outRef->statistics->pathInfo->iterationCost +
        costInfo->blockCount * model->getIOCost();
    cost->nextCost  = (cost->fullCost - cost->firstCost) / result->rowCount;
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

using namespace phop;
using namespace pe;

struct GraphExecutionBlockWarden {
    GraphExecutionBlockWarden(POProt * opin)
    {
        phop::GraphExecutionBlock::current()->sourceStack.push(opin);
    };

    ~GraphExecutionBlockWarden()
    {
        phop::GraphExecutionBlock::current()->sourceStack.pop();
    };
};

IOperator* ExternalVarPrototype::compile()
{
    U_ASSERT(false);
}


phop::IOperator * AbsPathScanPrototype::compile()
{
    GraphExecutionBlockWarden warden(this);

    SchemaNodePtrSet schemaNodes;
    phop::TupleList inTuples;

    executeSchemaPathTest(dataRoot.getSchemaNode(), pe::AtomizedPath(path.getBody()->begin(), path.getBody()->end()), &schemaNodes, false);

    GraphExecutionBlock::current()->resultMap[resultSet.at(0)] = 0;
    
    if (schemaNodes.size() == 0) {
        U_ASSERT(false);
    };

    if (schemaNodes.size() == 1) {
        return new phop::SchemaScan(schema_node_cptr(*schemaNodes.begin()), 1, 0);
    }

    for (SchemaNodePtrSet::const_iterator it = schemaNodes.begin(); it != schemaNodes.end(); ++it) {
        inTuples.push_back(
            phop::MappedTupleIn(new phop::SchemaScan(schema_node_cptr(*it), 1, 0), 0, 0));
    };

    return new phop::DocOrderMerge(1, inTuples);
}

phop::IOperator * PathEvaluationPrototype::compile()
{
    GraphExecutionBlockWarden warden(this);
    // TODO : make effective evaluation

    ITupleOperator * opin = dynamic_cast<ITupleOperator *>(in.at(0).op->getStatement());
    TupleIn aopin(opin, GraphExecutionBlock::current()->resultMap[in.at(0).index]);

    U_ASSERT(opin != NULL);

    IValueOperator * ain = new phop::ReduceToItemOperator(aopin, true);

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
                it->satisfies(pe::StepPredicate(pe::ChildAttrAxisTest))) {
            ++it;
        };

        if (cstart != it) {
            ain = new pe::PathSchemaResolve(ain, pe::AtomizedPath(cstart, it));
        }

        if (pstart == it) {
            U_ASSERT(false);
            break;
        };
    };

    if (it != pathBody->end()) {
        ain = new pe::PathEvaluateTraverse(ain, pe::AtomizedPath(it, pathBody->end()));
    }

    GraphExecutionBlock::current()->resultMap[resultSet.at(0)] = aopin->_tsize();

    return new phop::NestedEvaluation(aopin, ain, aopin->_tsize() + 1, aopin->_tsize());
}

phop::IOperator * MergeJoinPrototype::compile()
{
    GraphExecutionBlockWarden warden(this);

    POProtIn left(in[0]), right(in[1]);

    ITupleOperator * leftPtr = dynamic_cast<ITupleOperator *>(left.op->getStatement());
    unsigned leftIdx = GraphExecutionBlock::current()->resultMap[left.index];

    ITupleOperator * rightPtr = dynamic_cast<ITupleOperator *>(right.op->getStatement());
    unsigned rightIdx = GraphExecutionBlock::current()->resultMap[right.index];

    TupleIn leftOp(leftPtr, leftIdx);
    TupleIn rightOp(rightPtr, rightIdx);

    if (needLeftSort) {
        leftOp.op = new TupleSort(leftOp->_tsize(), MappedTupleIn(leftOp), comparison->createTupleSerializer(leftOp.offs));
    }

    if (needRightSort) {
        rightOp.op = new TupleSort(rightOp->_tsize(), MappedTupleIn(rightOp), comparison->createTupleSerializer(rightOp.offs));
    }

    TupleChrysalis * rightScheme = right.op->result;

    for (unsigned i = 0; i < rightScheme->tuples.size(); ++i) {
        if (rightScheme->tuples[i].status == TupleValueInfo::evaluated) {
            GraphExecutionBlock::current()->resultMap[i] += leftOp->_tsize();
        };
    };

    return new TupleJoinFilter(
        leftOp->_tsize() + rightOp->_tsize(),
        MappedTupleIn(leftOp),
        MappedTupleIn(rightOp.op, rightOp.offs, leftOp->_tsize()),
        comparison->getTupleCellComparison());
}

IOperator* FilterTuplePrototype::compile()
{
    GraphExecutionBlockWarden warden(this);

    POProtIn left(in[0]), right(in[1]);

    ITupleOperator * opPtr = dynamic_cast<ITupleOperator *>(left.op->getStatement());

    unsigned leftIdx = GraphExecutionBlock::current()->resultMap[left.index];
    unsigned rightIdx = GraphExecutionBlock::current()->resultMap[right.index];
    
    return new TuplePredicateFilter(
        MappedTupleIn(opPtr, 0, 0),
        comparison->getValueFunction(leftIdx, rightIdx));
}


phop::IOperator * ValueScanPrototype::compile()
{
    GraphExecutionBlockWarden warden(this);

    if (in.at(0).op->getProtInfo() == OPREF(AbsPathScanPrototype)) {
        AbsPathScanPrototype * pathScan = dynamic_cast<AbsPathScanPrototype *>(in.at(0).op);

        SchemaNodePtrSet schemaNodes;
        phop::TupleList inTuples;
        pe::AtomizedPath path = pe::AtomizedPath(pathScan->getPath().getBody()->begin(), pathScan->getPath().getBody()->end());

        executeSchemaPathTest(pathScan->getRoot().getSchemaNode(), path, &schemaNodes, false);

        GraphExecutionBlock::current()->resultMap[pathScan->resultSet.at(0)] = 0;
        
        if (schemaNodes.size() == 0) {
            U_ASSERT(false);
        };

        if (schemaNodes.size() == 1) {
            return
                new phop::SchemaValueScan(*schemaNodes.begin(),
                    GeneralComparisonPrototype(cmp).getTupleCellComparison(), value, 1, 0, 1);
        }

        for (SchemaNodePtrSet::const_iterator it = schemaNodes.begin(); it != schemaNodes.end(); ++it) {
            inTuples.push_back(
                MappedTupleIn(
                    new phop::SchemaValueScan(schema_node_cptr(*it),
                        GeneralComparisonPrototype(cmp).getTupleCellComparison(), value, 1, 0, 1), 0, 0));
        };

        return new phop::DocOrderMerge(1, inTuples);
    } else {
        ITupleOperator * leftOpPtr = dynamic_cast<ITupleOperator *>(in.at(0).op->getStatement());
        // WARNING: Result is mapped after compile()
        unsigned leftIdx = GraphExecutionBlock::current()->resultMap[in.at(0).index];
        MappedTupleIn leftOp(leftOpPtr, leftIdx, 0);

        return new CachedNestedLoop(leftOp->_tsize(), leftOp,
            MappedTupleIn(new BogusConstSequence(value, 1, 0), 0, TupleMap()),
            GeneralComparisonPrototype(cmp).getTupleCellComparison(),
            CachedNestedLoop::strict_output);
    }
}

IOperator* EvaluatePrototype::compile()
{
    GraphExecutionBlockWarden warden(this);

    ITupleOperator * leftOpPtr = dynamic_cast<ITupleOperator *>(in.at(0).op->getStatement());
    // WARNING: Result is mapped after compile()
    unsigned leftIdx = GraphExecutionBlock::current()->resultMap[in.at(0).index];
    MappedTupleIn leftOp(leftOpPtr, leftIdx, 0);

    return NULL;//new FunctionOp(leftOp, leftOp->_tsize() + 1, leftOp->_tsize(), func->createInstance());
}


phop::IOperator * ValidatePathPrototype::compile()
{
    GraphExecutionBlockWarden warden(this);

    return in.at(0).op->getStatement();
}
