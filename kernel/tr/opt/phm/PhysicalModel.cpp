#include "PhysicalModel.h"

#include "tr/opt/alg/Predicates.h"
#include "tr/executor/xpath/XPathTypes.h"
#include "tr/opt/cost/Statistics.h"
#include "tr/opt/phm/Operations.h"
#include "tr/structures/producer.h"

PlanInfo::PlanInfo(size_t initialTupleSetSize)
  : desc(0), totalCost(0)
{
    initialTupleSet = new TupleChrysalis(initialTupleSetSize);
    branchList.resize(initialTupleSetSize, NULL);
};

PlanInfo::PlanInfo(const PlanInfo* parent)
  : desc(parent->desc), opList(parent->opList), totalCost(0),
    branchList(parent->branchList), initialTupleSet(parent->initialTupleSet)
{
}

TupleRef PlanInfo::initTupleSet(DataNode* node)
{
    TupleRef ref(initialTupleSet, node->absoluteIndex);

    ref->status = ElementDescriptor::available;
    ref->node = node;
    publicCostModel->evaluateBaseStatistics(ref->statistics);

    return ref;
}

TupleChrysalis::TupleChrysalis(size_t size)
  : availableTupleMask(0), rowSize(0, 0), rowCount(0, 0), op(0)
{
    ElementDescriptor _z = {};
    tuples.resize(size, _z);
}

TupleChrysalis::TupleChrysalis(const TupleChrysalis* parent, POProt * _op)
  : availableTupleMask(0), rowSize(0, 0), rowCount(0, 0), op(_op)
{
    availableTupleMask = parent->availableTupleMask;
    rowCount = parent->rowCount;
    rowSize = parent->rowSize;
}

TupleRef PhysicalModel::updateOne(const TupleChrysalis* parent, const POProtIn& op)
{
    TupleChrysalis * t = new TupleChrysalis(parent);
    t->tuples.at(op.index)._gen = op;
    t->tuples.at(op.index).status = ElementDescriptor::evaluated;
}

TupleRef PhysicalModel::updateTwo(const TupleChrysalis* x, const TupleChrysalis* y, POProt* op, TupleId ind1, TupleId ind2)
{
    U_ASSERT(x->size() == y->size());

    TupleChrysalis* result = new TupleChrysalis(x->size());

    result->rowSize = x->rowSize + y->rowSize;
    result->availableTupleMask = x->availableTupleMask | y->availableTupleMask;

    for (unsigned i = 0; i < x->size(); ++i) {
        ElementDescriptor * xd = &(x->tuples[i]);
        ElementDescriptor * yd = &(y->tuples[i]);

        if (xd->statistics == ElementDescriptor::evaluated && yd->statistics == ElementDescriptor::evaluated) {
            U_ASSERT(false);
        };

        if (xd->statistics == ElementDescriptor::evaluated) {
            result->tuples[i] = *xd;
        } else if (yd->statistics == ElementDescriptor::evaluated) {
            result->tuples[i] = *yd;
        } else {
            result->tuples[i] = plan->initialTupleSet->tuples[i];
        };
    };

    result->tuples.at(ind1).status = ElementDescriptor::evaluated;
    result->tuples.at(ind1)._gen = op;
    
    result->tuples.at(ind2).status = ElementDescriptor::evaluated;
    result->tuples.at(ind2)._gen = op;
    
    result->rowCount = x->rowCount * y->rowCount;

    return result;
}

double PlanInfo::evaluateTotalCost() const
{
    double sum = opList.back().cost->fullCost.;
    return sum;
}

PlanInfo* PlanInfo::extend(Predicate* what) const
{
    PhysicalModel phm(new PlanInfo(this));

    phm.plan->desc = this->desc | what->indexBit;
    what->compile(&phm);

    return phm.plan;
}

POProtIn PhysicalModel::doMaterialize(TupleId t)
{
    TupleRef tref(plan->initialTupleSet, t);
  
    switch (tref->status) {
        case ElementDescriptor::available : {
            POProt * op;
            DataNode * node = tref->node;

            U_ASSERT(tref.tupleDesc == plan->initialTupleSet);

            switch (node->type) {
    //        case DataNode::dnExternal : return new PPAdapter(el);
    //        case DataNode::dnConst : return new TSCache(el);
            case DataNode::dnDatabase :
                op = new AbsPathScanPrototype(this, tref);
                break;
            default :
                U_ASSERT(false);
            };

            plan->branchList.at(tref.tid) = op;
            plan->opList.push_back(op);

            return POProtIn(op, t);
        } break;
        case ElementDescriptor::empty :
          return POProtIn(NULL, t);
        default:
          U_ASSERT(false);
          return POProtIn(NULL, t);
    };
}

void PhysicalModel::updateBranch(const POProt* op)
{
    TupleChrysalis* x = op->result;

    for (unsigned i = 0; i != x->size(); ++i) {
        if (x->get(i)->status == ElementDescriptor::evaluated) {
            plan->branchList[i] = op;
        };
    };
}


#define EVALUATED(x) ((x)->status == ElementDescriptor::evaluated)
#define AVAILABLE(x) ((x)->status == ElementDescriptor::available)

void* PhysicalModel::compile(VPredicate* pred)
{
    POProtIn leftOp = materialize(plan->getRef(pred->leftNode->absoluteIndex));
    POProtIn rightOp = materialize(plan->getRef(pred->rightNode->absoluteIndex));

    result = NULL;

    if (leftOp.op != NULL && rightOp.op != NULL) {
        result = new SortMergeJoinPrototype(leftOp, rightOp, pred->cmp);
/*
        if (isCacheReasonable(left->statistics->nodeMass)) {
            result = new CachedNestedLoop(left->source, right->source, pred->Path);
        } else if (isCacheReasonable(right->mass)) {
            result = new CachedNestedLoop(right->source, left->source, pred->op);
        } else if (isHashable(pred)) {
        // TODO: check for index available
        // TODO: check for nested loop join
            //            result = new HashJoin(left->source, right->source, pred->op);
        } else {
            result = new SortMergeJoin(left->source, right->source, pred->op);
        };
*/
    } else {
        return NULL;
    };

    updateBranch(result);
    plan->opList.push_back(result);

    return result;
}

void* PhysicalModel::compile(SPredicate* pred)
{
    POProtIn leftOp = plan->getRef(pred->leftNode->absoluteIndex);
    POProtIn rightOp = plan->getRef(pred->rightNode->absoluteIndex);
    
    result = NULL;

    if (leftOp.op != NULL && rightOp.op != NULL) {
        result = new StructuralJoinPrototype(leftOp, rightOp, pred->path);

/*
        if (isCacheReasonable(left->mass)) {
            result = new CachedNestedLoop(left->source, right->source, pred->path);
        } else if (isCacheReasonable(right->mass)) {
            result = new CachedNestedLoop(right->source, left->source, pred->path.inverse());
        } else {
            result = new SortMergeJoin(left->source, right->source, pred->path);
        };
*/
    } else if (leftOp.op != NULL) {
        result = new PathEvaluationPrototype(this,
            leftOp, initialRef(rightOp.index), pred->path);
    } else if (rightOp.op != NULL && pred->path.inversable()) {
        result = new PathEvaluationPrototype(this,
            rightOp, initialRef(leftOp.index), pred->path.inverse().squeeze());
    } else {
        leftOp = materialize(leftOp);
        rightOp = materialize(rightOp);

        return asdlfknasdklgas;
        
        if (leftOp.op != NULL && rightOp.op != NULL) {
//            result = new PathEvaluationPrototype(this,
//              leftOp, right, pred->path);
        } else {
            return NULL;
        };
    };

    updateBranch(result);
    plan->opList.push_back(result);

    return result;
}



PPIterator* PlanInfo::compile()
{
    return NULL;
}

static inline
bool isCacheReasonable(const Range &amass) {
    return amass.second < 64*1024;
};




#define CDGQNAME(N) xsd::QName::getConstantQName(NULL_XMLNS, N)

IElementProducer* PlanInfo::toXML(IElementProducer* element) const
{
    element = element->addElement(CDGQNAME("plan"), xs_anyType);

    for (OperationList::const_iterator i = opList.begin(); i != opList.end(); ++i) {
        (*i)->toXML(element);
    };

    element->close();
    return element;
}
