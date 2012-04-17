#include "PhysicalModel.h"

#include "tr/opt/alg/Predicates.h"
#include "tr/executor/xpath/XPathTypes.h"
#include "tr/opt/cost/Statistics.h"
#include "tr/opt/phm/Operations.h"
#include "tr/structures/producer.h"

double PlanInfo::evaluateTotalCost() const
{
    double sum = 0;

    joinTree.back()->cost->fullCost.first;

    return sum;
}

int PlanInfo::updateScheme(int index, SchemeElement* el)
{
    PlanTupleSchemeMap::iterator it = scheme.find(index);

    if (it == scheme.end()) {
        scheme.insert(PlanTupleSchemeMap::value_type(index, el));
    } else {
        it->second = el;
    };

    return index;
}

PlanInfo::PlanInfo()
  : totalCost(0), desc(0)
{ };

PlanInfo::PlanInfo(const PlanInfo* parent)
  : totalCost(0), scheme(parent->scheme), joinTree(parent->joinTree), desc(parent->desc)
{

}

SchemeElement * PhysicalModel::materialize(SchemeElement * el)
{
    if (!el->available) {
        return NULL;
    } else if (el->evaluated) {
        return el;
    } else switch (el->node->type) {
//        case DataNode::dnExternal : return new PPAdapter(el);
//        case DataNode::dnConst : return new TSCache(el);
        case DataNode::dnDatabase :
            this->plan->joinTree.push_back(new AbsPathScanPrototype(el, el->node->path, el->node->root));
            break;
        default :
            U_ASSERT(false);
    };

    return el;
};

SchemeElement* PlanInfo::initSchemeElement(DataNode* node)
{
    SchemeElement* result = new SchemeElement();
    result->node = node;
    publicCostModel->evaluateBaseStatistics(result);
    updateScheme(node->varIndex, result);

    return result;
}

PlanInfo* PlanInfo::extend(Predicate* what) const
{
    PhysicalModel phm(new PlanInfo(this));
    
    phm.plan->desc = this->desc | what->indexBit;
    what->compile(&phm);

    return phm.plan;
}


PPIterator* PlanInfo::compile()
{
    return NULL;
}

#define CDGQNAME(N) xsd::QName::getConstantQName(NULL_XMLNS, #N)

IElementProducer* PlanInfo::toXML(IElementProducer* element) const
{
    element = element->addElement(CDGQNAME(plan), xs_anyType);
//    element->addElement() 
    element->close();
    return element;
}


static inline
bool isCacheReasonable(const Range &amass) {
    return amass.second < 64*1024;
};

void* PhysicalModel::compile(VPredicate* pred)
{
    SchemeElement * left = plan->scheme[pred->leftNode->varIndex];
    SchemeElement * right = plan->scheme[pred->rightNode->varIndex];

    if (!left->evaluated && left->available) {
        left = materialize(left);
    }

    if (!right->evaluated && right->available) {
        right = materialize(right);
    }

    if (left->evaluated && right->evaluated) {
        result = new SortMergeJoinPrototype(left, right, pred->cmp);
        plan->joinTree.push_back(result);
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
        result = NULL;
        return NULL;
    };

    plan->updateScheme(pred->leftNode->varIndex, left);
    plan->updateScheme(pred->rightNode->varIndex, right);

    return result;
}

void* PhysicalModel::compile(SPredicate* pred)
{
    SchemeElement * left = plan->scheme[pred->leftNode->varIndex];
    SchemeElement * right = plan->scheme[pred->rightNode->varIndex];

    if (left->evaluated && right->evaluated) {
        result = new StructuralSortMergeJoinPrototype(left, right, pred->path);
/*
        if (isCacheReasonable(left->mass)) {
            result = new CachedNestedLoop(left->source, right->source, pred->path);
        } else if (isCacheReasonable(right->mass)) {
            result = new CachedNestedLoop(right->source, left->source, pred->path.inverse());
        } else {
            result = new SortMergeJoin(left->source, right->source, pred->path);
        };
*/
    } else if (left->evaluated) {
        result = new AxisStepPrototype(left, right, pred->path);
    } else if (right->evaluated && pred->path.inversable()) {
        result = new AxisStepPrototype(right, left, pred->path.inverse());
    // TODO: Select between two available choices
    } else if (left->available || right->available) {
        result = new AxisStepPrototype(materialize(left), right, pred->path);
//        result = new PathStepPrototype(plan->materialize(right), left, pred->path.inverse());
    } else {
        result = NULL;
        return NULL;
    };

    if (result != NULL) {
        plan->joinTree.push_back(result);
    };

    plan->updateScheme(pred->leftNode->varIndex, left);
    plan->updateScheme(pred->rightNode->varIndex, right);
    
    return result;
}

