#include "PhysicalModel.h"

#include "tr/opt/alg/Predicates.h"
#include "tr/executor/xpath/XPathTypes.h"
#include "tr/opt/cost/Statistics.h"
#include "tr/opt/phm/Operations.h"

double PlanInfo::evaluateTotalCost() const
{
    double sum = 0;

    joinTree.back()->cost->fullCost.first;

    return sum;
}

int PlanTupleScheme::update(int index, SchemeElement* el)
{
    PlanTupleSchemeMap::iterator it = content.find(index);

    if (it == content.end()) {
        content.insert(PlanTupleSchemeMap::value_type(index, el));
    } else {
        it->second = el;
    };

    return index;
}


SchemeElement * PlanInfo::materialize(SchemeElement * el) {
    if (el->evaluated || !el->available) {
        return el;
    } else switch (el->node->type) {
//        case DataNode::dnExternal : return new PPAdapter(el);
//        case DataNode::dnConst : return new TSCache(el);
        case DataNode::dnDatabase :
            this->joinTree.push_back(new AbsPathScanPrototype(el, el->node->path, el->node->root));
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

    return result;
}

PlanInfo* PlanInfo::evaluateInitialPlanInfo(PlanTupleScheme* scheme, Predicate* pred, PlanDesc desc)
{
    PhysicalModel phm;

    phm.result = NULL;
    phm.plan = new PlanInfo();
    phm.plan->schema = scheme;
    phm.plan->desc = desc;
    pred->compile(&phm);

    return phm.plan;
}

PlanInfo* PlanInfo::clone() const
{
    PlanInfo* result = new PlanInfo;
    result->desc = desc;
    result->joinTree = joinTree;
    result->schema = new PlanTupleScheme(*schema);
    return result;
}

PlanInfo* PlanInfo::apply(Predicate* what)
{
    PhysicalModel phm;

    phm.result = NULL;
    phm.plan = this;
    phm.plan->desc |= what->indexBit;
    what->compile(&phm);

    return phm.plan;
}

PPIterator* PlanInfo::compile()
{
    return NULL;
}


static inline
bool isCacheReasonable(const Range &amass) {
    return amass.second < 64*1024;
};

void* PhysicalModel::compile(VPredicate* pred)
{
    SchemeElement * left = plan->schema->get(pred->leftNode->varIndex);
    SchemeElement * right = plan->schema->get(pred->rightNode->varIndex);

    if (!left->evaluated && left->available) {
        left = plan->materialize(left);
    }

    if (!right->evaluated && right->available) {
        right = plan->materialize(right);
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

    plan->schema->update(pred->leftNode->varIndex, left);
    plan->schema->update(pred->rightNode->varIndex, right);

    return result;
}

void* PhysicalModel::compile(SPredicate* pred)
{
    SchemeElement * left = plan->schema->get(pred->leftNode->varIndex);
    SchemeElement * right = plan->schema->get(pred->rightNode->varIndex);

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
    } else if (right->evaluated) {
        result = new AxisStepPrototype(right, left, pred->path.inverse());
    // TODO: Select between two available choices
    } else if (left->available || right->available) {
        result = new AxisStepPrototype(plan->materialize(left), right, pred->path);
//        result = new PathStepPrototype(plan->materialize(right), left, pred->path.inverse());
    } else {
        result = NULL;
        return NULL;
    };

    if (result != NULL) {
        plan->joinTree.push_back(result);
    };

    plan->schema->update(pred->leftNode->varIndex, left);
    plan->schema->update(pred->rightNode->varIndex, right);
    
    return result;
}

/*

*/