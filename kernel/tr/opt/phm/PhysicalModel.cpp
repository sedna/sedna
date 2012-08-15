#include "PhysicalModel.h"

#include <stdint.h>

#include "tr/opt/graphs/Predicates.h"
#include "tr/opt/path/XPathTypes.h"
#include "tr/opt/cost/Statistics.h"
#include "tr/opt/phm/Operations.h"
#include "tr/models/XmlConstructor.h"
#include "tr/strings/strings.h"
#include "tr/opt/algorithms/SequenceModel.h"
#include "tr/opt/phm/ComparisonModels.h"
#include "tr/opt/functions/Functions.h"

#include <vector>

using namespace opt;

RTTI_DEF_BASE(POProt)

PlanInfo::PlanInfo(size_t initialTupleSetSize)
  : desc(0), parent(0), totalCost(0)
{
    initialTupleSet = new TupleChrysalis(initialTupleSetSize);
    branchList.resize(initialTupleSetSize, NULL);
};

PlanInfo::PlanInfo(const PlanInfo* parent, PlanDesc _desc)
    : desc(_desc), parent(parent->desc), totalCost(0), opList(parent->opList),
    branchList(parent->branchList), initialTupleSet(parent->initialTupleSet)
{
}

TupleRef PlanInfo::initTupleSet(DataNode* node)
{
    TupleRef ref(initialTupleSet, node->absoluteIndex);

    ref->status = TupleValueInfo::available;
    ref->node = node;
    ref->statistics = NULL;

    return ref;
}

TupleChrysalis::TupleChrysalis(size_t size)
  : _width(0), rowSize(0, 0), rowCount(0, 0)
{
    TupleValueInfo _z = {};
    tuples.resize(size, _z);
}

TupleChrysalis::TupleChrysalis(const TupleChrysalis* parent)
  : tuples(parent->tuples), _width(parent->_width), rowSize(0, 0), rowCount(0, 0)
{
    rowCount = parent->rowCount;
    rowSize = parent->rowSize;
}

TupleChrysalis * PhysicalModel::updateOne(TupleChrysalis* parent, const POProtIn& op)
{
    TupleChrysalis * t = new TupleChrysalis(parent);
    TupleRef opT(t, op.index);
    opT->_gen = op.op;

    if (opT->status != TupleValueInfo::evaluated) {
        opT->status = TupleValueInfo::evaluated;
        t->_width++;
    };

    return t;
}

TupleChrysalis * PhysicalModel::updateTwo(TupleChrysalis* x, TupleChrysalis* y, POProt* op, TupleId ind1, TupleId ind2)
{
    U_ASSERT(x->tuples.size() == y->tuples.size());

    TupleChrysalis* result = new TupleChrysalis(x);

    if (x != y) {
        result->rowSize = x->rowSize + y->rowSize;
        result->rowCount = x->rowCount * y->rowCount;

        for (unsigned i = 0; i < result->tuples.size(); ++i) {
            TupleValueInfo * xd = x->get(i);
            TupleValueInfo * yd = y->get(i);

            if (xd->status == TupleValueInfo::evaluated && yd->status == TupleValueInfo::evaluated) {
                U_ASSERT(false);
            };

            if (xd->status == TupleValueInfo::evaluated) {
                result->tuples[i] = *xd;
            } else if (yd->status == TupleValueInfo::evaluated) {
                result->tuples[i] = *yd;
            } else {
                result->tuples[i] = plan->initialTupleSet->tuples[i];
            };
        };
    };

    result->tuples.at(ind1).status = TupleValueInfo::evaluated;
    result->tuples.at(ind1)._gen = op;

    result->tuples.at(ind2).status = TupleValueInfo::evaluated;
    result->tuples.at(ind2)._gen = op;

    result->_width = 0;
    for (unsigned i = 0; i < x->tuples.size(); ++i) {
        if (result->get(i)->status == TupleValueInfo::evaluated) {
            result->_width++;
        }
    }

    return result;
}

double PlanInfo::evaluateTotalCost() const
{
    double sum = opList.back()->getCost()->fullCost.avg();
    return sum;
}

PlanInfo* PlanInfo::extend(Predicate* what) const
{
    PhysicalModel phm(new PlanInfo(this, this->desc | what->indexBit));

    what->compile(&phm);

    if (phm.result == NULL) {
        return NULL;
    };

    return phm.plan;
}

POProtIn PhysicalModel::doMaterialize(TupleId t, bool addToTree)
{
    TupleRef tref(plan->initialTupleSet, t);
  
    switch (tref->status) {
        case TupleValueInfo::available : {
            POProt * op;
            DataNode * node = tref->node;

            U_ASSERT(tref.tupleDesc == plan->initialTupleSet);

            switch (node->type) {
    //        case DataNode::dnExternal : return new PPAdapter(el);
            case DataNode::dnFreeNode :
            case DataNode::dnConst : /* return new TSCache(el); */
                return POProtIn(NULL, t);
            case DataNode::dnDatabase :
                op = new AbsPathScanPrototype(this, tref);
                break;
            default :
                U_ASSERT(false);
            };

            if (addToTree) {
                plan->branchList.at(tref.tid) = op;
                plan->opList.push_back(op);
            }

            return POProtIn(op, t);
        } break;
        case TupleValueInfo::empty :
          return POProtIn(NULL, t);
        default:
          U_ASSERT(false);
          return POProtIn(NULL, t);
    };
}

void PlanInfo::updateBranch(POProt* op)
{
    TupleChrysalis* x = op->result;

    x->_width = 0;
    for (unsigned i = 0; i != x->tuples.size(); ++i) {
        if (x->get(i)->status == TupleValueInfo::evaluated) {
            branchList.at(i) = op;
            x->_width++;
        };
    };
}

#define EVALUATED(x) ((x)->status == TupleValueInfo::evaluated)
#define AVAILABLE(x) ((x)->status == TupleValueInfo::available)

inline static
bool isConst(const TupleRef &x) { return AVAILABLE(x) && x->node->type == DataNode::dnConst; };

inline static
bool isPath(const TupleRef &x) { return AVAILABLE(x) && x->node->type == DataNode::dnDatabase; };

void* PhysicalModel::compile(ValuePredicate* pred)
{
    POProtIn leftOp = materialize(plan->getRef(pred->left()->absoluteIndex));
    POProtIn rightOp = materialize(plan->getRef(pred->right()->absoluteIndex));

    result = NULL;

    if (leftOp.op == NULL && rightOp.op == NULL) {
        return NULL;
    };

    if (leftOp.op != NULL && rightOp.op != NULL) {
        if (leftOp.op == rightOp.op) {
            result = new FilterTuplePrototype(this, leftOp, rightOp, new GeneralComparisonPrototype(pred->cmp));
        } else {
            result = new MergeJoinPrototype(this, leftOp, rightOp, new GeneralComparisonPrototype(pred->cmp));
        };
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
    } else if (isConst(initialRef(rightOp.index))) {
        result = new ValueScanPrototype(this, leftOp, initialRef(rightOp.index), pred->cmp);
    } else if (isConst(initialRef(leftOp.index)) && pred->cmp.inversable()) {
        result = new ValueScanPrototype(this, rightOp, initialRef(leftOp.index), pred->cmp.inverse());
    } else {
        return NULL;
    };

    pushOp(result);

    return result;
}

enum strategy_t {
    impossible_to_evaluate,
    join,
    evaluate_Right_from_Left,
    evaluate_Left_from_Right,
    evaluate_Right_then_Left,
    evaluate_Left_then_Right,
};

namespace opt {
  
struct Candidate
{
    int strategy;
    double cost;
    OperationList opList;

    explicit Candidate(int _strategy) : strategy(_strategy) {};
};

}

inline static
bool setOp(POProtIn & op, POProt* can, OperationList & opList)
{
    if (op.op == NULL) {
        if (can == NULL) {
            return false;
        } else {
            opList.push_back(can);
            op.op = can;
        };
    };

    return true;
};

struct StructuralCandidateSelector
{
    PhysicalModel * model;
    StructuralPredicate* pred;
    
    POProtIn leftOp;
    POProtIn rightOp;
    
    POProt* leftCandidate;
    POProt* rightCandidate;

    void * evaluate(Candidate* candidate);
};

void* StructuralCandidateSelector::evaluate(Candidate* candidate)
{
    switch(candidate->strategy) {
        case join:
            if (!setOp(leftOp, leftCandidate, candidate->opList)) {
                return NULL;
            }

            if (!setOp(rightOp, rightCandidate, candidate->opList)) {
                return NULL;
            }
            
            if (leftOp.op == rightOp.op) {
                candidate->opList.push_back(
                  new FilterTuplePrototype(model,
                    leftOp, rightOp, new PathComparisonPrototype(pred->path)));
            } else {
                candidate->opList.push_back(
                  new MergeJoinPrototype(model,
                    leftOp, rightOp, new PathComparisonPrototype(pred->path)));
            };

            break;
        case evaluate_Left_then_Right:
            if (!setOp(leftOp, leftCandidate, candidate->opList)) {
                return NULL;
            }
        case evaluate_Right_from_Left:
            U_ASSERT(rightOp.op == NULL);

            candidate->opList.push_back(
              new PathEvaluationPrototype(model,
                leftOp, model->initialRef(rightOp.index), pred->path));
            // TODO :: maybe we should validate path here
            break;
        case evaluate_Right_then_Left:
            if (!setOp(rightOp, rightCandidate, candidate->opList)) {
                return NULL;
            }
        case evaluate_Left_from_Right:
            U_ASSERT(pred->path.inversable());
            U_ASSERT(leftOp.op == NULL);

            candidate->opList.push_back(
              new PathEvaluationPrototype(model,
                rightOp, model->initialRef(leftOp.index),
                pred->path.inverse(pe::StepTest(pe::nt_any_kind, xsd::QNameAny))));
            // TODO :: maybe we should validate path here
            break;
        case impossible_to_evaluate:
            return NULL;
//                break;
    };

    if (candidate->opList.size() > 0) {
        candidate->cost = candidate->opList.back()->getCost()->fullCost.avg();
        return candidate->opList.back();
    } else {
        return NULL;
    };
}


void* PhysicalModel::compile(StructuralPredicate* pred)
{
    StructuralCandidateSelector sel;
    
    bool evaluatable = !pred->path.horizontal();
    bool inversablePath = pred->path.inversable();
    
    std::vector<Candidate> candidates;
    candidates.reserve(3);

    sel.model = this;
    sel.pred = pred;

    result = NULL;

    if (!evaluatable) {
        sel.leftOp = materialize(plan->getRef(pred->left()->absoluteIndex));
        sel.rightOp = materialize(plan->getRef(pred->right()->absoluteIndex));

       // Just for a nice code
        sel.leftCandidate = sel.leftOp.op;
        sel.rightCandidate = sel.rightOp.op;
    } else {
        sel.leftOp = plan->getRef(pred->left()->absoluteIndex);
        sel.rightOp = plan->getRef(pred->right()->absoluteIndex);

        if (sel.leftOp.op == NULL) {
            sel.leftCandidate = doMaterialize(sel.leftOp.index, false).op;
        } else {
            sel.leftCandidate = sel.leftOp.op;
        };

        if (sel.rightOp.op == NULL) {
            sel.rightCandidate = doMaterialize(sel.rightOp.index, false).op;
        } else {
            sel.rightCandidate = sel.rightOp.op;
        };
    }

    if (evaluatable) {
        if (sel.leftOp.op == NULL && sel.rightOp.op == NULL) {
            if (sel.rightCandidate != NULL && inversablePath) {
                candidates.push_back(Candidate(evaluate_Right_then_Left));
            }
            
            if (sel.leftCandidate != NULL) {
                candidates.push_back(Candidate(evaluate_Left_then_Right));
            }
        } else if (sel.rightOp.op == NULL) {
            candidates.push_back(Candidate(evaluate_Right_from_Left));
        } else if (sel.leftOp.op == NULL && inversablePath) {
            candidates.push_back(Candidate(evaluate_Left_from_Right));
        }
    };

    // Join is always an option!
    if (sel.leftCandidate != NULL || sel.rightCandidate != NULL) {
        candidates.push_back(Candidate(join));
    }
    
    if (candidates.size() == 0) {
        return NULL;
    };

    // TODO: actially, we should minimize cost here, but we just
    // take the last option 

    Candidate * minCandidate = NULL;
    
    for (std::vector<Candidate>::iterator cand = candidates.begin(); cand != candidates.end(); ++cand) {
        StructuralCandidateSelector newSelector(sel);
        Candidate * candidate = &(*cand);

        if (newSelector.evaluate(candidate) != NULL) {
            if (minCandidate == NULL || candidate->cost < minCandidate->cost) {
                minCandidate = candidate;
            }
        };
    };

    if (minCandidate != NULL) {
        for (OperationList::iterator it = minCandidate->opList.begin(); it != minCandidate->opList.end(); ++it) {
            result = *it;
            pushOp(result);
        };
    };
    
    return result;
}

/*
void* PhysicalModel::compile(FPredicate* pred)
{
    POProtIn leftOp = materialize(plan->getRef(pred->left()->absoluteIndex));
    POProtIn rightOp = plan->getRef(pred->right()->absoluteIndex);

    result = NULL;
    
    if (leftOp.op == NULL) {
        return NULL;
    } else {
        U_ASSERT(rightOp.op == NULL);

        result = new EvaluatePrototype(this, leftOp, initialRef(rightOp.index), pred->func);
    };

    updateBranch(result);
    plan->opList.push_back(result);
    
    return NULL;
}
*/

phop::ITupleOperator* PlanInfo::compile()
{
    return dynamic_cast<phop::ITupleOperator*>(opList.back()->getStatement());
}

#define SE_EL_NAME(N) xsd::QName::getConstantQName(NULL_XMLNS, N)

XmlConstructor & PlanInfo::toXML(XmlConstructor& constructor) const
{
    constructor.openElement(SE_EL_NAME("plan"));

    constructor.addAttributeValue(SE_EL_NAME("desc"), tuple_cell::atomic_int(desc));
    constructor.addAttributeValue(SE_EL_NAME("parent"), tuple_cell::atomic_int(parent));
    
    for (OperationList::const_iterator i = opList.begin(); i != opList.end(); ++i) {
        (*i)->toXML(constructor);
    };

    constructor.closeElement();

    return constructor;
}

inline static
XmlConstructor & rangeToElement(Range x, XmlConstructor & element, const char * name)
{
    element.openElement(SE_EL_NAME(name));

    element.addAttributeValue(SE_EL_NAME("low"), tuple_cell::atomic(x.lower));
    element.addAttributeValue(SE_EL_NAME("up"), tuple_cell::atomic(x.upper));
    element.addAttributeValue(SE_EL_NAME("avg"), tuple_cell::atomic(x.avg()));

    element.closeElement();

    return element;
};

XmlConstructor & POProt::__commonToXML(XmlConstructor & element) const
{
    for (std::vector<POProtIn>::const_iterator i = in.begin(); i != in.end(); ++i) {
        element.openElement(SE_EL_NAME("in"));
        element.addAttributeValue(SE_EL_NAME("ref"), tuple_cell::atomic_int((ptrdiff_t) (i->op)));
        element.closeElement();
    };

    for (std::vector<int>::const_iterator i = resultSet.begin(); i != resultSet.end(); ++i) {
        TupleRef tref(result, *i);
        element.openElement(SE_EL_NAME("result"));
        element.addAttributeValue(SE_EL_NAME("var"), tuple_cell::atomic_int(tref->node->index));
        element.closeElement();
    };

    return element;
}

XmlConstructor & TupleChrysalis::toXML(XmlConstructor & constructor) const
{
    constructor.openElement(SE_EL_NAME("tuple"));

    constructor.addAttributeValue(SE_EL_NAME("count"), tuple_cell::atomic(rowCount.avg()));
    constructor.addAttributeValue(SE_EL_NAME("size"), tuple_cell::atomic(rowSize.avg()));
    constructor.addAttributeValue(SE_EL_NAME("width"), tuple_cell::atomic_int(_width));

    constructor.closeElement();
    
    return constructor;
};

XmlConstructor & POProt::__toXML(XmlConstructor & constructor) const
{
    this->result->toXML(constructor);

    return constructor;
}

XmlConstructor & POProt::toXML(XmlConstructor & constructor) const
{
    constructor.openElement(SE_EL_NAME(this->info()->name));
    constructor.addAttributeValue(SE_EL_NAME("id"), tuple_cell::atomic_int((ptrdiff_t) (this)));

    __commonToXML(constructor);
    __toXML(constructor);

    constructor.openElement(SE_EL_NAME("cost"));

    rangeToElement(this->cost->firstCost, constructor, "first");
    rangeToElement(this->cost->nextCost, constructor, "next");
    rangeToElement(this->cost->fullCost, constructor, "total");
    
    constructor.closeElement();
    constructor.closeElement();
    return constructor;
}
