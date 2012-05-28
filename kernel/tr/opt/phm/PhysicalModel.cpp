#include "PhysicalModel.h"

#include <stdint.h>

#include "tr/opt/algebra/Predicates.h"
#include "tr/opt/path/XPathTypes.h"
#include "tr/opt/cost/Statistics.h"
#include "tr/opt/phm/Operations.h"
#include "tr/models/XmlConstructor.h"
#include "tr/strings/strings.h"
#include "tr/opt/SequenceModel.h"
#include "tr/opt/phm/ComparisonModels.h"

using namespace opt;

PlanInfo::PlanInfo(size_t initialTupleSetSize)
  : desc(0), parent(0), totalCost(0)
{
    initialTupleSet = new TupleChrysalis(initialTupleSetSize);
    branchList.resize(initialTupleSetSize, NULL);
};

PlanInfo::PlanInfo(const PlanInfo* parent, PlanDesc _desc)
    : desc(_desc), parent(parent->desc), opList(parent->opList), totalCost(0),
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

void PhysicalModel::updateBranch(POProt* op)
{
    TupleChrysalis* x = op->result;

    x->_width = 0;
    for (unsigned i = 0; i != x->tuples.size(); ++i) {
        if (x->get(i)->status == TupleValueInfo::evaluated) {
            plan->branchList.at(i) = op;
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

void* PhysicalModel::compile(PhantomPredicate* pred)
{
    U_ASSERT(false);
    return NULL;
}

void* PhysicalModel::compile(VPredicate* pred)
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

    updateBranch(result);
    plan->opList.push_back(result);

    return result;
}

void* PhysicalModel::compile(SPredicate* pred)
{
    enum strategy_t {
        impossible_to_evaluate,
        join,
        magic_join,
        evaluate_Right,
        evaluate_Left,
        evaluate_Left_then_Right,
        evaluate_Right_then_Left,
        evaluate_Left_then_join,
    };

    result = NULL;
    
    if (pred->path.horizontal()) {
        POProtIn leftOp = materialize(plan->getRef(pred->left()->absoluteIndex));
        POProtIn rightOp = materialize(plan->getRef(pred->right()->absoluteIndex));

        if (leftOp.op != NULL && rightOp.op != NULL) {
            if (leftOp.op == rightOp.op) {
                result = new FilterTuplePrototype(this, leftOp, rightOp, new PathComparisonPrototype(pred->path));
            } else {
                result = new MergeJoinPrototype(this, leftOp, rightOp, new PathComparisonPrototype(pred->path));
            };
//        } else
        // TODO: if free node available, make horizontal scan
        } else {
            return NULL;
        }
    } else {
        strategy_t strategy = impossible_to_evaluate;

        POProtIn leftOp = plan->getRef(pred->left()->absoluteIndex);
        POProtIn rightOp = plan->getRef(pred->right()->absoluteIndex);

        POProtIn leftCandidate, rightCandidate;

        do {
            if (leftOp.op != NULL && rightOp.op != NULL) {
                strategy = join;
            } else if (leftOp.op != NULL) {
                strategy = evaluate_Right;
            } else {
                bool pathInversable = pred->path.inversable();
                leftCandidate = doMaterialize(leftOp.index, false);

                if (rightOp.op != NULL) {
                    if (pathInversable) {
                        strategy = evaluate_Left;
                    } else {
                        strategy = evaluate_Left_then_join;
                    };
                } else {
                    U_ASSERT(leftOp.op == NULL && rightOp.op == NULL);
                    rightCandidate = doMaterialize(rightOp.index, false);

                    if (!pathInversable || rightCandidate.op == NULL) {
                        strategy = evaluate_Left_then_Right;
                    } else if (leftCandidate.op == NULL) {
                        strategy = evaluate_Right_then_Left;
                    } else {
                        strategy = magic_join;
                    };
                };
            }
        } while (false);

        switch(strategy) {
            case evaluate_Left_then_join:
                U_ASSERT(leftCandidate.op != NULL);
                plan->opList.push_back(leftCandidate.op);
                leftOp = leftCandidate;
            case join:
                if (leftOp.op == rightOp.op) {
                    result = new FilterTuplePrototype(this, leftOp, rightOp, new PathComparisonPrototype(pred->path));
                } else {
                    result = new MergeJoinPrototype(this, leftOp, rightOp, new PathComparisonPrototype(pred->path));
                };
                break;
            case magic_join:
/*              
                U_ASSERT(leftCandidate.op != NULL && rightCandidate.op != NULL);
                result = new MagicJoinPrototype(this, initialRef(leftOp.index), initialRef(rightOp.index), pred->path);
                break;
*/
            case evaluate_Left_then_Right:
                U_ASSERT(leftCandidate.op != NULL);
                plan->opList.push_back(leftCandidate.op);
                leftOp = leftCandidate;
            case evaluate_Right:
                result = new PathEvaluationPrototype(this, leftOp, initialRef(rightOp.index), pred->path);

                if (initialRef(rightOp.index)->node->producedFrom != pred) {
                    updateBranch(result);
                    plan->opList.push_back(result);
                    result = new ValidatePathPrototype(this, POProtIn(result, rightOp.index));
                }

                break;
            case evaluate_Right_then_Left:
                U_ASSERT(rightCandidate.op != NULL);
                plan->opList.push_back(rightCandidate.op);
                rightOp = rightCandidate;
            case evaluate_Left:
                U_ASSERT(pred->path.inversable());
                result = new PathEvaluationPrototype(this, rightOp, initialRef(leftOp.index), pred->path.inverse(pe::StepTest(pe::nt_any_kind, xsd::QNameAny)));
                updateBranch(result);
                plan->opList.push_back(result);
                result = new ValidatePathPrototype(this, POProtIn(result, leftOp.index));
                break;
            case impossible_to_evaluate:
                return NULL;
//                break;
        };
    }

    updateBranch(result);
    plan->opList.push_back(result);

    return result;
}


phop::ITupleOperator* PlanInfo::compile()
{
    return dynamic_cast<phop::ITupleOperator*>(opList.back()->getStatement());
}

#define CDGQNAME(N) xsd::QName::getConstantQName(NULL_XMLNS, N)

XmlConstructor & PlanInfo::toXML(XmlConstructor & element) const
{
    element.openElement(CDGQNAME("plan"));

    element.addAttributeValue(CDGQNAME("desc"), tuple_cell::atomic_int(desc));
    element.addAttributeValue(CDGQNAME("parent"), tuple_cell::atomic_int(parent));
    
    for (OperationList::const_iterator i = opList.begin(); i != opList.end(); ++i) {
        (*i)->toXML(element);
    };

    element.closeElement();

    return element;
}

inline static
XmlConstructor & rangeToElement(Range x, XmlConstructor & element, const char * name)
{
    element.openElement(CDGQNAME(name));

    element.addAttributeValue(CDGQNAME("low"), tuple_cell::atomic(x.lower));
    element.addAttributeValue(CDGQNAME("up"), tuple_cell::atomic(x.upper));
    element.addAttributeValue(CDGQNAME("avg"), tuple_cell::atomic(x.avg()));

    element.closeElement();

    return element;
};

XmlConstructor & POProt::__commonToXML(XmlConstructor & element) const
{
    for (std::vector<POProtIn>::const_iterator i = in.begin(); i != in.end(); ++i) {
        element.openElement(CDGQNAME("in"));
        element.addAttributeValue(CDGQNAME("ref"), tuple_cell::atomic_int((ptrdiff_t) (i->op)));
        element.closeElement();
    };

    for (std::vector<int>::const_iterator i = resultSet.begin(); i != resultSet.end(); ++i) {
        TupleRef tref(result, *i);
        element.openElement(CDGQNAME("result"));
        element.addAttributeValue(CDGQNAME("var"), tref->node->getName());
        element.closeElement();
    };

    return element;
}

XmlConstructor & TupleChrysalis::__toXML(XmlConstructor & element) const
{
    element.openElement(CDGQNAME("tuple"));

    element.addAttributeValue(CDGQNAME("count"), tuple_cell::atomic(rowCount.avg()));
    element.addAttributeValue(CDGQNAME("size"), tuple_cell::atomic(rowSize.avg()));
    element.addAttributeValue(CDGQNAME("width"), tuple_cell::atomic_int(_width));

    element.closeElement();
    
    return element;
};

XmlConstructor & POProt::__toXML(XmlConstructor & element) const
{
    this->result->__toXML(element);

    return element;
}

XmlConstructor & POProt::toXML(XmlConstructor & element) const
{
    element.openElement(CDGQNAME(this->getProtInfo()->name));
    element.addAttributeValue(CDGQNAME("id"), tuple_cell::atomic_int((ptrdiff_t) (this)));

    __commonToXML(element);
    __toXML(element);

    element.openElement(CDGQNAME("cost"));

    rangeToElement(this->cost->firstCost, element, "first");
    rangeToElement(this->cost->nextCost, element, "next");
    rangeToElement(this->cost->fullCost, element, "total");
    
    element.closeElement();
    element.closeElement();
    return element;
}
