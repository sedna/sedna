#include "IndependentPlan.h"

#include "tr/models/XmlConstructor.h"

#include <set>

using namespace rqp;
using namespace opt;

int RPBase::opids = 0;
PlanContext * rqp::PlanContext::current = NULL;
const opt::TupleScheme empty_tuple_set;

#define ABSTRACT_OPERATION_INFO(C)

#define OPERATION_INFO(C, result_type) \
const opdesc_t C::sopdesc = {#C, C::opid, result_type}; \

//void C::__init() { opdesc = &C::sopdesc; };

ABSTRACT_OPERATION_INFO(ConstantOperation)
ABSTRACT_OPERATION_INFO(ListOperation)
ABSTRACT_OPERATION_INFO(BinaryOperation)
ABSTRACT_OPERATION_INFO(NestedOperation)


OPERATION_INFO(ItemReduce, orTupleCellList)
OPERATION_INFO(MapConcat, orTupleList)
OPERATION_INFO(SequenceConcat, orTupleList)
OPERATION_INFO(If, orTupleList)

OPERATION_INFO(VarIn, orTupleCellList)
OPERATION_INFO(Const, orTupleCellList)
OPERATION_INFO(XPathStep, orTupleCellList)
OPERATION_INFO(Select, orTupleCellList)

OPERATION_INFO(ComparisonExpression, orTupleCellList)
OPERATION_INFO(FunCall, orTupleCellList)



XmlConstructor& RPBase::toXML(XmlConstructor& element) const
{
    element.openElement(CDGQNAME(info()->opname));
    __toXML(element);
    element.closeElement();

    return element;
}

XmlConstructor& BinaryOperation::__toXML(XmlConstructor& element) const
{
    leftList->toXML(element);
    rightList->toXML(element);
    
    return element;
}

XmlConstructor& ConstantOperation::__toXML(XmlConstructor& element) const
{
    return element;
}

XmlConstructor& ListOperation::__toXML(XmlConstructor& element) const
{
    list->toXML(element);

    return element;
}

XmlConstructor& NestedOperation::__toXML(XmlConstructor& element) const
{
    element.openElement(CDGQNAME("nested"));
    subplan->toXML(element);
    element.closeElement();

    return rqp::ListOperation::__toXML(element);
}


XmlConstructor& ItemReduce::__toXML(XmlConstructor& element) const {
    element.addAttributeValue(CDGQNAME("tuple"), tuple_cell::atomic_int(tid));
    return rqp::ListOperation::__toXML(element);
};

XmlConstructor& MapConcat::__toXML(XmlConstructor& element) const {
    element.addAttributeValue(CDGQNAME("tuple"), tuple_cell::atomic_int(tid));
    return rqp::NestedOperation::__toXML(element);
};

XmlConstructor& SequenceConcat::__toXML(XmlConstructor& element) const {
    element.addAttributeValue(CDGQNAME("tuple"), tuple_cell::atomic_int(tid));
    return rqp::NestedOperation::__toXML(element);
};

XmlConstructor& If::__toXML(XmlConstructor& element) const {
    element.openElement(CDGQNAME("if"));
    condition->toXML(element);
    element.closeElement();

    element.openElement(CDGQNAME("then"));
    thenBranch->toXML(element);
    element.closeElement();

    element.openElement(CDGQNAME("else"));
    elseBranch->toXML(element);
    element.closeElement();

    return element;
};

XmlConstructor& VarIn::__toXML(XmlConstructor& element) const {
    element.addAttributeValue(CDGQNAME("tuple"), tuple_cell::atomic_int(tid));
    return rqp::ConstantOperation::__toXML(element);
};

XmlConstructor& Const::__toXML(XmlConstructor& element) const {
    for (MemoryTupleSequence::const_iterator it = sequence->begin(); it != sequence->end(); ++it) {
        element.addElementValue(CDGQNAME("value"), *it);
    };

    return rqp::ConstantOperation::__toXML(element);
};

XmlConstructor& XPathStep::__toXML(XmlConstructor& element) const {
    element.addElementValue(CDGQNAME("step"), step.toXPathString());
    return rqp::ListOperation::__toXML(element);
};

XmlConstructor& Select::__toXML(XmlConstructor& element) const {
    return rqp::NestedOperation::__toXML(element);
};

XmlConstructor& ComparisonExpression::__toXML(XmlConstructor& element) const {
    element.addElementValue(CDGQNAME("cmp"), cmp.toLRString());
    return rqp::BinaryOperation::__toXML(element);
};

XmlConstructor& FunCall::__toXML(XmlConstructor& element) const {
    element.addElementValue(CDGQNAME("fun"), name);
    return rqp::ConstantOperation::__toXML(element);
};



PlanContext::PlanContext() : lastScopeMarker(0), currentTupleId(worldDataTupleId)
{
    TupleDefinition td(worldDataTupleId, "WorldData", xs_anyType);
    greatTupleScheme.insert(GreatMapRecord(td.tid, td));
}


PlanContext::~PlanContext()
{
    //
}

void PlanContext::newScope() {
    scopeStack.push(invalidTupleId);
    ++lastScopeMarker;
}

void PlanContext::clearScope()
{
    while (scopeStack.top() != invalidTupleId) {
        scope.erase(greatTupleScheme.at(scopeStack.top()).name);
        scopeStack.pop();
    }
    --lastScopeMarker;
}

ScopeMarker PlanContext::setScopeMarker()
{
    return lastScopeMarker;
}

void PlanContext::clearScopesToMarker(ScopeMarker marker)
{
    while (lastScopeMarker > marker) {
        clearScope();
    }
}

TupleId PlanContext::generateTupleId()
{
    TupleDefinition td(++currentTupleId, (xmlscm_type) xs_anyType);
    greatTupleScheme.insert(GreatMapRecord(td.tid, td));
    
    return td.tid;
}

TupleId PlanContext::generateTupleIdVarScoped(TupleVarDescriptor* var)
{
    TupleDefinition td(++currentTupleId, var);
    
    greatTupleScheme.insert(GreatMapRecord(td.tid, td));
    scope.insert(VarMapRecord(td.name, td.tid));
    scopeStack.push(td.tid);
    
    return td.tid;
}

TupleId PlanContext::getVarTupleInScope(const std::string& canonicalName)
{
    return scope.at(canonicalName);
}


/*
TupleScheme * PlanContext::newMapExtend(TupleScheme * in, const TupleScheme * with) {
    TupleScheme * ts;
    
    if (in != NULL) {
        ts = new TupleScheme(*in);
    } else {
        ts = new TupleScheme();
    }
    
    if (with != NULL) {
        ts->insert(with->begin(), with->end());
    }
    
    tupleSchemeStorage.push_back(ts);
    return ts;
}

TupleScheme * PlanContext::newMapExtendSingle(TupleScheme * in, TupleId with) {
    TupleScheme * ts;
    
    if (in != NULL) {
        ts = new TupleScheme(*in);
    } else {
        ts = new TupleScheme();
    }
    
    ts->insert(with);
    tupleSchemeStorage.push_back(ts);
    return ts;
}
*/


