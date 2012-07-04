#include "IndependentPlan.h"

#include "tr/models/XmlConstructor.h"
#include "tr/opt/functions/Functions.h"
#include "tr/opt/graphs/DataGraphCollection.h"
#include "tr/opt/graphs/DataGraphs.h"

#include <set>

using namespace rqp;
using namespace opt;

int RPBase::opids = 0;
PlanContext * rqp::PlanContext::current = NULL;
const opt::TupleScheme empty_tuple_set;

void RPBase::replace(RPBase* op, RPBase* with)
{
    for (OperationList::iterator it = children.begin(); it != children.end(); ++it) {
        if (*it == op) {
            *it = with;
            return;
        }
    };

    U_ASSERT(false);
}

XmlConstructor& RPBase::toXML(XmlConstructor& element) const
{
    element.openElement(CDGQNAME(info()->opname));
    element.addAttributeValue(CDGQNAME("id"), tuple_cell::atomic_int(opuid));
    __toXML(element);
    element.closeElement();

    return element;
}

XmlConstructor& BinaryOperation::__toXML(XmlConstructor& element) const
{
    if (getLeft() != null_op) {
        getLeft()->toXML(element);
    }

    if (getRight() != null_op) {
        getRight()->toXML(element);
    }

    return element;
}

XmlConstructor& ConstantOperation::__toXML(XmlConstructor& element) const
{
    return element;
}

XmlConstructor& ListOperation::__toXML(XmlConstructor& element) const
{
    if (getList() != null_op) {
        getList()->toXML(element);
    }

    return element;
}

XmlConstructor& NestedOperation::__toXML(XmlConstructor& element) const
{
    element.addAttributeValue(CDGQNAME("tuple"), tuple_cell::atomic_int(tid));
    element.addAttributeValue(CDGQNAME("name"), getContext()->getVarDef(tid)->getVarLabel() );

    element.openElement(CDGQNAME("nested"));
    
    if (getSubplan() != null_op) {
        getSubplan()->toXML(element);
    }

    element.closeElement();

    return rqp::ListOperation::__toXML(element);
}

XmlConstructor& ManyChildren::__toXML(XmlConstructor& element) const
{
    for (OperationList::const_iterator it = children.begin(); it != children.end(); ++it) {
        if (*it != null_op) {
            (*it)->toXML(element);
        }
    };

    return element;
}

PlanContext::PlanContext() : lastScopeMarker(0), currentTupleId(worldDataTupleId)
{
    TupleDefinition td(worldDataTupleId, "WorldData", xs_anyType);
    greatTupleScheme.insert(GreatMapRecord(td.tid, td));
    dataGraphFactory = new DataGraphMaster();
}


PlanContext::~PlanContext()
{
    delete dataGraphFactory;
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

TupleId PlanContext::generateTupleIdVarScoped(const std::string & varName)
{
    TupleDefinition td(++currentTupleId, varName);
    
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


