#include "IndependentPlan.h"

#include "tr/models/XmlConstructor.h"
#include "tr/opt/functions/Functions.h"

#include <set>

using namespace rqp;
using namespace opt;

int RPBase::opids = 0;
PlanContext * rqp::PlanContext::current = NULL;
const opt::TupleScheme empty_tuple_set;

#define ABSTRACT_OPERATION_INFO(C)

#define OPERATION_INFO(C, TT) \
const opdesc_t C::sopdesc = {#C, C::opid, TT}; \

//void C::__init() { opdesc = &C::sopdesc; };

ABSTRACT_OPERATION_INFO(ConstantOperation)
ABSTRACT_OPERATION_INFO(ListOperation)
ABSTRACT_OPERATION_INFO(BinaryOperation)
ABSTRACT_OPERATION_INFO(NestedOperation)


OPERATION_INFO(ItemReduce, rqp::ofNone)
OPERATION_INFO(MapConcat, rqp::oReturnTuple)
OPERATION_INFO(SequenceConcat, rqp::oReturnTuple)
OPERATION_INFO(If, rqp::oReturnTuple)

OPERATION_INFO(VarIn, rqp::oBlockBuilder)
OPERATION_INFO(Const, rqp::oBlockBuilder)
OPERATION_INFO(XPathStep, rqp::oBlockBuilder)
OPERATION_INFO(Select, rqp::oBlockBuilder)

OPERATION_INFO(ComparisonExpression, rqp::oBlockBuilder)
OPERATION_INFO(FunCall, rqp::oBlockSpecial)
OPERATION_INFO(Construct, rqp::ofNone)
OPERATION_INFO(Sequence, rqp::oBlockSpecial)

OPERATION_INFO(DataGraphOperation, rqp::oBlockSpecial)
OPERATION_INFO(MapGraph, rqp::oBlockSpecial)

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
    if (leftList != null_op) {
        leftList->toXML(element);
    }

    if (rightList != null_op) {
        rightList->toXML(element);
    }

    return element;
}

XmlConstructor& ConstantOperation::__toXML(XmlConstructor& element) const
{
    return element;
}

XmlConstructor& ListOperation::__toXML(XmlConstructor& element) const
{
    if (list != null_op) {
        list->toXML(element);
    }

    return element;
}

XmlConstructor& NestedOperation::__toXML(XmlConstructor& element) const
{
    element.openElement(CDGQNAME("nested"));
    
    if (subplan != null_op) {
        subplan->toXML(element);
    }

    element.closeElement();

    return rqp::ListOperation::__toXML(element);
}


XmlConstructor& ItemReduce::__toXML(XmlConstructor& element) const
{
    element.addAttributeValue(CDGQNAME("tuple"), tuple_cell::atomic_int(tid));
    element.addAttributeValue(CDGQNAME("name"), getContext()->getVarDef(tid)->getVarLabel() );
    return rqp::ListOperation::__toXML(element);
};

XmlConstructor& MapConcat::__toXML(XmlConstructor& element) const
{
    element.addAttributeValue(CDGQNAME("tuple"), tuple_cell::atomic_int(tid));
    element.addAttributeValue(CDGQNAME("name"), getContext()->getVarDef(tid)->getVarLabel() );
    return rqp::NestedOperation::__toXML(element);
};

XmlConstructor& SequenceConcat::__toXML(XmlConstructor& element) const
{
    element.addAttributeValue(CDGQNAME("tuple"), tuple_cell::atomic_int(tid));
    element.addAttributeValue(CDGQNAME("name"), getContext()->getVarDef(tid)->getVarLabel() );
    return rqp::NestedOperation::__toXML(element);
};

XmlConstructor& If::__toXML(XmlConstructor& element) const
{
    element.openElement(CDGQNAME("condition"));

    if (condition != null_op) {
        condition->toXML(element);
    }

    element.closeElement();

    element.openElement(CDGQNAME("then"));
    
    if (thenBranch != null_op) {
        thenBranch->toXML(element);
    }
    
    element.closeElement();

    element.openElement(CDGQNAME("else"));
    
    if (elseBranch != null_op) {
        elseBranch->toXML(element);
    }

    element.closeElement();

    return element;
};

XmlConstructor& VarIn::__toXML(XmlConstructor& element) const
{
    element.addAttributeValue(CDGQNAME("tuple"), tuple_cell::atomic_int(tid));
    element.addAttributeValue(CDGQNAME("name"), getContext()->getVarDef(tid)->getVarLabel() );
    return rqp::ConstantOperation::__toXML(element);
};

XmlConstructor& Const::__toXML(XmlConstructor& element) const
{
    for (MemoryTupleSequence::const_iterator it = sequence->begin(); it != sequence->end(); ++it) {
        element.addElementValue(CDGQNAME("value"), *it);
    };

    return rqp::ConstantOperation::__toXML(element);
};

XmlConstructor& XPathStep::__toXML(XmlConstructor& element) const
{
    element.addElementValue(CDGQNAME("step"), step.toXPathString());
    return rqp::ListOperation::__toXML(element);
};

XmlConstructor& Select::__toXML(XmlConstructor& element) const
{
    return rqp::NestedOperation::__toXML(element);
};

XmlConstructor& ComparisonExpression::__toXML(XmlConstructor& element) const
{
    element.addElementValue(CDGQNAME("cmp"), cmp.toLRString());
    return rqp::BinaryOperation::__toXML(element);
};

XmlConstructor& ManyChildren::__toXML(XmlConstructor& element) const
{
    return element;
}


XmlConstructor& FunCall::__toXML(XmlConstructor& element) const
{
    element.addAttributeValue(CDGQNAME("name"), name.getColonizedName());

    int i = 0;
    for (OperationList::const_iterator it = opList.begin(); it != opList.end(); ++it) {
        element.openElement(CDGQNAME("param"));
        element.addAttributeValue(CDGQNAME("index"), tuple_cell::atomic_int(i++));
        
        if (*it != null_op) {
            (*it)->toXML(element);
        }

        element.closeElement();
    };
    
    return rqp::ManyChildren::__toXML(element);
};

XmlConstructor& Construct::__toXML(XmlConstructor& element) const
{
    element.addElementValue(CDGQNAME("type"), type2string(type));

    if (name != null_op) {
        element.openElement(CDGQNAME("name"));
        name->toXML(element);
        element.closeElement();
    }

    return rqp::ListOperation::__toXML(element);
};

XmlConstructor& Sequence::__toXML(XmlConstructor& element) const
{
    for (OperationList::const_iterator it = opList.begin(); it != opList.end(); ++it) {
        if (*it != null_op) {
            (*it)->toXML(element);
        }
    };

    return rqp::ManyChildren::__toXML(element);
};

XmlConstructor& DataGraphOperation::__toXML(XmlConstructor& element) const
{
    element.openElement(CDGQNAME("graph"));
    func->toXML(element);
    element.closeElement();

    element.openElement(CDGQNAME("suboperations"));
    for (OperationList::const_iterator it = opList.begin(); it != opList.end(); ++it) {
        if (*it != null_op) {
            (*it)->toXML(element);
        }
    };
    element.closeElement();

    return rqp::ManyChildren::__toXML(element);
};

XmlConstructor& MapGraph::__toXML(XmlConstructor& element) const
{
    rqp::DataGraphOperation::__toXML(element);
  
    if (list != null_op) {
        list->toXML(element);
    }

    return element;
};


void ConstantOperation::getChildren(OperationList& children) const
{
}

void BinaryOperation::getChildren(OperationList& children) const
{
    children.push_back(leftList);
    children.push_back(rightList);
}

void Construct::getChildren(OperationList& children) const
{
    children.push_back(name);
    rqp::ListOperation::getChildren(children);
}

void If::getChildren(OperationList& children) const
{
    children.push_back(condition);
    children.push_back(thenBranch);
    children.push_back(elseBranch);
}

void ListOperation::getChildren(OperationList& children) const
{
    children.push_back(list);
}

void ManyChildren::getChildren(OperationList& _children) const
{
    _children.insert(_children.end(), opList.begin(), opList.end());
}

void NestedOperation::getChildren(OperationList& children) const
{
    children.push_back(subplan);
    rqp::ListOperation::getChildren(children);
}

void MapGraph::getChildren(OperationList& children) const
{
    rqp::ManyChildren::getChildren(children);
    children.push_back(list);
}




void FunCall::resolve()
{
    // TODO : remove this
    phop::initializeFunctionLibrary();
    function = phop::functionLibrary->findFunction(getName());
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

void PlanContext::replaceOperation(RPBase* a, RPBase* b)
{
    LinkMap::iterator i = linkmap.find(a);
    U_ASSERT(i != linkmap.end());
    
    RPBase ** aptr = i->second;
    linkmap.erase(i);
    *aptr = b;
    linkmap.insert(LinkMap::value_type(b, aptr));
}

void PlanContext::replaceLink(RPBase* a, RPBase** aptr)
{
    LinkMap::iterator i = linkmap.find(a);
    U_ASSERT(i != linkmap.end());
    i->second = aptr;
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


