#include "IndependentPlan.h"

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


PlanContext::PlanContext() : lastScopeMarker(0), currentTupleId(worldDataTupleId)
{
    TupleDefinition td(worldDataTupleId, "WorldData", xs_anyType);
    greatTupleScheme.insert(GreatMapRecord(td.tid, td));
}


PlanContext::~PlanContext()
{
    //
}

/*
void PlanContext::deleteFromPlan(RPBase* subj)
{
    typedef DependancyBackMap::iterator depmap_iterator;
    typedef DependancyMap::iterator depforemap_iterator;
    std::pair<depmap_iterator, depmap_iterator> range = tupleDependencyBackMap.equal_range(subj);

    for (depmap_iterator item = range.first; item != range.second; ++item) {
        std::pair<depforemap_iterator, depforemap_iterator> frange = tupleDependencyMap.equal_range(item->second);
        for (depforemap_iterator fitem = frange.first; fitem != frange.second; ++fitem) {
            if (fitem->second == item->first) {
                tupleDependencyMap.erase(fitem);
                break;
            }
        }
    }
    
    tupleDependencyBackMap.erase(subj);
    
    delete subj;
}

void PlanContext::deleteSubtree(RPBase* item)
{
    deleteFromPlan(item); // TODO
}
*/

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

RPBase* PlanContext::getExpressionResult(RPBase* tree)
{
    if (tree != null_op && tree->info()->resultType != orTupleCellList) {
        TupleId resultId = invalidTupleId;

/*        
        if (const Map * map = dynamic_cast<Map *>(tree)) {
            resultId = map->getVar();
        } else {
            // exception =)
        }
*/

        return new ItemReduce(tree, resultId);
    } else {
        return tree;
    }
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


