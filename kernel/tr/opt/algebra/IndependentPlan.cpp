
#include "IndependentPlan.h"

#include <set>

using namespace rqp;

int RPBase::opids = 0;
TupleScheme empty_tuple_set;

#define ABSTRACT_OPERATION_INFO(C) 

#define OPERATION_INFO(C, n, serial, result_type) \
const opdesc_t C::sopdesc = {n, serial, result_type}; \
void C::__init() { opdesc = &C::sopdesc; };\

ABSTRACT_OPERATION_INFO(ConstantOperation)
ConstantOperation::ConstantOperation(PlanContext* context_)
  : RPBase(context_) {};

ABSTRACT_OPERATION_INFO(ListOperation)
ListOperation::ListOperation(PlanContext* context_, RPBase* list_)
  : RPBase(context_), list(list_) {};

ABSTRACT_OPERATION_INFO(BinaryOperation)
BinaryOperation::BinaryOperation(PlanContext* context_, RPBase* ltItem_, RPBase* rtItem_)
  : RPBase(context_), leftList(ltItem_), rightList(rtItem_) {};

ABSTRACT_OPERATION_INFO(NestedOperation)
NestedOperation::NestedOperation(PlanContext* context_, RPBase* list_, RPBase* subplan_)
  : ListOperation(context_, list_), subplan(subplan_) {};


OPERATION_INFO(Map, "Map", 0x002, orTupleList)
Map::Map(PlanContext* context_, TupleId tupleid, RPBase* function_, RPBase* list_)
  : NestedOperation(context_, list_, function_), mapsTo(tupleid) 
{ __init(); };


OPERATION_INFO(MapAll, "MapAll", 0x100, orTupleList)
MapAll::MapAll(PlanContext* context_, RPBase* opin_, RPBase* function_, TupleId iterateTuple, TupleId resultTuple): 
  Map(context_, opin_, function_, iterateTuple, resultTuple)
{ __init(); }

OPERATION_INFO(Functional, "Functional", 0x101, orTupleList)
Functional::Functional(PlanContext* context_, DataGraph* function_): 
  RPBase(context_), func(function_)
{ __init(); };


DataGraph* Map::apply(DataGraph* in)
{
    return context->dataGraphFactory.map(subplan->apply(in), list->apply(in));
}



/*
OPERATION_INFO(MapProduct, "MapProduct", 0x003, orTupleList)
MapProduct::MapProduct(PlanContext* context_, TupleId tupleid, RPBase* function_, RPBase* list_)
  : Map(context_, tupleid, function_, list_)
{ __init(); };

OPERATION_INFO(Product, "Product", 0x021, orTupleList)
Product::Product(PlanContext* context_, RPBase* left_, RPBase* right_)
  : BinaryOperation(context_, left_, right_)
{ __init(); };

OPERATION_INFO(SemiJoin, "SemiJoin", 0x004, orTupleList)
SemiJoin::SemiJoin(PlanContext* context_, TupleId tupleid, RPBase* function_, RPBase* list_)
  : Map(context_, tupleid, function_, list_)
{ __init(); };

OPERATION_INFO(Select, "Select", 0x005, orTupleList)
Select::Select(PlanContext* context_, RPBase* predicate_, RPBase* list_, const rqp::TupleScheme& subjects)
  : NestedOperation(context_, predicate_, list_), subjectNodes(subjects)
{ __init(); };

OPERATION_INFO(Let, "Let", 0x010, orTupleList)
Let::Let(PlanContext* context_, TupleId tupleid, RPBase* list_)
  : ListOperation(context_, list_), mapsTo(tupleid)
{ __init(); };

OPERATION_INFO(Reduce, "Reduce", 0x006, orTupleList)
Reduce::Reduce(PlanContext* context_, const rqp::TupleScheme& reduce_, RPBase* list_)
  : ListOperation(context_, list_), reduceList(reduce_)
{ __init(); };

OPERATION_INFO(Collect, "Collect", 0x007, orTupleList)
Collect::Collect(PlanContext* context_, const rqp::TupleScheme& collect_, RPBase* list_)
  : ListOperation(context_, list_), collectList(collect_)
{ __init(); };

OPERATION_INFO(ItemReduce, "ItemReduce", 0x008, orTupleCellList)
ItemReduce::ItemReduce(PlanContext* context_, TupleId tid_, RPBase* list_)
  : ListOperation(context_, list_), tupleid(tid_)
{ __init(); };

OPERATION_INFO(Enumerate, "Enumerate", 0x009, orTupleList)
Enumerate::Enumerate(PlanContext* context_, TupleId tid_, RPBase* list_)
  : ListOperation(context_, list_), countVar(tid_)
{ __init(); };

OPERATION_INFO(GeneralComparison, "GeneralComparison", 0x020, orTupleCellList)
GeneralComparison::GeneralComparison(PlanContext* context_, RPBase* ltItem_, RPBase* rtItem_, GeneralComparison::OpType op_)
  : BinaryOperation(context_, ltItem_, rtItem_), op(op_)
{ __init(); };

OPERATION_INFO(AtomicConstructor, "AtomicConstructor", 0x030, orTupleCellList)
AtomicConstructor::AtomicConstructor(PlanContext* context_)
  : ConstantOperation(context_)
{ __init(); };
*/

/*
OPERATION_INFO(PathExpressionTemplate, "XPath", 0x101, orTupleList)
OPERATION_INFO(ByValueTemplate, "byValue", 0x102, orTupleList)
OPERATION_INFO(JoinTemplate, "Join", 0x103, orTupleList)
*/

PlanContext::PlanContext() : lastScopeMarker(0), currentTupleId(worldDataTupleId)
{
    TupleCellType tt = {};
    TupleDefinition td(worldDataTupleId, "WorldData", tt);
    greatTupleScheme.insert(GreatMapRecord(td.tid, td));
}


PlanContext::~PlanContext()
{
    for (std::vector<void *>::const_iterator p = temporaryData.begin(); p != temporaryData.end(); p++) {
        free(*p);
    }
}

RPBase* PlanContext::createAxisStep(xpath::NodeTest* nodeTest, TupleId tid)
{
    U_ASSERT(false);
//    return new Functional(this, );
}


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
    TupleCellType tt = {};
    TupleDefinition td(++currentTupleId, tt);
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
    if (tree != null_op && tree->getOperationResultType() != orTupleCellList) {
        TupleId resultId = invalidTupleId;
        
        if (const Map * map = dynamic_cast<Map *>(tree)) {
            resultId = map->getVar();
        } else {
            // exception =)
        }
      
        return new ItemReduce(this, resultId, tree);
    } else {
        return tree;
    }
}

void ListOperation::collectSupplyTuples(TupleScheme& ts)
{
    if (list != null_op) {
        list->collectSupplyTuples(ts);
    }
}

void BinaryOperation::collectSupplyTuples(TupleScheme& ts)
{
    if (leftList != null_op) {
        leftList->collectSupplyTuples(ts);
    }
    
    if (rightList != null_op) {
        rightList->collectSupplyTuples(ts);
    }
}

void NestedOperation::collectSupplyTuples(TupleScheme& ts)
{
    subplan->collectSupplyTuples(ts);
    ListOperation::collectSupplyTuples(ts);
}


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



