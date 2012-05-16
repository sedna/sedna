/*
 * File:  OptimizablePlan.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _OPT_PLAN_H_
#define _OPT_PLAN_H_

#include <map>
#include <vector>
#include <set>
#include <string>
#include <sstream>
#include <stack>

#include "tr/structures/xmlns.h"

#include "tr/opt/algebra/PlanOperations.h"
#include "tr/opt/algebra/TupleScheme.h"
#include "tr/opt/algebra/DataGraph.h"
#include "tr/opt/algebra/Predicates.h"

/* Look PlanOperationTypes for description of all operators */

namespace opt {
    class DataGraphMaster;
    class DataGraph;
}

namespace rqp {

class PlanVisitor;
  
struct opdesc_t {
    const char * opname;
    int opType;
    operation_result_type_t resultType;
};

typedef const opdesc_t * _opinfo_t;

typedef int ScopeMarker;
typedef std::stack<opt::TupleId> ScopeStack;
typedef std::map<std::string, opt::TupleId> VarNameMap;
typedef std::map<opt::TupleId, TupleDefinition> GreatTupleScheme;

typedef VarNameMap::value_type VarMapRecord;
typedef GreatTupleScheme::value_type GreatMapRecord;

/*
typedef std::multimap<TupleId, RPBase *> DependancyMap;
typedef std::pair<DependancyMap::const_iterator, DependancyMap::const_iterator> DependancyMapRange;
typedef std::multimap<RPBase *, opt::TupleId> DependancyBackMap;
typedef std::vector<TupleScheme *> TupleSchemeStorage;
typedef std::vector<RPBase *> OperationList;
*/



class TupleDependancyMap {
//        DependancyMap tupleDependencyMap;
//        DependancyBackMap tupleDependencyBackMap;

/*
    void addDependancy(opt::TupleId tid, RPBase * item) {
        tupleDependencyMap.insert(std::pair<TupleId, RPBase *>(tid, item));
        tupleDependencyBackMap.insert(std::pair<RPBase *, TupleId>(item, tid));
    };
*/
};

class PlanContext : public opt::IPlanDisposable {
  private:
    ScopeMarker lastScopeMarker;
    opt::TupleId currentTupleId;
    GreatTupleScheme greatTupleScheme;

    ScopeStack scopeStack;
    VarNameMap scope;
  public:
    static PlanContext * current;

    opt::DataGraphMaster * dataGraphFactory;

    PlanContext();
    ~PlanContext();

//        TupleSchemeStorage tupleSchemeStorage;
//        OperationList subplans;

//        TupleScheme * newMapExtend(TupleScheme * in, const TupleScheme * with);
//        TupleScheme * newMapExtendSingle(TupleScheme * in, TupleId with);

    const TupleDefinition * getVarDef(opt::TupleId tid) const { return &(greatTupleScheme.at(tid)); };

    opt::TupleId generateTupleId();
    opt::TupleId generateTupleIdVarScoped(TupleVarDescriptor * var);
    opt::TupleId getVarTupleInScope(const std::string & canonicalName);

    ScopeMarker setScopeMarker();

    void newScope();
    void clearScope();
    void clearScopesToMarker(ScopeMarker marker);

    RPBase * getExpressionResult(RPBase * tree);

    void deleteFromPlan(RPBase * item);
    void deleteSubtree(RPBase * item);
};

class RPBase : public opt::IPlanDisposable {
  protected:
    _opinfo_t opdesc;
  public:
    virtual ~RPBase() {};
  protected:
    PlanContext * context;
    static int opids;
    int opid;
  public:
    RPBase(_opinfo_t op) : opdesc(op), context(PlanContext::current), opid(opids++) {};

    inline const opdesc_t * info() const { return opdesc; };
    inline bool isType(int t) const { return opdesc->opType == t; };
    PlanContext * getContext() const { return context; };
};

template<class T> inline static
bool instanceof(RPBase * op) { return op != null_op && op->info()->opType == &(T::opid); };

/* YES! I know about typeid operator existance. */

#define ABSTRACT_OPERATION \

#define OPERATION(ID) \
public:\
  static const opdesc_t sopdesc; \
  enum _opid_t { opid = (ID) }; \
private:

#define PROPERTY(name, t, member) t get##name() const { return member; } void set##name(t value) { member = value; }
#define PROPERTY_RO(name, t, member) t get##name() const { return member; }
#define PROPERTY_WO(name, t, member) void set##name(t value) { member = value; }

/* Basic operation classes, devided by arity */

/* 0r-operations */
class ConstantOperation : public RPBase {
    ABSTRACT_OPERATION
  public:
    ConstantOperation(_opinfo_t op)
      : RPBase(op) {};
};

/* 1r-operations */
class ListOperation : public RPBase {
    ABSTRACT_OPERATION
  protected:
    RPBase * list;
  public:
    ListOperation(_opinfo_t op, RPBase * list_)
      : RPBase(op), list(list_) {};

    PROPERTY_RO(List, RPBase *, list)
};

/* 1r-operations with independent nested operation plan */
class NestedOperation : public ListOperation {
    ABSTRACT_OPERATION
  protected:
    RPBase * subplan;
  public:
    NestedOperation(_opinfo_t op, RPBase * list_, RPBase * subplan_)
      : ListOperation(op, list_), subplan(subplan_) {};

    PROPERTY_RO(Subplan, RPBase *, subplan)
};

/* 2r-operations */
class BinaryOperation : public RPBase {
    ABSTRACT_OPERATION
  private:
    RPBase * leftList;
    RPBase * rightList;
  public:
    BinaryOperation(_opinfo_t op, RPBase * ltItem_, RPBase * rtItem_)
      : RPBase(op), leftList(ltItem_), rightList(rtItem_) {};
  public:
    PROPERTY_RO(Left, RPBase *, leftList)
    PROPERTY_RO(Right, RPBase *, rightList)
};

class ItemReduce : public ListOperation {
    OPERATION(0x001)
    opt::TupleId tid;
public:
    ItemReduce(RPBase* list_, opt::TupleId _tid)
      : ListOperation(&sopdesc, list_), tid(_tid) {};

    PROPERTY_RO(Tuple, opt::TupleId, tid)
};

class MapConcat : public NestedOperation {
    OPERATION(0x002)
    opt::TupleId tid;
public:
    MapConcat(RPBase* _list, RPBase* _subplan, opt::TupleId _tid)
      : NestedOperation(&sopdesc, _list, _subplan), tid(_tid) {};

    PROPERTY_RO(Tuple, opt::TupleId, tid)
};

class SequenceConcat : public NestedOperation {
    OPERATION(0x003)
    opt::TupleId tid;
public:
    SequenceConcat(RPBase* _list, RPBase* _subplan, opt::TupleId _tid)
      : NestedOperation(&sopdesc, _list, _subplan), tid(_tid) {};

    PROPERTY_RO(Tuple, opt::TupleId, tid)
};


class VarIn : public ConstantOperation {
    OPERATION(0x011)
    opt::TupleId tid;
public:
    VarIn(opt::TupleId _tid)
      : ConstantOperation(&sopdesc), tid(_tid) {};

    PROPERTY_RO(Tuple, opt::TupleId, tid)
};

class Const : public ConstantOperation {
    OPERATION(0x012)
    opt::MemoryTupleSequencePtr sequence;
public:
    Const(opt::MemoryTupleSequencePtr _sequence)
      : ConstantOperation(&sopdesc), sequence(_sequence) {};
  
    PROPERTY_RO(Sequence, opt::MemoryTupleSequencePtr, sequence)
};

class XPathStep : public ListOperation {
    OPERATION(0x013)
    pe::Step step;
public:
    XPathStep(RPBase* _in, const pe::Step & _step)
      : ListOperation(&sopdesc, _in), step(_step) {};

    PROPERTY_RO(Step, pe::Step, step)
};

class Select : public NestedOperation {
    OPERATION(0x004)
public:
    Select(RPBase* _list, RPBase* _subplan)
      : NestedOperation(&sopdesc, _list, _subplan) {};
};

class If : public RPBase {
    OPERATION(0x015)
private:
    RPBase * condition;
    RPBase * thenBranch;
    RPBase * elseBranch;
public:
    If(RPBase* _condition, RPBase* _then, RPBase* _else)
      : RPBase(&sopdesc), condition(_condition), thenBranch(_then), elseBranch(_else) {};

    PROPERTY_RO(Condition, RPBase *, condition)
    PROPERTY_RO(Then, RPBase *, thenBranch)
    PROPERTY_RO(Else, RPBase *, elseBranch)
};


class ComparisonExpression : public BinaryOperation {
    OPERATION(0x016)
    opt::Comparison cmp;
public:
    ComparisonExpression(RPBase* _left, RPBase* _right, const opt::Comparison &_cmp)
      : BinaryOperation(&sopdesc, _left, _right), cmp(_cmp) {};

    PROPERTY_RO(Op, opt::Comparison, cmp)
};

typedef std::vector< RPBase * > OperationList;

class FunCall : public ConstantOperation {
    OPERATION(0x017)
    std::string name;
    OperationList opList;
public:
    FunCall(std::string fname, const OperationList & _oplist)
      : ConstantOperation(&sopdesc), name(fname), opList(_oplist) {};

    PROPERTY_RO(Operations, const OperationList &, opList)
};

/*
class Let : public ListOperation {
    OPERATION
  protected:
    TupleId mapsTo;
  public:
    Let(PlanContext * context_, TupleId tupleid, RPBase * list_);
  public:
    PROPERTY_RO(Var, TupleId, mapsTo)
};
*/
/*
class Map : public NestedOperation {
    OPERATION(0x010)
  protected:
    opt::TupleId mapsTo;
  public:
    Map(PlanContext * context_, RPBase * opin_, RPBase * function_, opt::TupleId iterateTuple, opt::TupleId resultTuple);
  public:
    PROPERTY_RO(Var, TupleId, mapsTo)
};

class MapAll : public Map {
    OPERATION(0x011)
  public:
    MapAll(PlanContext * context_, RPBase * opin_, RPBase * function_, opt::TupleId iterateTuple, opt::TupleId resultTuple);
};
*/

/*
class DataNavigationGraph : public RPBase {
    OPERATION(0x012)
  protected:
    DataGraph * func;
  public:
    DataNavigationGraph(PlanContext * context_, DataGraph * function_);
};

class Serialize : public RPBase {
    OPERATION(0x013)
  protected:
    RPBase * opin_;
  public:
    Serialize(PlanContext * context_, opt::TupleId iterateTuple, RPBase * opin_);
};
*/

#undef ABSTRACT_OPERATION
#undef OPERATION
#undef PROPERTY
#undef PROPERTY_RO
#undef PROPERTY_WO

}

#endif /* _OPT_PLAN_H_ */
