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
    operation_flags_t flags;
};

typedef const opdesc_t * _opinfo_t;

typedef int ScopeMarker;
typedef std::stack<opt::TupleId> ScopeStack;
typedef std::map<std::string, opt::TupleId> VarNameMap;
typedef std::map<opt::TupleId, TupleDefinition> GreatTupleScheme;
typedef std::vector< RPBase * > OperationList;

typedef VarNameMap::value_type VarMapRecord;
typedef GreatTupleScheme::value_type GreatMapRecord;

typedef std::pair<RPBase *, RPBase *> RPEdge;
typedef std::map<RPBase *, RPBase **> LinkMap;


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

    LinkMap linkmap;
    
    opt::DataGraphMaster * dataGraphFactory;
  public:
    static PlanContext * current;

    opt::DataGraphMaster * dgm() const { return dataGraphFactory; };

    PlanContext();
    ~PlanContext();

    const TupleDefinition * getVarDef(opt::TupleId tid) const { return &(greatTupleScheme.at(tid)); };

    void registerLink(RPBase * a, RPBase * b, RPBase ** bptr) {
        linkmap[b] = bptr;
    };
    
    opt::TupleId generateTupleId();
    opt::TupleId generateTupleIdVarScoped(const std::string & varName);
    opt::TupleId getVarTupleInScope(const std::string & canonicalName);

    ScopeMarker setScopeMarker();

    void replaceOperation(RPBase * a, RPBase * b);
    void replaceLink(RPBase * a, RPBase ** aptr);
    
    void newScope();
    void clearScope();
    void clearScopesToMarker(ScopeMarker marker);
};

class RPBase : public opt::IPlanDisposable {
  protected:
    _opinfo_t opdesc;
  public:
    virtual ~RPBase() {};
  protected:
    PlanContext * context;
    static int opids;
    int opuid;

    virtual XmlConstructor & __toXML(XmlConstructor &) const = 0;
  public:
    RPBase(_opinfo_t op) : opdesc(op), context(PlanContext::current), opuid(opids++) {};

    inline int oid() const { return opuid; };
    inline const opdesc_t * info() const { return opdesc; };
    inline bool isType(int t) const { return opdesc->opType == t; };
    PlanContext * getContext() const { return context; };

    virtual void getChildren(OperationList & children) const = 0;

    XmlConstructor & toXML(XmlConstructor &) const;
};

template<class T> inline static
bool instanceof(RPBase * op) { return op != null_op && op->isType(T::opid); };

/* YES! I know about typeid operator existance. */

#define ABSTRACT_OPERATION \

#define MAX_OPERATION_ID 0x200

#define OPERATION(ID) \
public:\
  static const opdesc_t sopdesc; \
  enum _opid_t { opid = (ID) }; \
protected: virtual XmlConstructor& __toXML(XmlConstructor& ) const; \
private:

#define PROPERTY(name, t, member) t get##name() const { return member; } void set##name(t value) { member = value; }
#define PROPERTY_RO(name, t, member) t get##name() const { return member; }
#define PROPERTY_WO(name, t, member) void set##name(t value) { member = value; }

/* Basic operation classes, devided by arity */

/* 0r-operations */
class ConstantOperation : public RPBase {
    ABSTRACT_OPERATION
  protected:
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
  public:
    ConstantOperation(_opinfo_t op)
      : RPBase(op) {};

    virtual void getChildren(OperationList & children) const;
};

/* 1r-operations */
class ListOperation : public RPBase {
    ABSTRACT_OPERATION
  protected:
    RPBase * list;
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
  public:
    ListOperation(_opinfo_t op, RPBase * list_)
      : RPBase(op), list(list_) {
        PlanContext::current->registerLink(this, list_, &list);
    };

    virtual void getChildren(OperationList & children) const;

    PROPERTY_RO(List, RPBase *, list)
};

/* 1r-operations with independent nested operation plan */
class NestedOperation : public ListOperation {
    ABSTRACT_OPERATION
  protected:
    RPBase * subplan;
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
  public:
    NestedOperation(_opinfo_t op, RPBase * list_, RPBase * subplan_)
      : ListOperation(op, list_), subplan(subplan_) {
        PlanContext::current->registerLink(this, subplan_, &subplan);
    };

    virtual void getChildren(OperationList & children) const;
      
    PROPERTY_RO(Subplan, RPBase *, subplan)
};

/* 2r-operations */
class BinaryOperation : public RPBase {
    ABSTRACT_OPERATION
  private:
    RPBase * leftList;
    RPBase * rightList;
  protected:
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
  public:
    BinaryOperation(_opinfo_t op, RPBase * ltItem_, RPBase * rtItem_)
      : RPBase(op), leftList(ltItem_), rightList(rtItem_) {
        PlanContext::current->registerLink(this, ltItem_, &leftList);
        PlanContext::current->registerLink(this, rtItem_, &rightList);
    };

    virtual void getChildren(OperationList & children) const;

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

struct EmptySequenceConst { explicit EmptySequenceConst(int x = 0) {}; };

class Const : public ConstantOperation {
    OPERATION(0x012)
    opt::MemoryTupleSequencePtr sequence;
public:
    explicit Const(opt::MemoryTupleSequencePtr _sequence)
      : ConstantOperation(&sopdesc), sequence(_sequence)
    {};

    explicit Const(tuple_cell _value)
      : ConstantOperation(&sopdesc), sequence(new opt::MemoryTupleSequence())
    { sequence->push_back(_value); };

    explicit Const(const EmptySequenceConst &)
      : ConstantOperation(&sopdesc), sequence(new opt::MemoryTupleSequence())
    { };
    
    PROPERTY_RO(Sequence, opt::MemoryTupleSequencePtr, sequence)
};

class XPathStep : public ListOperation {
    OPERATION(0x013)
    pe::Step step;
public:
    XPathStep(RPBase* _in, const pe::Step & _step)
      : ListOperation(&sopdesc, _in), step(_step) { };

    PROPERTY_RO(Step, pe::Step, step)
};

class Select : public NestedOperation {
    OPERATION(0x004)
    opt::TupleId tid;
public:
    Select(RPBase* _list, RPBase* _subplan, opt::TupleId _tid)
      : NestedOperation(&sopdesc, _list, _subplan), tid(_tid) {};

    PROPERTY_RO(ContextTuple, opt::TupleId, tid)
};

class If : public RPBase {
    OPERATION(0x015)
private:
    RPBase * condition;
    RPBase * thenBranch;
    RPBase * elseBranch;
public:
    If(RPBase* _condition, RPBase* _then, RPBase* _else)
      : RPBase(&sopdesc), condition(_condition), thenBranch(_then), elseBranch(_else) {
        PlanContext::current->registerLink(this, _condition, &condition);
        PlanContext::current->registerLink(this, _then, &thenBranch);
        PlanContext::current->registerLink(this, _else, &elseBranch);
    };

    virtual void getChildren(OperationList & children) const;

    PROPERTY_RO(Condition, RPBase *, condition)
    PROPERTY_RO(Then, RPBase *, thenBranch)
    PROPERTY_RO(Else, RPBase *, elseBranch)
};


class ComparisonExpression : public BinaryOperation {
    OPERATION(0x016)
    opt::Comparison cmp;
public:
    ComparisonExpression(RPBase* _left, RPBase* _right, const opt::Comparison &_cmp)
      : BinaryOperation(&sopdesc, _left, _right), cmp(_cmp) { };

    PROPERTY_RO(Op, opt::Comparison, cmp)
};

class ManyChildren : public RPBase {
    ABSTRACT_OPERATION
  protected:
    OperationList opList;

    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
  private:
    void registerOps() {
        for (OperationList::iterator it = opList.begin(); it != opList.end(); ++it) {
            PlanContext::current->registerLink(this, *it, it.base());
        };
    };    
  public:
    ManyChildren(_opinfo_t op, const OperationList & _oplist)
      : RPBase(op), opList(_oplist) { registerOps(); };

    ManyChildren(_opinfo_t op, RPBase* _in)
      : RPBase(op) { opList.push_back(_in); registerOps(); };

    virtual void getChildren(OperationList & children) const;

    PROPERTY_RO(Operations, const OperationList &, opList)
};

class FunCall : public ManyChildren {
    OPERATION(0x017)
    xsd::QName name;
public:
    FunCall(const xsd::QName & fname, const OperationList & _oplist)
      : ManyChildren(&sopdesc, _oplist), name(fname) { };

    FunCall(const xsd::QName & fname, RPBase* _in)
      : ManyChildren(&sopdesc, _in), name(fname) { };
      
};

class Construct : public ListOperation {
    OPERATION(0x018)
    t_item type;
    RPBase * name;
public:
    Construct(t_item _type, RPBase* _name, RPBase* list_)
      : ListOperation(&sopdesc, list_), type(_type), name(_name) {
        PlanContext::current->registerLink(this, _name, &name);
    };

    virtual void getChildren(OperationList & children) const;
      
    PROPERTY_RO(Name, RPBase *, name)
    PROPERTY_RO(Type, t_item, type)
};

class Sequence : public ManyChildren {
    OPERATION(0x019)
public:
    enum space_t {
        none,
        atomic_spaces,
        all_spaces
    };

private:    
    space_t spaces;
public:
    Sequence(const OperationList & _oplist)
      : ManyChildren(&sopdesc, _oplist), spaces(none) { };

    Sequence(RPBase* _in)
      : ManyChildren(&sopdesc, _in), spaces(none) { };

    PROPERTY(Spaces, space_t, spaces)
};

class DataGraphOperation : public ManyChildren {
    OPERATION(0x01a)
  protected:
    opt::DataGraph * func;
    
    DataGraphOperation(_opinfo_t op, opt::DataGraph * function_, const OperationList & _oplist)
      : ManyChildren(op, _oplist), func(function_) {
    };
  public:
    DataGraphOperation(opt::DataGraph * function_, const OperationList & _oplist)
      : ManyChildren(&sopdesc, _oplist), func(function_) {
    };

    PROPERTY(Graph, opt::DataGraph *, func)
//    PROPERTY(Mapping, const opt::TupleMapping &, tmapping)
};

class MapGraph : public DataGraphOperation {
    OPERATION(0x01b)

    RPBase * list;
public:
    MapGraph(RPBase* _list, opt::DataGraph * function_, const OperationList & _oplist)
      : DataGraphOperation(&sopdesc, function_, _oplist), list(_list) {
        context->registerLink(this, _list, &list);
    };

    virtual void getChildren(OperationList & children) const;
      
    opt::TupleScheme tupleMask;
};

/*

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
