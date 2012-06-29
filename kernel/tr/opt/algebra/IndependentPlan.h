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
#include "tr/opt/OptTypes.h"

#include "tr/opt/algebra/PlanOperations.h"
#include "tr/opt/types/TupleScheme.h"
#include "tr/opt/path/XPathTypes.h"
#include "tr/models/XmlConstructor.h"

/* Look PlanOperationTypes for description of all operators */

namespace phop {
    class IOperator;
}

struct ContextInfo {
    opt::TupleId item, position, size;
};

namespace rqp {

class PlanVisitor;

struct opdesc_t {
    const char * opname;
    int opType;
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

class RPBase : public opt::IPlanDisposable, public IXMLSerializable {
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
    inline int oid() const { return opuid; };
    inline const opdesc_t * info() const { return opdesc; };
  public:
    OperationList children;

    /* Replace child operation op with another operation */
    void replace(RPBase * op, RPBase * with);

    RPBase(_opinfo_t op) : opdesc(op), context(PlanContext::current), opuid(opids++) {};

    PlanContext * getContext() const { return context; };

    virtual XmlConstructor& toXML(XmlConstructor& constructor) const;

//    virtual rewrite() const;
//    virtual phop::IOperator * compile() const = 0;
};

template<class T> inline static
bool instanceof(RPBase * op) { return op != null_op && op->info()->opType == T::opid; };

/* YES! I know about typeid operator existance. */

#define ABSTRACT_OPERATION \

#define MAX_OPERATION_ID 0x200

#define OPERATION(ID) \
public:\
  static const opdesc_t sopdesc; \
  enum _opid_t { opid = (ID) }; \
protected: virtual XmlConstructor& __toXML(XmlConstructor& ) const; \
private:

#define OPERATION_INFO(C) \
const opdesc_t C::sopdesc = {#C, C::opid}; \
  
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
};

/* 1r-operations */
class ListOperation : public RPBase {
    ABSTRACT_OPERATION
  protected:
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
  public:
    ListOperation(_opinfo_t op, RPBase * list_)
      : RPBase(op) {
        children.push_back(list_);
        PlanContext::current->registerLink(this, list_, &(children[0]));
    };

    PROPERTY_RO(List, RPBase *, children[0])
};

/* 1r-operations with independent nested operation plan */
class NestedOperation : public ListOperation {
    ABSTRACT_OPERATION
  protected:
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
  public:
    opt::TupleId tid;

    NestedOperation(_opinfo_t op, RPBase * list_, RPBase * subplan_, opt::TupleId _tid)
      : ListOperation(op, list_), tid(_tid) {
        children.push_back(subplan_);
        PlanContext::current->registerLink(this, subplan_, &(children[1]));
    };

    PROPERTY_RO(Subplan, RPBase *, children[1])
};

/* 2r-operations */
class BinaryOperation : public RPBase {
    ABSTRACT_OPERATION
  private:
  protected:
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
  public:
    BinaryOperation(_opinfo_t op, RPBase * ltItem_, RPBase * rtItem_)
      : RPBase(op) {
        children.push_back(ltItem_);
        children.push_back(rtItem_);

        PlanContext::current->registerLink(this, ltItem_, &(children[0]));
        PlanContext::current->registerLink(this, rtItem_, &(children[1]));
    };

    PROPERTY_RO(Left, RPBase *, children[0])
    PROPERTY_RO(Right, RPBase *, children[1])
};

/*
class ComparisonExpression : public BinaryOperation {
    OPERATION(0x016)
    opt::Comparison cmp;
public:
    ComparisonExpression(RPBase* _left, RPBase* _right, const opt::Comparison &_cmp)
      : BinaryOperation(&sopdesc, _left, _right), cmp(_cmp) { };

    PROPERTY_RO(Op, opt::Comparison, cmp)
};
*/

class ManyChildren : public RPBase {
    ABSTRACT_OPERATION
  protected:
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
  private:
    void registerOps() {
        for (OperationList::iterator it = children.begin(); it != children.end(); ++it) {
            PlanContext::current->registerLink(this, *it, it.base());
        };
    };    
  public:
    ManyChildren(_opinfo_t op, const OperationList & _oplist)
      : RPBase(op)
    {
        for (OperationList::const_iterator it = _oplist.begin(); it != _oplist.end(); ++it) {
            children.push_back(*it);
        };

        registerOps();
    };

    ManyChildren(_opinfo_t op, RPBase* _in)
      : RPBase(op) { children.push_back(_in); registerOps(); };
};

}

#endif /* _OPT_PLAN_H_ */
