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
    class PlanExecutor;
}

struct ContextInfo {
    opt::TupleId item, position, size;
};

namespace rqp {

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

class PlanContext : public opt::IPlanDisposable {
  private:
    ScopeMarker lastScopeMarker;
    opt::TupleId currentTupleId;
    GreatTupleScheme greatTupleScheme;

    ScopeStack scopeStack;
    VarNameMap scope;

  public:
    PlanContext();
    ~PlanContext();

    phop::PlanExecutor * executor;
    
    const TupleDefinition * getVarDef(opt::TupleId tid) const { return &(greatTupleScheme.at(tid)); };

    opt::TupleId generateTupleId();
    opt::TupleId generateTupleIdVarScoped(const std::string & varName);
    opt::TupleId getVarTupleInScope(const std::string & canonicalName);

    ScopeMarker setScopeMarker();

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
    int resultChild;

    virtual XmlConstructor & __toXML(XmlConstructor &) const = 0;
  public:
    inline int oid() const { return opuid; };
    inline const opdesc_t * info() const { return opdesc; };
  public:
    OperationList children;

    RPBase(_opinfo_t op) : opdesc(op), context(optimizer->context()), opuid(opids++), resultChild(-1) {};

    /* Replace child operation op with another operation */
    void replace(RPBase * op, RPBase * with);
    PlanContext * getContext() const { return context; };
    RPBase * result() { if (resultChild > -1) { return children[resultChild]; } else { return null_op; } };

    virtual XmlConstructor& toXML(XmlConstructor& constructor) const;
//    virtual void execute() const = 0;

//    virtual rewrite() const;
//    virtual phop::IOperator * compile() const = 0;
};

template<class T> inline static
bool instanceof(RPBase * op) { return op != null_op && op->info()->opType == T::opid; };

/* YES! I know about typeid operator existance. */

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
  protected:
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
  public:
    ConstantOperation(_opinfo_t op)
      : RPBase(op) {};
};

/* 1r-operations */
class ListOperation : public RPBase {
  protected:
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
  public:
    ListOperation(_opinfo_t op, RPBase * list_)
      : RPBase(op) {
        children.push_back(list_);
    };

    PROPERTY_RO(List, RPBase *, children[0])
};

/* 1r-operations with independent nested operation plan */
class NestedOperation : public ListOperation {
  protected:
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
  public:
    opt::TupleId tid;

    NestedOperation(_opinfo_t op, RPBase * list_, RPBase * subplan_, opt::TupleId _tid)
      : ListOperation(op, list_), tid(_tid) {
        children.push_back(subplan_);
    };

    PROPERTY_RO(Subplan, RPBase *, children[1])
};

/* 2r-operations */
class BinaryOperation : public RPBase {
  private:
  protected:
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
  public:
    BinaryOperation(_opinfo_t op, RPBase * ltItem_, RPBase * rtItem_)
      : RPBase(op) {
        children.push_back(ltItem_);
        children.push_back(rtItem_);
    };

    PROPERTY_RO(Left, RPBase *, children[0])
    PROPERTY_RO(Right, RPBase *, children[1])
};

class ManyChildren : public RPBase {
  protected:
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
  private:
  public:
    ManyChildren(_opinfo_t op, const OperationList & _oplist)
      : RPBase(op)
    {
        for (OperationList::const_iterator it = _oplist.begin(); it != _oplist.end(); ++it) {
            children.push_back(*it);
        };
    };

    ManyChildren(_opinfo_t op, RPBase* _in)
      : RPBase(op) { children.push_back(_in); };
};

}

#endif /* _OPT_PLAN_H_ */
