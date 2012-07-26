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
#include "tr/models/rtti.h"

/* Look PlanOperationTypes for description of all operators */

struct ContextInfo {
    opt::TupleId item, position, size;
};

namespace rqp {

typedef int ScopeMarker;
typedef std::stack<opt::TupleId> ScopeStack;
typedef std::map<std::string, opt::TupleId> VarNameMap;
typedef std::map<opt::TupleId, TupleDefinition> GreatTupleScheme;
typedef std::vector< RPBase * > OperationList;

typedef VarNameMap::value_type VarMapRecord;
typedef GreatTupleScheme::value_type GreatMapRecord;

class PlanContext {
  private:
    ScopeMarker lastScopeMarker;
    opt::TupleId currentTupleId;
    GreatTupleScheme greatTupleScheme;

    ScopeStack scopeStack;
    VarNameMap scope;

  public:
    PlanContext();
    ~PlanContext();

    const TupleDefinition * getVarDef(opt::TupleId tid) const { return &(greatTupleScheme.at(tid)); };

    opt::TupleId generateTupleId();
    opt::TupleId generateTupleIdVarScoped(const std::string & varName);
    opt::TupleId getVarTupleInScope(const std::string & canonicalName);

    ScopeMarker setScopeMarker();

    void newScope();
    void clearScope();
    void clearScopesToMarker(ScopeMarker marker);
};

class RPBase : public ObjectBase, public opt::IPlanDisposable, public IXMLSerializable {
    RTTI_DECL(plan_operation_base, ObjectBase)
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

    OperationList children;

    RPBase(clsinfo_t op) : ObjectBase(op), context(optimizer->context()), opuid(opids++), resultChild(-1) {};

    /* Replace child operation op with another operation */
    void replace(RPBase * op, RPBase * with);
    PlanContext * getContext() const { return context; };
    RPBase * result() { if (resultChild > -1) { return children[resultChild]; } else { return static_cast<RPBase *>(null_obj); } };

    virtual XmlConstructor& toXML(XmlConstructor& constructor) const;
};

static RPBase * const null_op = (RPBase *)(NULL);

#define PROPERTY(name, t, member) t get##name() const { return member; } void set##name(t value) { member = value; }
#define PROPERTY_RO(name, t, member) t get##name() const { return member; }
#define PROPERTY_WO(name, t, member) void set##name(t value) { member = value; }

/* Basic operation classes, devided by arity */

/* 0r-operations */
class ConstantOperation : public RPBase {
   RTTI_DECL(plan_operation_ConstantOperation, RPBase)
protected:
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
public:
    ConstantOperation(clsinfo_t op)
      : RPBase(op) {};
};

/* 1r-operations */
class ListOperation : public RPBase {
   RTTI_DECL(plan_operation_ListOperation, RPBase)
protected:
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
public:
    ListOperation(clsinfo_t op, RPBase * list_)
      : RPBase(op) {
        children.push_back(list_);
    };

    PROPERTY_RO(List, RPBase *, children[0])
};

/* 1r-operations with independent nested operation plan */
class NestedOperation : public ListOperation {
  RTTI_DECL(plan_operation_NestedOperation, ListOperation)
protected:
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
public:
    opt::TupleId tid;

    NestedOperation(clsinfo_t op, RPBase * list_, RPBase * subplan_, opt::TupleId _tid)
      : ListOperation(op, list_), tid(_tid) {
        children.push_back(subplan_);
    };

    PROPERTY_RO(Subplan, RPBase *, children[1])
};

class ManyChildren : public RPBase {
    RTTI_DECL(plan_operation_ManyChildren, RPBase)
protected:
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;

    ManyChildren(clsinfo_t op) : RPBase(op) {};
private:
public:
    ManyChildren(clsinfo_t op, const OperationList & _oplist)
      : RPBase(op)
    {
        for (OperationList::const_iterator it = _oplist.begin(); it != _oplist.end(); ++it) {
            children.push_back(*it);
        };
    };

    ManyChildren(clsinfo_t op, RPBase* _in)
      : RPBase(op) { children.push_back(_in); };
};

}

#endif /* _OPT_PLAN_H_ */
