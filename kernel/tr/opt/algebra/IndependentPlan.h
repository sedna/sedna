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
#include "tr/opt/path/XPathTypes.h"
#include "tr/opt/graphs/VariableGraph.h"

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

class PlanContext {
  private:
    ScopeMarker lastScopeMarker;
    opt::TupleId currentTupleId;

    ScopeStack scopeStack;
    VarNameMap scope;

  public:
    PlanContext();
    ~PlanContext();

    opt::VariableUsageGraph varGraph;

    opt::TupleId generateTupleId() { return ++currentTupleId; };

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
    opt::TupleScheme dependantVariables;

    virtual XmlConstructor & __toXML(XmlConstructor &) const = 0;
public:
    OperationList children;
    int resultChild;

    RPBase(clsinfo_t op) : ObjectBase(op), context(optimizer->planContext()), opuid(opids++), resultChild(-1) {};
    inline int oid() const { return opuid; };

    /* Replace child operation op with another operation */
    void replace(RPBase * op, RPBase * with);
    PlanContext * getContext() const { return context; };

    RPBase * result() { if (resultChild > -1) { return children[resultChild]; } else { return (RPBase *)(NULL); } };

    const opt::TupleScheme & dependsOn() const { return dependantVariables; }

    virtual void evaluateTo(executor::DynamicContext * dynamicContext) { U_ASSERT(false); };
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
        dependantVariables.insert(list_->dependsOn().begin(), list_->dependsOn().end());
    };

    PROPERTY(List, RPBase *, children[0])
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
            dependantVariables.insert((*it)->dependsOn().begin(), (*it)->dependsOn().end());
        };
    };

    ManyChildren(clsinfo_t op, RPBase* _in)
      : RPBase(op)
    {
          children.push_back(_in);
          dependantVariables.insert(_in->dependsOn().begin(), _in->dependsOn().end());
    };
};

}

#endif /* _OPT_PLAN_H_ */
