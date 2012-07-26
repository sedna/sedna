#ifndef _FUNCTION_OPERATIONS_H_
#define _FUNCTION_OPERATIONS_H_

#include "IndependentPlan.h"
#include "tr/opt/functions/Functions.h"

struct IFunctionData;

namespace rqp {

class XPathStep : public ListOperation {
    RTTI_DECL(plan_operation_XPathStep, ListOperation)

    pe::Step step;
protected:
    virtual XmlConstructor& __toXML ( XmlConstructor& constructor ) const;
public:
    explicit XPathStep(RPBase* _in, const pe::Step & _step)
      : ListOperation(SELF_RTTI_REF, _in), step(_step) { };

    PROPERTY_RO(Step, pe::Step, step)
};

class FunCall : public ManyChildren {
    RTTI_DECL(plan_operation_FunCall, ManyChildren)

    IFunctionData * function_data;
    phop::FunctionInfo * function;
protected:
    virtual XmlConstructor& __toXML ( XmlConstructor& constructor ) const;
public:
    explicit FunCall(phop::FunctionInfo * func, IFunctionData * _fd, const OperationList & _oplist)
      : ManyChildren(SELF_RTTI_REF, _oplist), function_data(_fd), function(func) {
        U_ASSERT(func != NULL);
    };

    explicit FunCall(phop::FunctionInfo * func, IFunctionData * _fd, RPBase* _in)
      : ManyChildren(SELF_RTTI_REF, _in), function_data(_fd), function(func) {
        U_ASSERT(func != NULL);
    };

    explicit FunCall(phop::FunctionInfo * func, IFunctionData * _fd, RPBase* _in1, RPBase* _in2)
      : ManyChildren(SELF_RTTI_REF, _in1), function_data(_fd), function(func) {
        children.push_back(_in2);
        U_ASSERT(func != NULL);
    };

    ~FunCall() { delete function_data; }

    PROPERTY_RO(Function, phop::FunctionInfo * , function)
    PROPERTY_RO(Data, IFunctionData * , function_data)
};

class Construct : public ListOperation {
    RTTI_DECL(plan_operation_Construct, ListOperation)

    t_item type;
protected:
    virtual XmlConstructor& __toXML ( XmlConstructor& constructor ) const;
public:
    opt::TupleId contentId;

    explicit Construct(t_item _type, RPBase* _name, RPBase* list_)
      : ListOperation(SELF_RTTI_REF, list_), type(_type)
    {
        contentId = context->generateTupleId();
        children.push_back(_name);
    };

    PROPERTY_RO(Name, RPBase *, children[1])
    PROPERTY_RO(Type, t_item, type)
};

}

#endif /* _FUNCTION_OPERATIONS_H_ */
