#ifndef _FUNCTION_OPERATIONS_H_
#define _FUNCTION_OPERATIONS_H_

#include "IndependentPlan.h"
#include "tr/opt/functions/Functions.h"

struct IFunctionData;

namespace rqp {

class XPathStep : public ListOperation {
    OPERATION(0x013)
    pe::Step step;
public:
    XPathStep(RPBase* _in, const pe::Step & _step)
      : ListOperation(&sopdesc, _in), step(_step) { };

    PROPERTY_RO(Step, pe::Step, step)
};

class FunCall : public ManyChildren {
    OPERATION(0x017)
    IFunctionData * function_data;
    phop::FunctionInfo * function;
public:
    FunCall(phop::FunctionInfo * func, IFunctionData * _fd, const OperationList & _oplist)
      : ManyChildren(&sopdesc, _oplist), function_data(_fd), function(func) {
        U_ASSERT(func != NULL);
    };

    FunCall(phop::FunctionInfo * func, IFunctionData * _fd, RPBase* _in)
      : ManyChildren(&sopdesc, _in), function_data(_fd), function(func) {
        U_ASSERT(func != NULL);
    };

    FunCall(phop::FunctionInfo * func, IFunctionData * _fd, RPBase* _in1, RPBase* _in2)
      : ManyChildren(&sopdesc, _in1), function_data(_fd), function(func) {
        children.push_back(_in2);
        U_ASSERT(func != NULL);
    };

    ~FunCall() { delete function_data; }
    
    PROPERTY_RO(Function, phop::FunctionInfo * , function)
    PROPERTY_RO(Data, IFunctionData * , function_data)
};

class Construct : public ListOperation {
    OPERATION(0x018)
    t_item type;
public:
    Construct(t_item _type, RPBase* _name, RPBase* list_)
      : ListOperation(&sopdesc, list_), type(_type) {
        children.push_back(_name);
        PlanContext::current->registerLink(this, _name, &(children[1]));
    };

    PROPERTY_RO(Name, RPBase *, children[1])
    PROPERTY_RO(Type, t_item, type)
};

}

#endif /* _FUNCTION_OPERATIONS_H_ */
