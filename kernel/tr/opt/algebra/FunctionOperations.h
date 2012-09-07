#ifndef _FUNCTION_OPERATIONS_H_
#define _FUNCTION_OPERATIONS_H_

#include "IndependentPlan.h"
#include "tr/opt/functions/Functions.h"

#include <algorithm>

struct IFunctionData;

namespace rqp {

typedef std::vector<opt::TupleId> ParamList;

inline static
ParamList fParams(opt::TupleId p1)
{
    ParamList result;
    result.push_back(p1);
    return result;
};

inline static
ParamList fParams(opt::TupleId p1, opt::TupleId p2)
{
    ParamList result;
    result.push_back(p1);
    result.push_back(p2);
    return result;
};

class FunCallParams : public RPBase {
    RTTI_DECL(plan_operation_FunCallParams, RPBase)

    IFunctionData * function_data;
    phop::FunctionInfo * function;
protected:
    virtual XmlConstructor& __toXML ( XmlConstructor& constructor ) const;
public:
    ParamList paramList;

    explicit FunCallParams(phop::FunctionInfo * func, IFunctionData * _fd, const ParamList & params)
      : RPBase(SELF_RTTI_REF), function_data(_fd), function(func), paramList(params)
    {
        U_ASSERT(func != NULL);

        for (ParamList::const_iterator it = params.begin(); it != params.end(); ++it) {
            context->varGraph.addVariableUsage(*it, this, NULL);
            dependantVariables.insert(*it);
        };
    };

    ~FunCallParams() { delete function_data; }

    virtual void evaluateTo(executor::DynamicContext* dynamicContext);

    PROPERTY_RO(Params, const ParamList & , paramList)
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
