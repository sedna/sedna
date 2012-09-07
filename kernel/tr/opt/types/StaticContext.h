#ifndef _STATIC_CONTEXT_H_
#define _STATIC_CONTEXT_H_

#include "tr/opt/OptTypes.h"
#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/executor/base/namespaces.h"

static const ContextInfo invalidContext = {opt::invalidTupleId, opt::invalidTupleId, opt::invalidTupleId};

struct ResultInfo {
    rqp::RPBase * op;
    int opid;
    ContextInfo variable;

    explicit ResultInfo(rqp::RPBase * _op)
      : op(_op), opid(0), variable(invalidContext) { };
};

struct StepInfo {
    pe::axis_t axis;
    pe::node_test_t nodeTest;
    xsd::TemplateQName tqname;
};

struct StaticContext {
    ContextInfo context;

    std::stack<ResultInfo> resultStack;
    std::stack<StepInfo> stepStack;

    StaticallyKnownNamespaces skn;
    CollationHandler * collation;

    void generateContext()
    {
        context.item = optimizer->planContext()->generateTupleId();
    };

    rqp::RPBase * popResult() {
        rqp::RPBase * result = resultStack.top().op;
        resultStack.pop();
        return result;
    };

    rqp::RPBase * getContextVariableOp() {
        return new rqp::VarIn(context.item);
    }
};

#endif /* _STATIC_CONTEXT_H_ */
