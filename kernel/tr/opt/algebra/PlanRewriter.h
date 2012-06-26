#ifndef _PLAN_REWRITER_H_
#define _PLAN_REWRITER_H_

#include "tr/opt/algebra/PlanAlgorithms.h"

//namespace rqp {
//    class RPBase;
//}

//rqp::RPBase* varUsageAnalyzis(rqp::RPBase* op);
rqp::RPBase* selectDataGraphs(rqp::RPBase* op)
{
    rqp::PlanRewriter rewriter(op);
    rewriter.execute();
    return rewriter.inputOp;
//    return op;
};

#endif /* _PLAN_REWRITER_H_ */
