#ifndef _PLAN_ALGORITHMS_H_
#define _PLAN_ALGORITHMS_H_

#include "AllOperations.h"

#include "tr/opt/graphs/DataGraphCollection.h"
#include "tr/opt/graphs/DataGraphs.h"

namespace rqp {

struct VarStatInfoItem {
    enum influence_t {
        inf_function,
        inf_not,
        inf_dg,
        inf_no_null_preserve,
    } influence;

    union {
        rqp::RPBase * rp;
        opt::DataNode * dn;
    } op;
};

static const VarStatInfoItem initialVar = {};

typedef std::list<VarStatInfoItem> VarPath;

struct VarStatInfo {
    VarPath path;

    bool used;
    bool singleton;

    VarStatInfo() : used(false), singleton(false) {};
};

typedef std::map<opt::TupleId, VarStatInfo> VarInfoMap;

struct PlanRewriter {
    rqp::RPBase* inputOp;
    rqp::OperationList traverseStack;
    VarInfoMap varMap;

    void do_execute();

    void traverseChildren(rqp::OperationList & children) {
        for (rqp::OperationList::const_iterator it = children.begin(); it != children.end(); ++it) {
            if (*it != null_op) {
                traverseStack.push_back(*it);
                do_execute();
            }
        };
    };

    void execute();

    PlanRewriter(rqp::RPBase* op) : inputOp(op) {};
};

}

#endif /* _PLAN_ALGORITHMS_H_ */
