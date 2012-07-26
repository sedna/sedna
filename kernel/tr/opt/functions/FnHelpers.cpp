#include "FnHelpers.h"

#include "tr/opt/algebra/PlanAlgorithms.h"

using namespace opt;
using namespace rqp;
using namespace phop;

bool do_outer_bind_parameter(PlanRewriter* pr, RPBase* op, unsigned int idx, bool preserveNull)
{
    return false;
}


bool do_operation_push_down(rqp::PlanRewriter * pr, rqp::RPBase * op, unsigned idx)
{
    RPBase * child = op->children[idx];

    if (child == null_op) {
        return false;
    };

    if (child->resultChild != -1)
    {
        RPBase * grandChild = child->result();

        child->children[child->resultChild] = op;
        op->children[idx] = grandChild;

        pr->replaceInParent(op, child);
        return true;
    };

    return false;
};
