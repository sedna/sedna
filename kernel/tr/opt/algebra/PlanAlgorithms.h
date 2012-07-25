#ifndef _PLAN_ALGORITHMS_H_
#define _PLAN_ALGORITHMS_H_

#include "AllOperations.h"

#include "tr/opt/graphs/DataGraphCollection.h"
#include "tr/opt/graphs/DataGraphs.h"

namespace rqp {

struct PlanRewriter {
    rqp::RPBase* root;
    rqp::OperationList traverseStack;

    RPBase * getParent()
    {
        if (traverseStack.size() < 2) {
            return NULL;
        }

        return traverseStack.at(traverseStack.size() - 2);
    };

    void do_execute();

    inline 
    void traverse(RPBase * op)
    {
        if (null_obj != op) {
            traverseStack.push_back(op);
            do_execute();
        }
    };

//    bool __debug_trace_replace(uint op1, uint op2);

    inline 
    void replaceInParent(rqp::RPBase * op1, rqp::RPBase * op2)
    {
        rqp::RPBase * parent = getParent();

        /* If parent not exists, replace root */

        if (parent == null_obj) {
            U_ASSERT(root == op1);
            root = op2;
        } else {
            U_ASSERT(parent != op1);
            parent->replace(op1, op2);
//            U_ASSERT(__debug_trace_replace(op1->oid(), op2->oid()));
        }

        traverseStack.pop_back();
        traverseStack.push_back(op2);
    };

    void traverseChildren(const rqp::OperationList & children)
    {
        for (rqp::OperationList::const_iterator it = children.begin(); it != children.end(); ++it) {
            traverse(*it);
        };
    };

    void execute();

    PlanRewriter(rqp::RPBase* op) : root(op) {};
};

}

#endif /* _PLAN_ALGORITHMS_H_ */
