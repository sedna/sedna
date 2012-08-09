#ifndef _PLAN_ALGORITHMS_H_
#define _PLAN_ALGORITHMS_H_

#include "AllOperations.h"
#include "tr/opt/graphs/DataGraphs.h"

namespace rqp {

struct VarGraphRewriting
{
    rqp::RPBase* root;

    VarGraphRewriting(rqp::RPBase* op) : root(op) {};

    static
    RPBase * rewrite(RPBase * op)
    {
        VarGraphRewriting rewriter(op);
        rewriter.execute();
        return rewriter.root;
    };

    void execute();
};

struct RewritingContext {
    rqp::RPBase* root;
    rqp::OperationList traverseStack;

    RewritingContext(rqp::RPBase* op) : root(op) {};

    void execute();
    void do_execute();

    static
    RPBase * rewrite(RPBase * op) 
    {
        RewritingContext rewriter(op);
        rewriter.execute();
        return rewriter.root;
    };

    RPBase * getParent()
    {
        if (traverseStack.size() < 2) {
            return NULL;
        }

        return traverseStack.at(traverseStack.size() - 2);
    };

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

    inline
    void traverseChildren(const rqp::OperationList & children)
    {
        for (rqp::OperationList::const_iterator it = children.begin(); it != children.end(); ++it) {
            traverse(*it);
        };
    };
};

/**
 * @brief Looks for declaration of given variables
 */
struct TreePathAnalisys
{
    RewritingContext * rewriter;
    bool preserveNull;
    RPBase * result;

    TreePathAnalisys(RewritingContext * pr)
      : rewriter(pr), preserveNull(true), result(NULL) {};

    bool findDeclaration(const opt::TupleScheme & tuplesToSearch);
    bool isConditional();
};

struct DataNodeTupleLookup
{
    opt::TupleId tid;

    inline
    DataNodeTupleLookup(opt::TupleId _tid) : tid(_tid) {};

    inline
    bool operator()(const opt::DataNode * dnode) {
        return dnode->varTupleId == tid;
    };
};

}

#endif /* _PLAN_ALGORITHMS_H_ */
