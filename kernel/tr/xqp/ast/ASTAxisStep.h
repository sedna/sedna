/*
 * File:  ASTAxisStep.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_AXIS_STEP_H_
#define _AST_AXIS_STEP_H_

#include "ASTNode.h"
#include "ASTNameTest.h"
#include "ASTAttribTest.h"
#include "ASTElementTest.h"
#include "AST.h"

class ASTAxisStep : public ASTNode
{
public:
    enum AxisType
    {
        CHILD = 0,
        DESCENDANT,
        ATTRIBUTE,
        SELF,
        DESCENDANT_OR_SELF,
        FOLLOWING_SIBLING,
        FOLLOWING,
        PARENT,
        ANCESTOR,
        PRECEDING_SIBLING,
        PRECEDING,
        ANCESTOR_OR_SELF,
    };

    AxisType axis;
    ASTNode *test;
    ASTNodesVector *preds;

public:
    ASTAxisStep(ASTLocation &loc, AxisType axis_, ASTNode *test_ = NULL, ASTNodesVector *preds_ = NULL) :
        ASTNode(loc),
        axis(axis_),
        preds(preds_)

    {
        if (test_ && dynamic_cast<ASTNameTest *>(test_))
        {
            if (axis_ == ATTRIBUTE)
                test = new ASTAttribTest(test_->loc, test_);
            else
                test = new ASTElementTest(test_->loc, test_);
        }
        else
        {
            test = test_;
        }
    }

    ~ASTAxisStep();

    void setNodeTest(ASTNode *test_)
    {
        delete test;

        if (test_ && dynamic_cast<ASTNameTest *>(test_))
        {
            if (axis == ATTRIBUTE)
                test = new ASTAttribTest(test_->loc, test_);
            else
                test = new ASTElementTest(test_->loc, test_);
        }
        else
        {
            test = test_;
        }
    }

    void setPredicates(ASTNodesVector *preds_)
    {
        destroyASTNodesVector(preds);
        preds = preds_;
    }

    ASTNodesVector *getPreds() const
    {
        return preds;
    }

    AxisType getAxis() const
    {
        return axis;
    }

    ASTNode *getTest() const
    {
        return test;
    }

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif
