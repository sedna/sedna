/*
 * File:  ASTStep.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_STEP_H_
#define _AST_STEP_H_

#include "ASTNode.h"
#include "ASTNameTest.h"
#include "ASTElementTest.h"
#include "ASTPiTest.h"

class ASTVisitor;

class ASTStep : public ASTNode
{
public:
    ASTNode *cont; // context for this step (previous step)
    ASTNodesVector *preds;
    bool isLast; // last step in XPath

public:

    ASTStep(const ASTNodeCommonData &loc, ASTNodesVector *preds_) : ASTNode(loc), cont(NULL), preds(preds_), isLast(false) {}

    ~ASTStep();

    void setPredicates(ASTNodesVector *preds_)
    {
        destroyASTNodesVector(preds);
        preds = preds_;
    }

    ASTNodesVector *getPreds() const
    {
        return preds;
    }

    void setContext(ASTNode* context)
    {
        cont = context;
    }

    ASTNode *getContext()
    {
        return cont;
    }

    void setAsLast()
    {
        isLast = true;
    }

    bool isFirstStep() const
    {
        return (cont == NULL);
    }

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);
};

class ASTAxisStep : public ASTStep
{
public:
    // NOTE: order of axis types matters since we use the order to simplify some axis checks
    enum AxisType
    {
        CHILD = 0,
        DESCENDANT,
        ATTRIBUTE,
        SELF,
        DESCENDANT_OR_SELF,
        DESCENDANT_ATTRIBUTE,         // sedna specifix axes (for internal use)
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

public:
    ASTAxisStep(const ASTNodeCommonData &loc, AxisType axis_, ASTNode *test_ = NULL, ASTNodesVector *preds_ = NULL) :
        ASTStep(loc, preds_),
        axis(axis_)
    {
        if (test_ && dynamic_cast<ASTNameTest *>(test_))
        {
            if (axis_ == ATTRIBUTE)
                test = new ASTAttribTest(test_->cd.loc, test_);
            else
                test = new ASTElementTest(test_->cd.loc, test_);
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
                test = new ASTAttribTest(test_->cd.loc, test_);
            else
                test = new ASTElementTest(test_->cd.loc, test_);
        }
        else
        {
            test = test_;
        }
    }

    AxisType getAxis() const
    {
        return axis;
    }

    ASTNode *getTest() const
    {
        return test;
    }
    
    bool isSuitableForAbsPath() const
    {
        ASTPiTest *pit = dynamic_cast<ASTPiTest *>(test);

        return (axis <= ASTAxisStep::DESCENDANT_ATTRIBUTE && !preds && (!pit || pit->type == ASTPiTest::NONE));
    }

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

class ASTFilterStep : public ASTStep
{

public:
    ASTNode *expr; // may be NULL in case of context expression

    bool use_last; // expression in this step uses fn:last()
    bool use_pos;  // // expression in this step uses fn:position()

public:
    ASTFilterStep(const ASTNodeCommonData &loc, ASTNode *expr_, ASTNodesVector *preds_ = NULL) :
        ASTStep(loc, preds_),
        expr(expr_)
    {
        use_last = false;
        use_pos = false;
    }

    ~ASTFilterStep();

    ASTNode *getExpr() const
    {
        return expr;
    }

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif
