/*
 * File:  ASTFilterStep.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_FILTER_STEP_H_
#define _AST_FILTER_STEP_H_

#include "ASTNode.h"
#include "AST.h"

class ASTFilterStep : public ASTNode
{

public:
    ASTNode *expr;
    ASTNodesVector *preds;

public:
    ASTFilterStep(ASTLocation &loc, ASTNode *expr_, ASTNodesVector *preds_ = NULL) : ASTNode(loc), expr(expr_), preds(preds_) {}

    ~ASTFilterStep();

    void setPredicates(ASTNodesVector *preds_)
    {
        destroyASTNodesVector(preds);
        preds = preds_;
    }

    ASTNodesVector *getPreds() const
    {
        return preds;
    }

    ASTNode *getExpr() const
    {
        return expr;
    }

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif
