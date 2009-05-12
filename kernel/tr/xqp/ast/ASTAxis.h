/*
 * File:  ASTAxis.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_AXIS_H_
#define _AST_AXIS_H_

#include "ASTNode.h"
#include "AST.h"
#include "ASTAxisStep.h"

class ASTAxis : public ASTNode
{
public:
    ASTAxisStep::AxisType axis;
    ASTNode *expr, *test;

public:
    ASTAxis(ASTLocation &loc, ASTAxisStep::AxisType axis_, ASTNode *expr_, ASTNode *test_) :
        ASTNode(loc),
        axis(axis_),
        expr(expr_),
        test(test_)
    {}

    ~ASTAxis();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif
