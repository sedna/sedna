/*
 * File:  ASTQuantExpr.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_QUANT_EXPR_H_
#define _AST_QUANT_EXPR_H_

#include "ASTNode.h"
#include "AST.h"

class ASTFunDef;

class ASTQuantExpr : public ASTNode
{
public:
    enum QuantMod
    {
        SOME,
        EVERY
    };

    ASTNode *expr;
    ASTFunDef *fd;
    QuantMod type;

public:
    ASTQuantExpr(ASTLocation &loc, ASTNode *var_expr, ASTFunDef *sat_expr, QuantMod mod) : ASTNode(loc), expr(var_expr), fd(sat_expr), type(mod) {}

    ~ASTQuantExpr();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif
