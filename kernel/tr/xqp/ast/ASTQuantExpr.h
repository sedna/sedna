/*
 * File:  ASTQuantExpr.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_QUANT_EXPR_H_
#define _AST_QUANT_EXPR_H_

#include "ASTNode.h"
#include "AST.h"

#include "ASTFunDef.h"

class ASTQuantExpr : public ASTNode
{
public:
    enum QuantMod
    {
        SOME,
        EVERY
    };

    ASTNode *expr;
    ASTNode *fd; // ASTFunDef
    QuantMod type;

public:
    ASTQuantExpr(ASTLocation &loc, ASTNode *var_expr, ASTNode *sat_expr, QuantMod mod) : ASTNode(loc), expr(var_expr), fd(sat_expr), type(mod) {}

    ~ASTQuantExpr();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif
