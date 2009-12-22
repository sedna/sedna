/*
 * File:  ASTQuantExpr.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_QUANT_EXPR_H_
#define _AST_QUANT_EXPR_H_

#include "ASTNode.h"
class ASTVisitor;

class ASTQuantExpr : public ASTNode
{
public:
    enum QuantMod
    {
        SOME,
        EVERY
    };

    ASTNode *var; // ASTTypeVar
    ASTNode *expr;
    ASTNode *sat;
    QuantMod type;

public:
    ASTQuantExpr(const ASTNodeCommonData &loc, ASTNode *var_, ASTNode *var_expr, ASTNode *sat_expr, QuantMod mod) : ASTNode(loc), var(var_), expr(var_expr), sat(sat_expr), type(mod) {}

    ~ASTQuantExpr();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif
