/*
 * File:  ASTOrdExpr.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_ORD_EXPR_H_
#define _AST_ORD_EXPR_H_

#include "ASTNode.h"
class ASTVisitor;

class ASTOrdExpr : public ASTNode
{
public:
    enum OrdType
    {
        ORDERED,
        UNORDERED
    };

    OrdType type;
    ASTNode *expr;

public:
    ASTOrdExpr(const ASTNodeCommonData &loc, OrdType type_, ASTNode *expr_) : ASTNode(loc), type(type_), expr(expr_) {}

    ~ASTOrdExpr();

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif
