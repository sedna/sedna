/*
 * File:  ASTIf.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_IF_H_
#define _AST_IF_H_

#include "ASTNode.h"
#include "AST.h"

class ASTIf : public ASTNode
{
public:
    ASTNode *i_expr, *t_expr, *e_expr;

public:
    ASTIf(ASTLocation &loc, ASTNode *expr1, ASTNode *expr2, ASTNode *expr3) : ASTNode(loc), i_expr(expr1), t_expr(expr2), e_expr(expr3) {}

    ~ASTIf();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif
