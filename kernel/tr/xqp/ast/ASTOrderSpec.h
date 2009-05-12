/*
 * File:  ASTOrderSpec.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_ORDER_SPEC_H_
#define _AST_ORDER_SPEC_H_

#include "ASTNode.h"
#include "AST.h"

class ASTOrderMod;

class ASTOrderSpec : public ASTNode
{
public:
    ASTNode *expr;
    ASTOrderMod *mod;

public:
    ASTOrderSpec(ASTLocation &loc, ASTNode *ord_expr, ASTOrderMod *ord_mod = NULL) : ASTNode(loc), expr(ord_expr), mod(ord_mod) {}

    ~ASTOrderSpec();

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif
