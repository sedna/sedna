/*
 * File:  ASTCastable.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_CASTABLE_H_
#define _AST_CASTABLE_H_

#include "ASTNode.h"
#include "AST.h"

class ASTTypeSingle;

class ASTCastable : public ASTNode
{
public:
    ASTNode *expr;
    ASTTypeSingle *type;

public:
    ASTCastable(ASTLocation &loc, ASTNode *casta_expr, ASTTypeSingle *casta_type) : ASTNode(loc), expr(casta_expr), type(casta_type) {}
    ~ASTCastable();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif
