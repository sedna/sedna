/*
 * File:  ASTCast.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_CAST_H_
#define _AST_CAST_H_

#include "ASTNode.h"
#include "AST.h"

#include "ASTTypeSingle.h"

class ASTCast : public ASTNode
{
public:
    ASTNode *expr;
    ASTNode *type; // ASTTypeSingle

public:
    ASTCast(ASTLocation &loc, ASTNode *cast_expr, ASTNode *cast_type) : ASTNode(loc), expr(cast_expr), type(cast_type)
    {
    }

    ~ASTCast();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif
