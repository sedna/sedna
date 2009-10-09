/*
 * File:  ASTCastable.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_CASTABLE_H_
#define _AST_CASTABLE_H_

#include "ASTNode.h"
#include "AST.h"

#include "ASTTypeSingle.h"

class ASTCastable : public ASTNode
{
public:
    ASTNode *expr;
    ASTNode *type; // ASTTypeSingle

public:
    ASTCastable(const ASTNodeCommonData &loc, ASTNode *casta_expr, ASTNode *casta_type) : ASTNode(loc), expr(casta_expr), type(casta_type)
    {
    }
    ~ASTCastable();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif
