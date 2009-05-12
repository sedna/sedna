/*
 * File:  ASTCommentConst.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_COMMENT_CONST_H_
#define _AST_COMMENT_CONST_H_

#include "ASTNode.h"
#include "AST.h"

class ASTCommentConst : public ASTNode
{
public:
    ASTNode *expr; // computed construction expression

public:
    ASTCommentConst(ASTLocation &loc, ASTNode *expr_) : ASTNode(loc), expr(expr_) {}

    ~ASTCommentConst();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif
